----------------------------------------------------------------------
--
-- Sequence.elm
-- Sequencing series of LocalStorage operations using state machines.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
-- Began life in githhub.com/billstclair/zapmeme
-- History: https://github.com/billstclair/zapmeme/commits/d5b3fd1f990589fc064c64ce58afdfe8a95d8a0e/src/PortFunnel/LocalStorage/Sequence.elm
--
-- Removed: https://github.com/billstclair/zapmeme/commit/fa2de5da67813b180f932d167ac2df479177e3a1
--
----------------------------------------------------------------------


module PortFunnel.LocalStorage.Sequence exposing
    ( Wrappers, LocalStorageStates, State
    , KeyPair, DbRequest(..), DbResponse(..)
    , makeLocalStorageStates, makeNullState
    , update, processStates, process
    , send, inject, injectTask
    , getState, getFullState, setState, setStateOnly
    , decodeExpectedDbGot, dbResponseToValue, decodePair, encodePair
    )

{-| Make it easier to create complex state machines from `LocalStorage` contents.

This is too complicated for an example here, but see [ZapMeme.Sequencer](https://github.com/billstclair/zapmeme/blob/master/src/ZapMeme/Sequencer.elm) for a real-life example.

This does not support simulated `LocalStorage`. Certainly possible, but it was already plenty complicated without that.

Since an instance of `LocalStorageStates` is stored in your `Model`, and it contains two functions which take a `model` as argument, there needs to be a custom class to prevent infinite type recursion:

    type WrappedModel =
        WrappedModel Model

    type alias Model =
        { localStorageStates : LocalStorageStates WrappedModel Msg
        , ...
        }

    type Msg =
        SequenceDone (WrappedModel -> ( WrappedModel, Cmd Msg ))
        | ProcessLocalStorage Value
        ...


# Types

@docs Wrappers, LocalStorageStates, State
@docs KeyPair, DbRequest, DbResponse


# Constructors

@docs makeLocalStorageStates, makeNullState


# Processing incoming messages

You will usually use only `update`, but the others let you do lower-level processing yourself.

@docs update, processStates, process


# LocalStorage interaction.

@docs send, inject, injectTask


# State accessors

@docs getState, getFullState, setState, setStateOnly


# Utilities

@docs decodeExpectedDbGot, dbResponseToValue, decodePair, encodePair

-}

import AssocList exposing (Dict)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import PortFunnel
import PortFunnel.LocalStorage as LocalStorage
import Task exposing (Task)


{-| These are converted to and from the strings sent as LocalStorage keys by `encodePair` and `decodePair`.
-}
type alias KeyPair =
    { prefix : String
    , subkey : String
    }


{-| A high-level representation of a LocalStorage operation.

`DbNothing` does nothing.

`DbCustomRequest` allows you to send some command other than a message out the LocalStorage `cmdPort`.

`DbBatch` allows you to gang more than one request.

The others have obvious correlation to the LocalStorage messages, but `DbPut` and `DbRemove` are decoupled, instead of using a `Maybe Value`.

-}
type DbRequest msg
    = DbNothing
    | DbGet KeyPair
    | DbPut KeyPair Value
    | DbRemove KeyPair
    | DbListKeys KeyPair
    | DbClear KeyPair
    | DbCustomRequest (Cmd msg)
    | DbBatch (List (DbRequest msg))


{-| A high-level represenation of a message returned from LocalStorage through the subscription port.

Since this module doesn't support simulated storage, only `DbGot` in response to `DbGet` and `DbKeys` in response to `DbListKeys` need to be distinguished.

The other messages are all turned into `DbNoResponse`, but there wont be any in a non-simulated environment.

-}
type DbResponse
    = DbNoResponse
    | DbGot KeyPair (Maybe Value)
    | DbKeys KeyPair (List KeyPair)


{-| Holds the state, label, and LocalStorage subscription port processing function for one state machine.

You will get very confused if you use the same label for two different `State` values passed to `makeLocalStorageStates`.

See the definition of `initialStorageStates` in [ZapMeme.Sequencer](https://github.com/billstclair/zapmeme/blob/master/src/ZapMeme/Sequencer.elm).

-}
type alias State key state model msg =
    { state : state
    , label : String
    , process : Wrappers key state model msg -> DbResponse -> state -> ( DbRequest msg, state )
    }


type NullState key state model msg
    = NullState (State key state model msg)


dummyProcess : Wrappers key state model msg -> DbResponse -> state -> ( DbRequest msg, state )
dummyProcess wrappers response state =
    ( DbNothing, state )


nullLabel : String
nullLabel =
    "If somebody uses this lable they'll get what they deserve!!"


isNullState : State key state model msg -> Bool
isNullState state =
    nullLabel == state.label


{-| This is how you create a `NullState` for your `Wrappers`.

The `state` you pass will never be used for anything.

-}
makeNullState : state -> NullState key state model msg
makeNullState state =
    NullState
        { state = state
        , label = nullLabel
        , process = dummyProcess
        }


{-| Process a response for a single `State`.

If the response's `label` matches the `State`'s, calls `state.process`, updates the user `state` inside the `State`, and turns the returned `DbRequest` into a `Cmd msg` (usually by calling `send`). Otherwise, returns `Nothing`.

You will usually call this indirectly, via `update`.

-}
process : Wrappers key state model msg -> LocalStorage.Response -> State key state model msg -> Maybe ( State key state model msg, Cmd msg )
process wrappers response state =
    let
        ( label, responseThunk ) =
            responseToDbResponse response
    in
    if label /= state.label then
        Nothing

    else
        let
            dbResponse =
                responseThunk ()

            ( request, state2 ) =
                state.process wrappers dbResponse state.state
        in
        Just
            ( { state | state = state2 }
            , case request of
                DbCustomRequest cmd ->
                    cmd

                DbBatch requests ->
                    List.map (send wrappers state) (flattenBatchList requests)
                        |> Cmd.batch

                _ ->
                    send wrappers state request
            )


flattenBatchList : List (DbRequest msg) -> List (DbRequest msg)
flattenBatchList requests =
    let
        loop tail res =
            case tail of
                [] ->
                    List.reverse res

                req :: rest ->
                    case req of
                        DbBatch reqs ->
                            loop rest <|
                                List.concat
                                    [ List.reverse <| flattenBatchList reqs
                                    , res
                                    ]

                        _ ->
                            loop rest <| req :: res
    in
    loop requests []


{-| Turn a `DbRequest` into a `Cmd msg`

that sends a `LocalStorage.Message` out to the JavaScript through the `Cmd` port.

Uses the `sender` in `Wrappers` and `label` in `State`.

-}
send : Wrappers key state model msg -> State key state model msg -> DbRequest msg -> Cmd msg
send wrappers state request =
    if isNullState state then
        Cmd.none

    else
        case requestToMessage state.label request of
            Just req ->
                wrappers.sender req

            Nothing ->
                Cmd.none


{-| Eliminate error checking boilerplate inside `State` `process` functions.

    decodeExpectedDbGot decoder expectedSubkey response

If `response` is not a `DbGot`, returns Nothing.

If `expectedSubkey` is not `""`, and also not the `subkey` of that `DbGot`'s `KeyPair`, returns `Just (KeyPair, Nothing)`.

If the `DbGot`'s value is `Nothing`, returns `Just (KeyPair, Nothing)`.

Otherwise, uses `decoder` to decode that value. If the result is an `Err`, returns `Just (KeyPair, Nothing)`.

If all goes well, returns `Just (KeyPair, value)`.

-}
decodeExpectedDbGot : Decoder value -> String -> DbResponse -> Maybe ( KeyPair, Maybe value )
decodeExpectedDbGot decoder expectedSubkey response =
    case response of
        DbGot pair value ->
            if
                (expectedSubkey /= "")
                    && (pair.subkey /= expectedSubkey)
            then
                Just ( pair, Nothing )

            else
                case value of
                    Nothing ->
                        Just ( pair, Nothing )

                    Just v ->
                        case JD.decodeValue decoder v of
                            Err _ ->
                                Just ( pair, Nothing )

                            Ok result ->
                                Just ( pair, Just result )

        _ ->
            Nothing


responseToDbResponse : LocalStorage.Response -> ( String, () -> DbResponse )
responseToDbResponse response =
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            ( Maybe.withDefault "" label
            , \() -> DbGot (decodePair key) value
            )

        LocalStorage.ListKeysResponse { label, prefix, keys } ->
            ( Maybe.withDefault "" label
            , \() -> DbKeys (decodePair prefix) <| List.map decodePair keys
            )

        _ ->
            ( "", \() -> DbNoResponse )


requestToMessage : String -> DbRequest msg -> Maybe LocalStorage.Message
requestToMessage label request =
    case request of
        DbNothing ->
            Nothing

        DbGet pair ->
            Just <| LocalStorage.getLabeled label (encodePair pair)

        DbPut pair value ->
            Just <| LocalStorage.put (encodePair pair) (Just value)

        DbRemove pair ->
            Just <| LocalStorage.put (encodePair pair) Nothing

        DbListKeys pair ->
            Just <| LocalStorage.listKeysLabeled label (encodePair pair)

        DbClear pair ->
            Just <| LocalStorage.clear label

        _ ->
            Nothing


{-| Turn a `KeyPair` into a string.

That string is used as the `Key` arg to `LocalStorage.getLabeled` or the `Prefix` arg to `LocalStorage.listKeysLabeled`.

-}
encodePair : KeyPair -> String
encodePair { prefix, subkey } =
    if prefix == "" then
        subkey

    else
        let
            suffix =
                if subkey == "" then
                    ""

                else if subkey == "." then
                    "."

                else
                    "." ++ subkey
        in
        prefix ++ suffix


{-| Turn a string that comes back from the subscription port into a `KeyPair`.

Splits it on the first ".".

-}
decodePair : String -> KeyPair
decodePair key =
    case String.split "." key of
        [] ->
            KeyPair "" ""

        [ prefix ] ->
            KeyPair prefix ""

        [ prefix, "" ] ->
            KeyPair prefix "."

        prefix :: tail ->
            KeyPair prefix (String.join "." tail)


{-| Turn a DbResponse into the Value that would create it,

if received from the LocalStorage port code, in response to a `DbGet` or `DbListKeys` request.

`NoResponse` is treated as `DbGot` with empty string components in its `KeyPair` and `Nothing` as its value. You will likely never want to do that.

-}
dbResponseToValue : String -> String -> DbResponse -> Value
dbResponseToValue prefix label response =
    let
        wrap tag args =
            { moduleName = LocalStorage.moduleName
            , tag = tag
            , args = args
            }
                |> PortFunnel.encodeGenericMessage

        prefixed key =
            encodePair <| KeyPair prefix key
    in
    case response of
        DbGot pair value ->
            wrap "got" <|
                JE.object <|
                    [ ( "label", JE.string label )
                    , ( "key", JE.string (prefixed <| encodePair pair) )
                    , ( "value"
                      , case value of
                            Nothing ->
                                JE.null

                            Just v ->
                                v
                      )
                    ]

        DbKeys pair keys ->
            wrap "keys" <|
                JE.object
                    [ ( "label", JE.string label )
                    , ( "prefix", JE.string (prefixed <| encodePair pair) )
                    , ( "keys"
                      , List.map encodePair keys
                            |> JE.list JE.string
                      )
                    ]

        DbNoResponse ->
            dbResponseToValue prefix label <| DbGot (KeyPair "" "") Nothing


{-| If you receive something outside of a LocalStorage return, and want to get it back into your state machine, use this.

It returns a `Task` that, if you send it with your LocalStorage `sub` port message, will make it seem as if the given DbResponse was received from LocalStorage.

It's a trivial task wrapper on the result of dbResponseToValue, using `label` property of the `State`.

-}
injectTask : Wrappers key state model msg -> State key state model msg -> DbResponse -> Task Never Value
injectTask wrappers state response =
    dbResponseToValue wrappers.injector.prefix state.label response
        |> Task.succeed


{-| Call `injectTask`, and use `Task.perform` to turn that `Task` into a `Cmd`.

The `(Value -> msg)` function will usually be the `Msg` that receives subscription input from your LocalStorage port.

-}
inject : Wrappers key state model msg -> State key state model msg -> DbResponse -> Cmd msg
inject wrappers state response =
    injectTask wrappers state response
        |> Task.perform wrappers.injector.tagger



---
--- Support for tables of state machines.
---


{-| Used by `inject` and `injectTask` to create a message that appears to

have come through the subscription port from the JavaScript code.

-}
type alias Injector msg =
    { prefix : String
    , tagger : Value -> msg
    }


{-| Communication between `Sequence` and your main `Model` and `Msg`.

`sender` is how you send messages out your command port. It's usually defined in your main program, with something like:

    {-| The `model` parameter is necessary here for `PortFunnels.makeFunnelDict`.
    -}
    getCmdPort : String -> model -> (Value -> Cmd Msg)
    getCmdPort moduleName _ =
        PortFunnels.getCmdPort ProcessLocalStorage moduleName False

    localStorageSend : LocalStorage.Message -> Cmd Msg
    localStorageSend message =
        LocalStorage.send (getCmdPort LocalStorage.moduleName ())
            message
            funnelState.storage

`localStorageStates` and `setLocalStorageStates` are functions that read/write a `LocalStorageStates` from/to your `Model`.

`sequenceDone` creates a `Msg` that calls its arg with the Model, and expects back the standard `update` result. E.g.:

    type Msg
       = SequenceDone (WrappedModel -> (WrappedModel, Cmd Msg))
       ...

    update model msg =
       SequenceDone updater ->
            let
                ( WrappedModel mdl, cmd ) =
                    updater (WrappedModel model)
            in
            ( mdl, cmd )
       ...

Make a `NullState` with `makeNullState`.

-}
type alias Wrappers key state model msg =
    { sender : LocalStorage.Message -> Cmd msg
    , injector : Injector msg
    , localStorageStates : model -> LocalStorageStates key state model msg
    , setLocalStorageStates : LocalStorageStates key state model msg -> model -> model
    , sequenceDone : (model -> ( model, Cmd msg )) -> msg
    , nullState : NullState key state model msg
    }


{-| Opaque type used to store a table of state machines.

Create one with `makeLocalStorageStates`. Store it in your `Model`.

-}
type LocalStorageStates key state model msg
    = LocalStorageStates
        { wrappers : Wrappers key state model msg
        , table : Dict key (State key state model msg)
        }


{-| Create a `LocalStorageStates` to store in your `Model`.

The `key` type is usually a simple enumerator custom type. You could use strings, but then the compiler won't catch typos for you.

The `state` type is usually a custom type with one tag per `key` value.

the `model` here is usually `WrappedModel` in your code, to prevent type recursion through `wrappers.localStorageStates` and `wrappers.setLocalStorageStates`.

-}
makeLocalStorageStates : Wrappers key state model msg -> List ( key, State key state model msg ) -> LocalStorageStates key state model msg
makeLocalStorageStates wrappers states =
    LocalStorageStates
        { wrappers = wrappers
        , table = AssocList.fromList states
        }


getWrappers : LocalStorageStates key state model msg -> Wrappers key state model msg
getWrappers (LocalStorageStates states) =
    states.wrappers


{-| Look up the `State` associated with `key` in `model`'s `LocalStorageStates`
-}
getFullState : Wrappers key state model msg -> key -> model -> Maybe (State key state model msg)
getFullState wrappers key model =
    let
        (LocalStorageStates states) =
            wrappers.localStorageStates model
    in
    AssocList.get key states.table


{-| Look up the `State` associated with `key` in `model`'s `LocalStorageStates`

(with `getFullState`). If found, return its`state` field.

-}
getState : Wrappers key state model msg -> key -> model -> Maybe state
getState wrappers key model =
    case getFullState wrappers key model of
        Nothing ->
            Nothing

        Just state ->
            Just state.state


nullState : Wrappers key state model msg -> State key state model msg
nullState wrappers =
    let
        (NullState state) =
            wrappers.nullState
    in
    state


{-| Update one `State` inside the `LocalStorageStates` inside of the `model`.

Return the updated `model`.

Use `setState` if you also need the updated `State` (or call `getFullState`).

-}
setStateOnly : Wrappers key state model msg -> key -> state -> model -> model
setStateOnly wrappers key stateState model =
    let
        ( res, _ ) =
            setState wrappers key stateState model
    in
    res


{-| Update one `State` inside the `LocalStorageStates` inside of the `model`.

Returns the updated `model` and `State`. This version is usually used when you start up a state machine, since you need the `State` to call `send`.

Use `setStateOnly` if you need only the updated `model`.

-}
setState : Wrappers key state model msg -> key -> state -> model -> ( model, State key state model msg )
setState wrappers key stateState model =
    let
        (LocalStorageStates states) =
            wrappers.localStorageStates model
    in
    case AssocList.get key states.table of
        Nothing ->
            ( model, nullState wrappers )

        Just state ->
            ( wrappers.setLocalStorageStates
                (LocalStorageStates
                    { states
                        | table =
                            AssocList.insert key
                                { state | state = stateState }
                                states.table
                    }
                )
                model
            , state
            )


{-| Calls `process` on each of the `State`s registered in the `LocalStorageStates`.

Accumulates the results, but in practice, only one of them will do anything other than note that the `LocalStorage.Response`'s `label` doesn't match, and return the `model` unchanged with `Cmd.none`.

You will usually call this via `update`, not directly.

-}
processStates : LocalStorage.Response -> LocalStorageStates key state model msg -> ( LocalStorageStates key state model msg, Cmd msg )
processStates response (LocalStorageStates states) =
    let
        loop key state ( states3, cmd2 ) =
            case process states.wrappers response state of
                Nothing ->
                    ( states3, cmd2 )

                Just ( state2, cmd3 ) ->
                    ( { states3
                        | table =
                            AssocList.insert key state2 states3.table
                      }
                    , Cmd.batch [ cmd3, cmd2 ]
                    )

        ( states2, cmd ) =
            AssocList.foldl loop ( states, Cmd.none ) states.table
    in
    ( LocalStorageStates states2, cmd )


{-| Modulo wrapping and unwrapping, designed to be the entire handler for incoming LocalStorage responses.

    funnelDict : FunnelDict Model Msg
    funnelDict =
        PortFunnels.makeFunnelDict
            [ LocalStorageHandler wrappedStorageHandler ]
            getCmdPort

    wrappedStorageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
    wrappedStorageHandler response _ model =
        let
            ( WrappedModel mdl, cmd ) =
                Sequence.storageHandler
                    sequenceWrappers
                    (WrappedModel model)
                    response
        in
        ( mdl, cmd )

-}
update : Wrappers key state model msg -> model -> LocalStorage.Response -> ( model, Cmd msg )
update wrappers model response =
    let
        ( states2, cmd ) =
            processStates response (wrappers.localStorageStates model)
    in
    ( wrappers.setLocalStorageStates states2 model
    , cmd
    )
