----------------------------------------------------------------------
--
-- LocalStorage.elm
-- Elm interface to JavaScript's localStorage facility
-- Copyright (c) 2018-2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module PortFunnel.LocalStorage exposing
    ( Message, Response(..), State, Key, Prefix, Value
    , moduleName, moduleDesc, commander
    , initialState
    , send
    , get, put, listKeys, clear
    , toString, toJsonString
    , makeSimulatedCmdPort
    , isLoaded, getPrefix, encode, decode
    , Label, getLabeled, listKeysLabeled
    )

{-| The `PortFunnelLocalStorage` uses the JavaScript `localStorage` facility to persistently store key/value pairs.

It is a `billstclair/elm-port-funnel` `PortFunnel` funnel.


# Types

@docs Message, Response, State, Key, Prefix, Value


# Components of a `PortFunnel.FunnelSpec`

@docs moduleName, moduleDesc, commander


# Initial `State`

@docs initialState


# Sending a `Message` out the `Cmd` Port

@docs send


# Creating Messages

The `Message` type is opaque, so there are functions to create the four messages
you may pass to `send`.

@docs get, put, listKeys, clear


# Conversion to Strings

@docs toString, toJsonString


# Simulator

@docs makeSimulatedCmdPort


# Non-standard Functions

@docs isLoaded, getPrefix, encode, decode

-}

import Dict exposing (Dict)
import Dict.Extra as DE
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import PortFunnel exposing (GenericMessage, ModuleDesc)
import PortFunnel.InternalTypes as IT exposing (Message(..))


{-| A convenience type for keys in the store. Same as `String`.
-}
type alias Key =
    IT.Key


{-| A convenience type for values in the store. Same as `Json.Encode.Value`.
-}
type alias Value =
    IT.Value


{-| A convenience type for prefixes. Same as `String`.
-}
type alias Prefix =
    IT.Prefix


{-| A convenience type for labels in the store. Same as `Maybe String`.
-}
type alias Label =
    IT.Label


{-| Our internal state.
-}
type State
    = State
        { isLoaded : Bool
        , prefix : Prefix
        , simulationDict : Dict Key Value
        }


{-| A `Response` is used to return values for `Get` and `ListKeys`.
-}
type Response
    = NoResponse
    | GetResponse { label : Label, key : Key, value : Maybe Value }
    | ListKeysResponse { label : Label, prefix : String, keys : List Key }


{-| An opaque type that represents a message to send to or receive from the JS code.

There are a number of internal messages, but the ones you can use are created by `get`, `put`, `listkeys`, and `clear`.

-}
type alias Message =
    IT.Message


{-| Return a `Message` to get a value from local storage.
-}
get : Key -> Message
get =
    Get Nothing


{-| Return a `Message` to get a labeled value from local storage.

Sometimes the `Key` alone isn't enough to mark your intention with
the return value. In this case you can label the return with a string
you can recognize. It will be returned in the `label` property of the
`GetResponse`.

-}
getLabeled : String -> Key -> Message
getLabeled label =
    Get (Just label)


{-| Return a `Message` to put a value into local storage.

`Nothing` for the value means to remove the key.

-}
put : Key -> Maybe Value -> Message
put =
    Put


{-| Return a `Message` to list all keys beginning with a prefix.
-}
listKeys : Prefix -> Message
listKeys =
    ListKeys Nothing


{-| Return a `Message` to list all keys beginning with a prefix.

Sometimes the `Prefix` alone isn't enough to mark your intention with
the return value. In this case you can label the return with a string
you can recognize. It will be returned in the `label` property of the
`ListKeysResponse`.

-}
listKeysLabeled : String -> Prefix -> Message
listKeysLabeled label =
    ListKeys (Just label)


{-| Return a message to remove all keys beginning with a prefix.

A prefix of `""` means to remove all keys.

-}
clear : Prefix -> Message
clear =
    Clear


{-| The initial state.

The `Prefix` arg (plus a period, if non-blank) is prepended to all
keys sent to the backend, and removed from keys returned. It basically
give you a namespace in local storage, and usually matches your
application name. It allows multiple different Elm applications to be
served by the same domain without stepping on each other's state.

The state also stores a `Dict`, which acts as the backing store for simulation,
and a flag saying whether the JavaScript code has sent its "I'm loaded" message
(see `isLoaded`).

-}
initialState : Prefix -> State
initialState prefix =
    State
        { isLoaded = False
        , prefix = prefix
        , simulationDict = Dict.empty
        }


{-| Get the prefix from the state.
-}
getPrefix : State -> Prefix
getPrefix (State state) =
    state.prefix


{-| The name of this funnel: "LocalStorage".
-}
moduleName : String
moduleName =
    "LocalStorage"


{-| Our module descriptor.
-}
moduleDesc : ModuleDesc Message State Response
moduleDesc =
    PortFunnel.makeModuleDesc moduleName encode decode process


{-| This is to prevent typos. Only one copy of each string means no typos possible.
-}
type Tag
    = StartupTag
    | GetTag
    | GotTag
    | PutTag
    | ListKeysTag
    | KeysTag
    | ClearTag
    | SimulateGetTag
    | SimulatePutTag
    | SimulateListKeysTag
    | SimulateClearTag
    | NOTAG


startupTag =
    "startup"


getTag =
    "get"


gotTag =
    "got"


putTag =
    "put"


listKeysTag =
    "listkeys"


keysTag =
    "keys"


clearTag =
    "clear"


simulateGetTag =
    "simulateget"


simulatePutTag =
    "simulateput"


simulateListKeysTag =
    "simulatelistkeys"


simulateClearTag =
    "simulateclear"


tagDict : Dict String Tag
tagDict =
    Dict.fromList
        [ ( startupTag, StartupTag )
        , ( getTag, GetTag )
        , ( gotTag, GotTag )
        , ( putTag, PutTag )
        , ( listKeysTag, ListKeysTag )
        , ( keysTag, KeysTag )
        , ( clearTag, ClearTag )
        , ( simulateGetTag, SimulateGetTag )
        , ( simulatePutTag, SimulatePutTag )
        , ( simulateListKeysTag, SimulateListKeysTag )
        , ( simulateClearTag, SimulateClearTag )
        ]


strtag : String -> Tag
strtag str =
    case Dict.get str tagDict of
        Just tag ->
            tag

        Nothing ->
            NOTAG


encodeLabeledString : Label -> String -> String -> Value
encodeLabeledString label string property =
    JE.object
        [ ( "label"
          , case label of
                Just lab ->
                    JE.string lab

                Nothing ->
                    JE.null
          )
        , ( property, JE.string string )
        ]


labeledStringDecoder : String -> Decoder ( Label, String )
labeledStringDecoder property =
    JD.map2 Tuple.pair
        (JD.field "label" <| JD.nullable JD.string)
        (JD.field property JD.string)


{-| Turn a `Message` into a `GenericMessage`.
-}
encode : Message -> GenericMessage
encode message =
    case message of
        Startup ->
            GenericMessage moduleName startupTag JE.null

        Get label key ->
            GenericMessage moduleName getTag <|
                encodeLabeledString label key "key"

        Got label key value ->
            GenericMessage moduleName gotTag <|
                JE.object
                    [ ( "label"
                      , case label of
                            Just lab ->
                                JE.string lab

                            Nothing ->
                                JE.null
                      )
                    , ( "key", JE.string key )
                    , ( "value"
                      , case value of
                            Nothing ->
                                JE.null

                            Just v ->
                                v
                      )
                    ]

        Put key value ->
            GenericMessage moduleName putTag <|
                JE.object
                    [ ( "key", JE.string key )
                    , ( "value"
                      , case value of
                            Nothing ->
                                JE.null

                            Just v ->
                                v
                      )
                    ]

        ListKeys label prefix ->
            GenericMessage moduleName listKeysTag <|
                encodeLabeledString label prefix "prefix"

        Keys label prefix keys ->
            GenericMessage moduleName keysTag <|
                JE.object
                    [ ( "label"
                      , case label of
                            Just lab ->
                                JE.string lab

                            Nothing ->
                                JE.null
                      )
                    , ( "prefix", JE.string prefix )
                    , ( "keys", JE.list JE.string keys )
                    ]

        Clear prefix ->
            GenericMessage moduleName clearTag <| JE.string prefix

        SimulateGet label key ->
            GenericMessage moduleName simulateGetTag <|
                encodeLabeledString label key "key"

        SimulatePut key value ->
            GenericMessage moduleName simulatePutTag <|
                JE.object
                    [ ( "key", JE.string key )
                    , ( "value"
                      , case value of
                            Nothing ->
                                JE.null

                            Just v ->
                                v
                      )
                    ]

        SimulateListKeys label prefix ->
            GenericMessage moduleName simulateListKeysTag <|
                encodeLabeledString label prefix "prefix"

        SimulateClear prefix ->
            GenericMessage moduleName simulateClearTag <|
                JE.string prefix


type alias GotRecord =
    { label : Label
    , key : Key
    , value : Value
    }


gotDecoder : Decoder GotRecord
gotDecoder =
    JD.map3 GotRecord
        (JD.field "label" <| JD.nullable JD.string)
        (JD.field "key" JD.string)
        (JD.field "value" JD.value)


type alias PutRecord =
    { key : Key
    , value : Value
    }


putDecoder : Decoder PutRecord
putDecoder =
    JD.map2 PutRecord
        (JD.field "key" JD.string)
        (JD.field "value" JD.value)


type alias KeysRecord =
    { label : Label
    , prefix : Key
    , keys : List Key
    }


keysDecoder : Decoder KeysRecord
keysDecoder =
    JD.map3 KeysRecord
        (JD.field "label" <| JD.nullable JD.string)
        (JD.field "prefix" JD.string)
        (JD.field "keys" <| JD.list JD.string)


{-| Turn a `GenericMessage` into a `Message`.
-}
decode : GenericMessage -> Result String Message
decode { tag, args } =
    case strtag tag of
        GetTag ->
            case JD.decodeValue (labeledStringDecoder "key") args of
                Ok ( label, key ) ->
                    Ok (Get label key)

                Err _ ->
                    Err <| "Get key not a string: " ++ JE.encode 0 args

        GotTag ->
            case JD.decodeValue gotDecoder args of
                Ok { label, key, value } ->
                    Ok
                        (Got label key <|
                            if value == JE.null then
                                Nothing

                            else
                                Just value
                        )

                Err _ ->
                    Err <| "Got not { label, key, value }: " ++ JE.encode 0 args

        PutTag ->
            case JD.decodeValue putDecoder args of
                Ok { key, value } ->
                    Ok
                        (Put key <|
                            if value == JE.null then
                                Nothing

                            else
                                Just value
                        )

                Err _ ->
                    Err <| "Put not { key, value }: " ++ JE.encode 0 args

        ListKeysTag ->
            case JD.decodeValue (labeledStringDecoder "prefix") args of
                Ok ( label, prefix ) ->
                    Ok (ListKeys label prefix)

                Err _ ->
                    Err <| "ListKeys prefix not a string: " ++ JE.encode 0 args

        KeysTag ->
            case JD.decodeValue keysDecoder args of
                Ok { label, prefix, keys } ->
                    Ok (Keys label prefix keys)

                Err _ ->
                    Err <| "Keys not { prefix, keys }: " ++ JE.encode 0 args

        ClearTag ->
            case JD.decodeValue JD.string args of
                Ok prefix ->
                    Ok (Clear prefix)

                Err _ ->
                    Err <| "Clear prefix not a string: " ++ JE.encode 0 args

        StartupTag ->
            Ok Startup

        SimulateGetTag ->
            case JD.decodeValue (labeledStringDecoder "key") args of
                Ok ( label, key ) ->
                    Ok (SimulateGet label key)

                Err _ ->
                    Err <| "Get key not a string: " ++ JE.encode 0 args

        SimulatePutTag ->
            case JD.decodeValue putDecoder args of
                Ok { key, value } ->
                    Ok
                        (SimulatePut key <|
                            if value == JE.null then
                                Nothing

                            else
                                Just value
                        )

                Err _ ->
                    Err <| "SimulatePut not { key, value }: " ++ JE.encode 0 args

        SimulateListKeysTag ->
            case JD.decodeValue (labeledStringDecoder "prefix") args of
                Ok ( label, prefix ) ->
                    Ok (SimulateListKeys label prefix)

                Err _ ->
                    Err <|
                        "SimulateListKeys prefix not a string: "
                            ++ JE.encode 0 args

        SimulateClearTag ->
            case JD.decodeValue JD.string args of
                Ok prefix ->
                    Ok (SimulateClear prefix)

                Err _ ->
                    Err <|
                        "SimulateClear prefix not a string: "
                            ++ JE.encode 0 args

        _ ->
            Err <| "Unknown tag: " ++ tag


{-| Send a `Message` through a `Cmd` port.

Note that this `send` function requires that you pass state,
but it is read-only, so you don't need to update your `Model` state
on return.

-}
send : (Value -> Cmd msg) -> Message -> State -> Cmd msg
send wrapper message (State state) =
    let
        prefix =
            state.prefix

        mess =
            case message of
                Get label key ->
                    Get label (addPrefix prefix key)

                Put key value ->
                    Put (addPrefix prefix key) value

                ListKeys label pref ->
                    ListKeys label (addPrefix prefix pref)

                Clear pref ->
                    Clear (addPrefix prefix pref)

                _ ->
                    message
    in
    PortFunnel.sendMessage moduleDesc wrapper mess


process : Message -> State -> ( State, Response )
process message ((State state) as boxedState) =
    case message of
        Got label key value ->
            ( boxedState
            , GetResponse
                { label = label
                , key = stripPrefix state.prefix key
                , value = value
                }
            )

        Keys label prefix keys ->
            ( boxedState
            , ListKeysResponse
                { label = label
                , prefix = stripPrefix state.prefix prefix
                , keys = List.map (stripPrefix state.prefix) keys
                }
            )

        Startup ->
            ( State { state | isLoaded = True }
            , NoResponse
            )

        SimulateGet label key ->
            ( boxedState
            , GetResponse
                { label = label
                , key = stripPrefix state.prefix key
                , value = Dict.get key state.simulationDict
                }
            )

        SimulatePut key value ->
            ( State
                { state
                    | simulationDict =
                        case value of
                            Nothing ->
                                Dict.remove key state.simulationDict

                            Just v ->
                                Dict.insert key v state.simulationDict
                }
            , NoResponse
            )

        SimulateListKeys label prefix ->
            ( boxedState
            , ListKeysResponse
                { label = label
                , prefix = stripPrefix state.prefix prefix
                , keys =
                    Dict.foldr
                        (\k _ res ->
                            if String.startsWith prefix k then
                                stripPrefix state.prefix k :: res

                            else
                                res
                        )
                        []
                        state.simulationDict
                }
            )

        SimulateClear prefix ->
            ( State
                { state
                    | simulationDict =
                        DE.removeWhen (\k _ -> String.startsWith prefix k)
                            state.simulationDict
                }
            , NoResponse
            )

        _ ->
            ( State state
            , NoResponse
            )


{-| Responsible for sending a `CmdResponse` back through the port.

This funnel doesn't initiate any sends, so this function always returns `Cmd.none`.

-}
commander : (GenericMessage -> Cmd msg) -> Response -> Cmd msg
commander _ _ =
    Cmd.none


simulator : Message -> Maybe Message
simulator message =
    case message of
        Get label key ->
            Just <| SimulateGet label key

        Put key value ->
            Just <| SimulatePut key value

        ListKeys label prefix ->
            Just <| SimulateListKeys label prefix

        Clear prefix ->
            Just <| SimulateClear prefix

        _ ->
            Nothing


{-| Make a simulated `Cmd` port.
-}
makeSimulatedCmdPort : (Value -> msg) -> Value -> Cmd msg
makeSimulatedCmdPort =
    PortFunnel.makeSimulatedFunnelCmdPort
        moduleDesc
        simulator


labelToString : Label -> String
labelToString string =
    case string of
        Nothing ->
            "Nothing"

        Just s ->
            "(Just \"" ++ s ++ "\")"


{-| Convert a `Message` to a nice-looking human-readable string.
-}
toString : Message -> String
toString message =
    case message of
        Startup ->
            "<Startup>"

        Get label key ->
            "<Get " ++ labelToString label ++ " \"" ++ key ++ "\">"

        Got label key value ->
            "<Get "
                ++ labelToString label
                ++ " \""
                ++ key
                ++ "\" "
                ++ (case value of
                        Nothing ->
                            "null"

                        Just v ->
                            JE.encode 0 v
                   )
                ++ ">"

        Put key value ->
            "<Put \""
                ++ key
                ++ "\" "
                ++ (case value of
                        Nothing ->
                            "null"

                        Just v ->
                            JE.encode 0 v
                   )
                ++ ">"

        ListKeys label prefix ->
            "<ListKeys " ++ labelToString label ++ " \"" ++ prefix ++ "\">"

        Keys label prefix keys ->
            "<Keys "
                ++ labelToString label
                ++ " \""
                ++ prefix
                ++ "\" ["
                ++ (List.map (\s -> "\"" ++ s ++ "\"") keys
                        |> List.intersperse ", "
                        |> String.concat
                   )
                ++ "]>"

        Clear prefix ->
            "<Clear \"" ++ prefix ++ "\""

        SimulateGet label key ->
            "<SimulateGet " ++ labelToString label ++ " \"" ++ key ++ "\""

        SimulatePut key value ->
            "<SimulatePut \""
                ++ key
                ++ "\" "
                ++ (case value of
                        Nothing ->
                            "null"

                        Just v ->
                            JE.encode 0 v
                   )
                ++ ">"

        SimulateListKeys label prefix ->
            "<SimulateListKeys " ++ labelToString label ++ " \"" ++ prefix ++ "\">"

        SimulateClear prefix ->
            "<SimulateClear \"" ++ prefix ++ "\">"


{-| Convert a `Message` to the same JSON string that gets sent

over the wire to the JS code.

-}
toJsonString : Message -> String
toJsonString message =
    message
        |> encode
        |> PortFunnel.encodeGenericMessage
        |> JE.encode 0


{-| Prepend the `String` and a period to the `Key`, or nothing if the `String` is empty.
-}
addPrefix : String -> Key -> Key
addPrefix prefix key =
    if prefix == "" then
        key

    else
        prefix ++ "." ++ key


{-| Drop the length of the first arg from the left of the second.
-}
stripPrefix : String -> Key -> Key
stripPrefix prefix key =
    if prefix == "" then
        key

    else
        String.dropLeft (1 + String.length prefix) key


{-| Returns true if a `Startup` message has been processed.

This is sent by the port code after it has initialized. Your code can use this to decide whether to use your real outgoing port or the one created by `makeSimulatedCmdPort`.

-}
isLoaded : State -> Bool
isLoaded (State state) =
    state.isLoaded
