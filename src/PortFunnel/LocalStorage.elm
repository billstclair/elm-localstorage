----------------------------------------------------------------------
--
-- LocalStorage.elm
-- Elm interface to JavaScript's localStorage facility
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module PortFunnel.LocalStorage exposing
    ( Message(..), Response(..), State
    , moduleName, moduleDesc, commander
    , initialState
    , send
    , toString, toJsonString
    , makeSimulatedCmdPort
    , isLoaded, getPrefix, encode, decode
    )

{-| The `PortFunnelLocalStorage` uses the JavaScript `localStorage` facility to persistently store key/value pairs.

It is a `billstclair/elm-port-funnel` `PortFunnel` funnel.


# Types

@docs Message, Response, State


# Components of a `PortFunnel.FunnelSpec`

@docs moduleName, moduleDesc, commander


# Initial `State`

@docs initialState


# Sending a `Message` out the `Cmd` Port

@docs send


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


{-| A convenience type for keys in the store. Same as `String`.
-}
type alias Key =
    String


{-| A convenience type for values in the store. Same as `Json.Encode.Value`.
-}
type alias Value =
    JE.Value


{-| A convenience type for prefixes. Same as `String`.
-}
type alias Prefix =
    String


{-| Our internal state.
-}
type State
    = State
        { isLoaded : Bool
        , prefix : Prefix
        , simulationDict : Dict Key Value
        }


{-| A `MessageResponse` is used to return values for `Get` and `ListKeys`.
-}
type Response
    = NoResponse
    | GetResponse { key : Key, value : Maybe Value }
    | ListKeysResponse { prefix : String, keys : List Key }


{-| Messages that can be sent/received to/from the JavaScript code.

`Startup` is received after the JS code loads. It sets the `isLoaded` flag in our state, and reports `NoResponse`.

`Get` is sent to request a read.

`Put` is sent to request a write or received in response to `Get`. A value of `Nothing` means remove that key/value pair when sent or no value in the store when received.

`ListKeys` is sent to request a list of keys.

`Keys` is received in response to `ListKeys`.

`Clear` is sent to clear keys starting with the prefix. No response is sent.

The `Simulate<Foo>` messages are returned by the simulator and simulated by the `process` function (not exposed, except inside of `moduleDesc`).

-}
type Message
    = Startup
    | Get Key
    | Put Key (Maybe Value)
    | ListKeys Prefix
    | Keys Prefix (List Key)
    | Clear Prefix
    | SimulateGet Key
    | SimulatePut Key (Maybe Value)
    | SimulateListKeys Prefix
    | SimulateClear Prefix


{-| The initial state.
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


{-| Turn a `Message` into a `GenericMessage`.
-}
encode : Message -> GenericMessage
encode message =
    case message of
        Startup ->
            GenericMessage moduleName startupTag JE.null

        Get key ->
            GenericMessage moduleName getTag <| JE.string key

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

        ListKeys prefix ->
            GenericMessage moduleName listKeysTag <| JE.string prefix

        Keys prefix keys ->
            GenericMessage moduleName keysTag <|
                JE.object
                    [ ( "prefix", JE.string prefix )
                    , ( "keys", JE.list JE.string keys )
                    ]

        Clear prefix ->
            GenericMessage moduleName clearTag <| JE.string prefix

        SimulateGet key ->
            GenericMessage moduleName simulateGetTag <| JE.string key

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

        SimulateListKeys prefix ->
            GenericMessage moduleName simulateListKeysTag <|
                JE.string prefix

        SimulateClear prefix ->
            GenericMessage moduleName simulateClearTag <|
                JE.string prefix


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
    { prefix : Key
    , keys : List Key
    }


keysDecoder : Decoder KeysRecord
keysDecoder =
    JD.map2 KeysRecord
        (JD.field "prefix" JD.string)
        (JD.field "keys" <| JD.list JD.string)


{-| Turn a `GenericMessage` into a `Message`.
-}
decode : GenericMessage -> Result String Message
decode { tag, args } =
    case strtag tag of
        GetTag ->
            case JD.decodeValue JD.string args of
                Ok key ->
                    Ok (Get key)

                Err _ ->
                    Err <| "Get key not a string: " ++ JE.encode 0 args

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
            case JD.decodeValue JD.string args of
                Ok prefix ->
                    Ok (ListKeys prefix)

                Err _ ->
                    Err <| "ListKeys prefix not a string: " ++ JE.encode 0 args

        KeysTag ->
            case JD.decodeValue keysDecoder args of
                Ok { prefix, keys } ->
                    Ok (Keys prefix keys)

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
            case JD.decodeValue JD.string args of
                Ok key ->
                    Ok (SimulateGet key)

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
            case JD.decodeValue JD.string args of
                Ok prefix ->
                    Ok (SimulateListKeys prefix)

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
                Get key ->
                    Get (addPrefix prefix key)

                Put key value ->
                    Put (addPrefix prefix key) value

                ListKeys pref ->
                    ListKeys (addPrefix prefix pref)

                Clear pref ->
                    Clear (addPrefix prefix pref)

                _ ->
                    message
    in
    PortFunnel.sendMessage moduleDesc wrapper mess


process : Message -> State -> ( State, Response )
process message ((State state) as boxedState) =
    let
        prefix =
            state.prefix
    in
    case message of
        Put key value ->
            ( boxedState
            , GetResponse { key = key, value = value }
            )

        Keys prefix keys ->
            ( boxedState
            , ListKeysResponse
                { prefix = prefix
                , keys = List.map (stripPrefix prefix) keys
                }
            )

        Startup ->
            ( State { state | isLoaded = True }
            , NoResponse
            )

        SimulateGet key ->
            ( boxedState
            , GetResponse
                { key = key
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

        SimulateListKeys prefix ->
            ( boxedState
            , ListKeysResponse
                { prefix = prefix
                , keys =
                    Dict.foldr
                        (\k _ res ->
                            if String.startsWith k prefix then
                                k :: res

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
        Get key ->
            Just <| SimulateGet key

        Put key value ->
            Just <| SimulatePut key value

        ListKeys prefix ->
            Just <| SimulateListKeys prefix

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


{-| Convert a `Message` to a nice-looking human-readable string.
-}
toString : Message -> String
toString message =
    case message of
        Startup ->
            "<Startup>"

        Get key ->
            "<Get \"" ++ key ++ "\">"

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

        ListKeys prefix ->
            "<ListKeys \"" ++ prefix ++ "\">"

        Keys prefix keys ->
            "<Keys \""
                ++ prefix
                ++ "\" ["
                ++ (List.map (\s -> "\"" ++ s ++ "\"") keys
                        |> List.intersperse ", "
                        |> String.concat
                   )
                ++ "]>"

        Clear prefix ->
            "<Clear \"" ++ prefix ++ "\""

        SimulateGet key ->
            "<SimulateGet \"" ++ key ++ "\""

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

        SimulateListKeys prefix ->
            "<SimulateListKeys \"" ++ prefix ++ "\">"

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


stripPrefix : String -> Key -> Key
stripPrefix prefix key =
    if prefix == "" then
        key

    else
        String.dropLeft (1 + String.length prefix) key


{-| Returns true if a `Startup` message has been processed.

This is sent by the port code after it has initialized.

-}
isLoaded : State -> Bool
isLoaded (State state) =
    state.isLoaded
