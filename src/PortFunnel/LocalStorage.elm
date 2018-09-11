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
    , isLoaded, getPrefix
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

@docs isLoaded, getPrefix

-}

import Dict exposing (Dict)
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
    | GetResponse { key : Key, value : Value }
    | ListKeysResponse { prefix : String, keys : List Key }


{-| Messages that can be sent to the JavaScript code.
-}
type Message
    = Startup
    | Get Key
    | Put Key Value
    | ListKeys Prefix
    | Keys Prefix (List Key)
    | Clear Prefix


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


encode : Message -> GenericMessage
encode message =
    case message of
        Startup ->
            GenericMessage moduleName "startup" JE.null

        Get key ->
            GenericMessage moduleName "get" <| JE.string key

        Put key value ->
            GenericMessage moduleName "put" <|
                JE.object
                    [ ( "key", JE.string key )
                    , ( "value", value )
                    ]

        ListKeys prefix ->
            GenericMessage moduleName "listkeys" <| JE.string prefix

        Keys prefix keys ->
            GenericMessage moduleName "keys" <|
                JE.object
                    [ ( "prefix", JE.string prefix )
                    , ( "keys", JE.list JE.string keys )
                    ]

        Clear prefix ->
            GenericMessage moduleName "clear" <| JE.string prefix


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
        (JD.field "value" <| JD.list JD.string)


decode : GenericMessage -> Result String Message
decode { tag, args } =
    case tag of
        "startup" ->
            Ok Startup

        "get" ->
            case JD.decodeValue JD.string args of
                Ok key ->
                    Ok (Get key)

                Err _ ->
                    Err <| "Get key not a string: " ++ JE.encode 0 args

        "put" ->
            case JD.decodeValue putDecoder args of
                Ok { key, value } ->
                    Ok (Put key value)

                Err _ ->
                    Err <| "Put not { key, value }: " ++ JE.encode 0 args

        "listkeys" ->
            case JD.decodeValue JD.string args of
                Ok prefix ->
                    Ok (ListKeys prefix)

                Err _ ->
                    Err <| "ListKeys prefix not a string: " ++ JE.encode 0 args

        "keys" ->
            case JD.decodeValue keysDecoder args of
                Ok { prefix, keys } ->
                    Ok (Keys prefix keys)

                Err _ ->
                    Err <| "Keys not { prefix, keys }: " ++ JE.encode 0 args

        "clear" ->
            case JD.decodeValue JD.string args of
                Ok prefix ->
                    Ok (Clear prefix)

                Err _ ->
                    Err <| "Clear prefix not a string: " ++ JE.encode 0 args

        _ ->
            Err <| "Unknown tag: " ++ tag


{-| Send a `Message` through a `Cmd` port.
-}
send : (Value -> Cmd msg) -> Message -> Cmd msg
send =
    PortFunnel.sendMessage moduleDesc


process : Message -> State -> ( State, Response )
process message (State state) =
    case message of
        Put key value ->
            ( State state
            , GetResponse { key = key, value = value }
            )

        Keys prefix keys ->
            ( State state
            , ListKeysResponse { prefix = prefix, keys = keys }
            )

        Startup ->
            ( State { state | isLoaded = True }
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
    -- TODO
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
    "TODO"


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


{-| Returns true if a `Startup` message has been processed.

This is sent by the port code after it has initialized.

-}
isLoaded : State -> Bool
isLoaded (State state) =
    state.isLoaded
