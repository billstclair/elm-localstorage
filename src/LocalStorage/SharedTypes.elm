----------------------------------------------------------------------
--
-- SharedTypes.elm
-- Shared types for LocalStorage module.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module LocalStorage.SharedTypes exposing
    ( Key, Value, Operation(..), Ports(..), DictState, MsgWrapper
    , GetItemPort, SetItemPort, ClearPort, ListKeysPort, ReceiveItemPort
    , receiveWrapper, decodeStringList, addPrefix, stripPrefix
    , emptyDictState
    )

{-| Types for the `LocalStorage` module.


# Types

@docs Key, Value, Operation, Ports, DictState, MsgWrapper


# Port Signatures

@docs GetItemPort, SetItemPort, ClearPort, ListKeysPort, ReceiveItemPort


# Functions

@docs receiveWrapper, decodeStringList, addPrefix, stripPrefix


# Constants

@docs emptyDictState

-}

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE


{-| A convenience type for keys in the store. Same as `String`.
-}
type alias Key =
    String


{-| A convenience type for values in the store. Same as `Json.Encode.Value`.
-}
type alias Value =
    JE.Value


{-| A `Dict` that stores key/value pairs for the simulated ports.
-}
type alias DictState =
    Dict Key Value


{-| `Dict.empty`
-}
emptyDictState : DictState
emptyDictState =
    Dict.empty


{-| The operation that caused your wrapper `Msg` to be sent.

For real ports, you'll only ever see GetItemOperation.

For simluated ports, you'll see the others as well, because you have to save the updated state.

-}
type Operation
    = GetItemOperation
    | SetItemOperation
    | ClearOperation
    | ListKeysOperation
    | ErrorOperation


{-| The required signature of your `localStorage.getItem` port.
-}
type alias GetItemPort msg =
    Key -> Cmd msg


{-| The required signature of your `localStorage.setItem` port.
-}
type alias SetItemPort msg =
    ( Key, Value ) -> Cmd msg


{-| The required signature of your `localStorage.clear` port.
-}
type alias ClearPort msg =
    String -> Cmd msg


{-| The required signature of your `localStorage.list` port.
-}
type alias ListKeysPort msg =
    String -> Cmd msg


{-| The required signature of your subscription to receive `getItem` values.
-}
type alias ReceiveItemPort msg =
    (Value -> msg) -> Sub msg


kvDecoder : String -> JD.Decoder ( Key, Value )
kvDecoder prefix =
    JD.map2 (\a b -> ( a, b ))
        (JD.map (stripPrefix prefix) <| JD.field "key" JD.string)
        (JD.field "value" JD.value)


keysDecoder : String -> JD.Decoder ( Key, Value )
keysDecoder prefix =
    JD.map2 (\a b -> ( a, b ))
        (JD.map (stripPrefix prefix) <| JD.field "prefix" JD.string)
        (JD.field "keys" (JD.list JD.string)
            |> JD.map (encodeKeyList prefix)
        )


encodeKeyList : String -> List String -> Value
encodeKeyList prefix keys =
    List.map (stripPrefix prefix) keys
        |> List.map JE.string
        |> JE.list identity


{-| Decode a Value representing a list of strings.

Useful for the result of `LocalStorage.listKeys`.

-}
decodeStringList : Value -> Result String (List String)
decodeStringList value =
    case JD.decodeValue (JD.list JD.string) value of
        Err err ->
            Err <| JD.errorToString err

        Ok list ->
            Ok list


{-| Drop the length of the first arg from the left of the second.
-}
stripPrefix : String -> Key -> String
stripPrefix prefix key =
    let
        len =
            case String.length prefix of
                0 ->
                    0

                x ->
                    x + 1
    in
    String.dropLeft len key


{-| Prepend the `String` and a period to the `Key`, or nothing if the `String` is empty.
-}
addPrefix : String -> Key -> Key
addPrefix prefix key =
    if prefix == "" then
        key

    else
        prefix ++ "." ++ key


{-| Use this to turn the Value coming from a receive port into a Msg.
-}
receiveWrapper : MsgWrapper msg -> String -> Value -> msg
receiveWrapper wrapper prefix json =
    case JD.decodeValue (kvDecoder prefix) json of
        Ok ( key, value ) ->
            wrapper GetItemOperation Nothing key value

        Err _ ->
            case JD.decodeValue (keysDecoder prefix) json of
                Ok ( key, value ) ->
                    wrapper ListKeysOperation Nothing key value

                Err err ->
                    wrapper ErrorOperation
                        Nothing
                        (JD.errorToString err)
                        JE.null


{-| Your Msg, which wraps the key/value pair from a `getItem` return.

For real ports, you'll only care about the `GetItem` operation. You'll get `Nothing` for the `Ports`.

For simluated ports, you need to store the `Ports` in your `Model`.

-}
type alias MsgWrapper msg =
    Operation -> Maybe (Ports msg) -> Key -> Value -> msg


{-| Wrap up your ports.

You'll usually create one of these with `LocalStorage.realRealPorts` or `DictPorts.make`.

Your update Msg will receive one, if you're using simulated ports.

-}
type Ports msg
    = Ports
        { getItem : Ports msg -> GetItemPort msg
        , setItem : Ports msg -> SetItemPort msg
        , clear : Ports msg -> ClearPort msg
        , listKeys : Ports msg -> ListKeysPort msg
        , state : DictState
        }
