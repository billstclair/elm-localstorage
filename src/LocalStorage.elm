module LocalStorage exposing
    ( LocalStorage
    , clear
    , getItem
    , getPorts
    , getPrefix
    , listKeys
    , make
    , makeRealPorts
    , setItem
    , setPorts
    )

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE


type alias GetItemPort msg =
    String -> Cmd msg


type alias SetItemPort msg =
    ( String, JE.Value ) -> Cmd msg


type alias ClearPort msg =
    String -> Cmd msg


type alias ListKeysPort msg =
    String -> Cmd msg


type alias ReceiveItemPort msg =
    (JE.Value -> msg) -> Sub msg


type Operation
    = GetItemOperation
    | SetItemOperation
    | ClearOperation
    | ListKeysOperation
    | ErrorOperation


kvDecoder : String -> JD.Decoder ( String, JE.Value )
kvDecoder prefix =
    JD.map2 (\a b -> ( a, b ))
        (JD.map (stripPrefix prefix) <| JD.field "key" JD.string)
        (JD.field "value" JD.value)


keysDecoder : String -> JD.Decoder ( String, JE.Value )
keysDecoder prefix =
    JD.map2 (\a b -> ( a, b ))
        (JD.map (stripPrefix prefix) <| JD.field "prefix" JD.string)
        (JD.field "keys" (JD.list JD.string)
            |> JD.map (encodeKeyList prefix)
        )


encodeKeyList : String -> List String -> JE.Value
encodeKeyList prefix keys =
    List.map (stripPrefix prefix) keys
        |> List.map JE.string
        |> JE.list identity


decodeStringList : JE.Value -> Result String (List String)
decodeStringList value =
    case JD.decodeValue (JD.list JD.string) value of
        Err err ->
            Err <| JD.errorToString err

        Ok list ->
            Ok list


stripPrefix : String -> String -> String
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


addPrefix : String -> String -> String
addPrefix prefix key =
    if prefix == "" then
        key

    else
        prefix ++ "." ++ key


receiveWrapper : MsgWrapper msg -> String -> JE.Value -> msg
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


type alias MsgWrapper msg =
    Operation -> Maybe (Ports msg) -> String -> JE.Value -> msg


type Ports msg
    = Ports
        { getItem : Ports msg -> GetItemPort msg
        , setItem : Ports msg -> SetItemPort msg
        , clear : Ports msg -> ClearPort msg
        , listKeys : Ports msg -> ListKeysPort msg
        , state : Dict String JE.Value
        }


type LocalStorage msg
    = LocalStorage ( Ports msg, String )


getPorts : LocalStorage msg -> Ports msg
getPorts (LocalStorage ( ports, _ )) =
    ports


setPorts : Ports msg -> LocalStorage msg -> LocalStorage msg
setPorts ports (LocalStorage ( _, prefix )) =
    LocalStorage ( ports, prefix )


getPrefix : LocalStorage msg -> String
getPrefix (LocalStorage ( _, prefix )) =
    prefix


make : Ports msg -> String -> LocalStorage msg
make ports prefix =
    LocalStorage ( ports, prefix )


makeRealPorts : GetItemPort msg -> SetItemPort msg -> ClearPort msg -> ListKeysPort msg -> Ports msg
makeRealPorts getPort setPort clearPort listKeysPort =
    Ports
        { getItem = \_ -> getPort
        , setItem = \_ -> setPort
        , clear = \_ -> clearPort
        , listKeys = \_ -> listKeysPort
        , state = Dict.empty
        }


getItem : LocalStorage msg -> String -> Cmd msg
getItem (LocalStorage ( ports, prefix )) key =
    case ports of
        Ports p ->
            p.getItem ports (addPrefix prefix key)


setItem : LocalStorage msg -> String -> JE.Value -> Cmd msg
setItem (LocalStorage ( ports, prefix )) key value =
    case ports of
        Ports p ->
            p.setItem ports ( addPrefix prefix key, value )


clear : LocalStorage msg -> Cmd msg
clear (LocalStorage ( ports, prefix )) =
    case ports of
        Ports p ->
            p.clear ports <| addPrefix prefix ""


listKeys : LocalStorage msg -> String -> Cmd msg
listKeys (LocalStorage ( ports, prefix )) userPrefix =
    case ports of
        Ports p ->
            p.listKeys ports <| addPrefix prefix userPrefix
