module LocalStorage exposing
    ( ClearPort, GetItemPort, SetItemPort, ListKeysPort, ResponsePort
    , LocalStorage, make
    , clear, getItem, setItem, listKeys
    , Response(..), responseHandler
    )

{-| A minimal local storage API that mirrors the raw API very closely.


# User supplied port signatures.

@docs ClearPort, GetItemPort, SetItemPort, ListKeysPort, ResponsePort


# Local storage handle and constructor.

@docs LocalStorage, make


# Operations on local storage.

@docs clear, getItem, setItem, listKeys


# Responses from local storage.

@docs Response, responseHandler

-}

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE



-- Port signatures.


{-| Port to clear the whole area of local storage underneath a prefix.
-}
type alias ClearPort msg =
    String -> Cmd msg


{-| Port to fetch an item by its key and prefix.
-}
type alias GetItemPort msg =
    String -> Cmd msg


{-| Port to set the value of an item by key and prefix.
-}
type alias SetItemPort msg =
    ( String, JE.Value ) -> Cmd msg


{-| Port to list the available local storage keys underneath a prefix.
-}
type alias ListKeysPort msg =
    String -> Cmd msg


{-| Subscription port to listen to all operation responses from local storage with.
-}
type alias ResponsePort msg =
    (JE.Value -> msg) -> Sub msg



-- Local storage handle and constructor.


{-| The type of the local storage handle, required to perform operations on
local storage.
-}
type LocalStorage msg
    = LocalStorage ( Ports msg, String )


type alias Ports msg =
    { getItem : GetItemPort msg
    , setItem : SetItemPort msg
    , clear : ClearPort msg
    , listKeys : ListKeysPort msg
    }


{-| Builds local storage using the supplied port implementations and a prefix to
namespace all keys stored in that area of local storage.
-}
make :
    GetItemPort msg
    -> SetItemPort msg
    -> ClearPort msg
    -> ListKeysPort msg
    -> String
    -> LocalStorage msg
make getPort setPort clearPort listKeysPort prefix =
    let
        ports =
            { getItem = getPort
            , setItem = setPort
            , clear = clearPort
            , listKeys = listKeysPort
            }
    in
    LocalStorage ( ports, prefix )



-- Operations on local storage.


{-| Clears the whole area of local storage underneath a prefix.
-}
clear : LocalStorage msg -> Cmd msg
clear (LocalStorage ( ports, prefix )) =
    addPrefix prefix ""
        |> ports.clear


{-| Fetches an item by its key and prefix.
-}
getItem : LocalStorage msg -> String -> Cmd msg
getItem (LocalStorage ( ports, prefix )) key =
    addPrefix prefix key
        |> ports.getItem


{-| Sets the value of an item by key and prefix.
-}
setItem : LocalStorage msg -> String -> JE.Value -> Cmd msg
setItem (LocalStorage ( ports, prefix )) key value =
    ( addPrefix prefix key, value )
        |> ports.setItem


{-| Lists the available local storage keys underneath a prefix.
-}
listKeys : LocalStorage msg -> String -> Cmd msg
listKeys (LocalStorage ( ports, prefix )) userPrefix =
    addPrefix prefix userPrefix
        |> ports.listKeys



-- Responses from local storage.


{-| The possible local storage operation responses.
-}
type Response
    = Item String JE.Value
    | ItemNotFound String
    | KeyList (List String)
    | Error String


{-| Creates a response handler to use with the `ResponsePort` subscription port.

The operation, key and JSON value message builder combined with the storage
prefix are needed to create a response handler.

-}
responseHandler : (Response -> msg) -> String -> JE.Value -> msg
responseHandler wrapper prefix json =
    case JD.decodeValue (responseDecoder prefix) json of
        Ok resp ->
            wrapper resp

        Err err ->
            wrapper (Error (JD.errorToString err))



-- Helpers


addPrefix : String -> String -> String
addPrefix prefix key =
    if prefix == "" then
        key

    else
        prefix ++ "." ++ key


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


responseDecoder : String -> JD.Decoder Response
responseDecoder prefix =
    JD.oneOf
        [ getItemDecoder prefix
        , listKeysDecoder prefix
        ]


getItemDecoder : String -> JD.Decoder Response
getItemDecoder prefix =
    JD.map2
        (\k v ->
            if v == JE.null then
                ItemNotFound k

            else
                Item k v
        )
        (JD.map (stripPrefix prefix) <| JD.field "key" JD.string)
        (JD.field "value" JD.value)


listKeysDecoder : String -> JD.Decoder Response
listKeysDecoder prefix =
    JD.list JD.string
        |> JD.map KeyList
