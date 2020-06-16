module LocalStorage exposing
    ( ClearPort, GetItemPort, SetItemPort, ListKeysPort
    , LocalStorage, make
    , clear, getItem, setItem, listKeys
    )

{-| A minimal local storage API that mirrors the raw API very closely.


# User supplied port signatures.

@docs ClearPort, GetItemPort, SetItemPort, ListKeysPort


# Local storage handle and constructor.

@docs LocalStorage, make


# Operations on local storage.

@docs clear, getItem, setItem, listKeys

-}

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE



-- Port signatures.


type alias GetItemPort msg =
    String -> Cmd msg


type alias SetItemPort msg =
    ( String, JE.Value ) -> Cmd msg


type alias ClearPort msg =
    String -> Cmd msg


type alias ListKeysPort msg =
    String -> Cmd msg



--


type LocalStorage msg
    = LocalStorage ( Ports msg, String )


make : GetItemPort msg -> SetItemPort msg -> ClearPort msg -> ListKeysPort msg -> String -> LocalStorage msg
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


clear : LocalStorage msg -> Cmd msg
clear (LocalStorage ( ports, prefix )) =
    addPrefix prefix ""
        |> ports.clear


getItem : LocalStorage msg -> String -> Cmd msg
getItem (LocalStorage ( ports, prefix )) key =
    addPrefix prefix key
        |> ports.getItem


setItem : LocalStorage msg -> String -> JE.Value -> Cmd msg
setItem (LocalStorage ( ports, prefix )) key value =
    ( addPrefix prefix key, value )
        |> ports.setItem


listKeys : LocalStorage msg -> String -> Cmd msg
listKeys (LocalStorage ( ports, prefix )) userPrefix =
    addPrefix prefix userPrefix
        |> ports.listKeys



-- Helpers


type alias Ports msg =
    { getItem : GetItemPort msg
    , setItem : SetItemPort msg
    , clear : ClearPort msg
    , listKeys : ListKeysPort msg
    }


addPrefix : String -> String -> String
addPrefix prefix key =
    if prefix == "" then
        key

    else
        prefix ++ "." ++ key
