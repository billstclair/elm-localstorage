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


module LocalStorage exposing
    ( LocalStorage
    , make, makeRealPorts
    , getItem, setItem, clear, listKeys
    , getPorts, setPorts, getPrefix
    )

{-| The 'LocalStorage' module does most of the work in using the JavaScript `localStorage` facility to persistently store key/value pairs.


# Types

@docs LocalStorage


# Constructors

@docs make, makeRealPorts


# Functions

@docs getItem, setItem, clear, listKeys


# State accessors

@docs getPorts, setPorts, getPrefix

-}

import LocalStorage.SharedTypes
    exposing
        ( ClearPort
        , GetItemPort
        , Key
        , ListKeysPort
        , Ports(..)
        , SetItemPort
        , Value
        , addPrefix
        , emptyDictState
        )


{-| Opaque type. Returned by `make` and `getPorts`. Passed to the other functions.
-}
type LocalStorage msg
    = LocalStorage ( Ports msg, String )


{-| Return the `Ports` instance you passed to `make` or `setPorts`.
-}
getPorts : LocalStorage msg -> Ports msg
getPorts (LocalStorage ( ports, _ )) =
    ports


{-| Set the internal `Ports` value. Fetch it back with `getPorts`.
-}
setPorts : Ports msg -> LocalStorage msg -> LocalStorage msg
setPorts ports (LocalStorage ( _, prefix )) =
    LocalStorage ( ports, prefix )


{-| Get the prefix passed to `make`
-}
getPrefix : LocalStorage msg -> String
getPrefix (LocalStorage ( _, prefix )) =
    prefix


{-| Make a `LocalStorage` instance.

If `prefix` is not empty (""), will prefix all keys in JS `localStorage` with
`prefix ++ "."`.

-}
make : Ports msg -> String -> LocalStorage msg
make ports prefix =
    LocalStorage ( ports, prefix )


{-| Create a `Ports` value, using what are usually your real Elm `port`s.
-}
makeRealPorts : GetItemPort msg -> SetItemPort msg -> ClearPort msg -> ListKeysPort msg -> Ports msg
makeRealPorts getPort setPort clearPort listKeysPort =
    Ports
        { getItem = \_ -> getPort
        , setItem = \_ -> setPort
        , clear = \_ -> clearPort
        , listKeys = \_ -> listKeysPort
        , state = emptyDictState
        }


{-| Return a `Cmd` to fetch the value for the `Key` from `LocalStorage`.
-}
getItem : LocalStorage msg -> Key -> Cmd msg
getItem (LocalStorage ( ports, prefix )) key =
    case ports of
        Ports p ->
            p.getItem ports (addPrefix prefix key)


{-| Return a `Cmd` to set the value for the `Key` to `Value` in `LocalStorage`.
-}
setItem : LocalStorage msg -> Key -> Value -> Cmd msg
setItem (LocalStorage ( ports, prefix )) key value =
    case ports of
        Ports p ->
            p.setItem ports ( addPrefix prefix key, value )


{-| Return a `Cmd` to clear all JS `localStorage` keys that begin with the `prefix` passed to `make`.
-}
clear : LocalStorage msg -> Cmd msg
clear (LocalStorage ( ports, prefix )) =
    case ports of
        Ports p ->
            p.clear ports <| addPrefix prefix ""


{-| Return a `Cmd` to list all JS `localStorage` keys that begin with the `prefix` passed to `make` followed by the String you pass here.
-}
listKeys : LocalStorage msg -> String -> Cmd msg
listKeys (LocalStorage ( ports, prefix )) userPrefix =
    case ports of
        Ports p ->
            p.listKeys ports <| addPrefix prefix userPrefix
