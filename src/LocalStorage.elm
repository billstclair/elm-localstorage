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


module LocalStorage
    exposing
        ( LocalStorage
        , clear
        , getItem
        , getPorts
        , make
        , makeRealPorts
        , setItem
        , setPorts
        )

import LocalStorage.SharedTypes exposing (Key, Ports(..), Value, emptyDictState)


type LocalStorage msg
    = LocalStorage ( Ports msg, String )


getPorts : LocalStorage msg -> Ports msg
getPorts (LocalStorage ( ports, _ )) =
    ports


setPorts : Ports msg -> LocalStorage msg -> LocalStorage msg
setPorts ports (LocalStorage ( _, prefix )) =
    LocalStorage ( ports, prefix )


make : Ports msg -> String -> LocalStorage msg
make ports prefix =
    LocalStorage ( ports, prefix )


makeRealPorts : (Key -> Cmd msg) -> (( Key, Value ) -> Cmd msg) -> (String -> Cmd msg) -> Ports msg
makeRealPorts getPort setPort clearPort =
    Ports
        { getItem = \_ -> getPort
        , setItem = \_ -> setPort
        , clear = \_ -> clearPort
        , state = emptyDictState
        }


addPrefix : String -> Key -> Key
addPrefix prefix key =
    if prefix == "" then
        key
    else
        prefix ++ "." ++ key


getItem : LocalStorage msg -> Key -> Cmd msg
getItem (LocalStorage ( ports, prefix )) key =
    case ports of
        Ports p ->
            p.getItem ports (addPrefix prefix key)


setItem : LocalStorage msg -> Key -> Value -> Cmd msg
setItem (LocalStorage ( ports, prefix )) key value =
    case ports of
        Ports p ->
            p.setItem ports ( addPrefix prefix key, value )


clear : LocalStorage msg -> Cmd msg
clear (LocalStorage ( ports, prefix )) =
    case ports of
        Ports p ->
            p.clear ports prefix
