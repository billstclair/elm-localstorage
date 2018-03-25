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
        , make
        , setItem
        , subscription
        )

import LocalStorage.SharedTypes exposing (Key, Ports, Value)


type LocalStorage msg
    = LocalStorage ( Ports msg, String )


make : Ports msg -> String -> LocalStorage msg
make ports prefix =
    LocalStorage ( ports, prefix )


getItem : LocalStorage msg -> Key -> Cmd msg
getItem (LocalStorage ( ports, prefix )) key =
    ports.getItem prefix key


setItem : LocalStorage msg -> ( Key, Value ) -> Cmd msg
setItem (LocalStorage ( ports, prefix )) ( key, value ) =
    ports.setItem prefix key value


clear : LocalStorage msg -> Cmd msg
clear (LocalStorage ( ports, prefix )) =
    ports.clear prefix


subscription : LocalStorage msg -> (( Key, Value ) -> msg) -> Sub msg
subscription (LocalStorage ( ports, prefix )) wrapper =
    ports.subscription prefix wrapper
