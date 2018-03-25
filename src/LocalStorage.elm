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
        , makeReal
        , setItem
        , setPorts
        )

import LocalStorage.SharedTypes exposing (Key, Ports(..), Value)


type LocalStorage state msg
    = LocalStorage ( Ports state msg, String )


setPorts : Ports state msg -> LocalStorage state msg -> LocalStorage state msg
setPorts ports (LocalStorage ( _, prefix )) =
    LocalStorage ( ports, prefix )


make : Ports state msg -> String -> LocalStorage state msg
make ports prefix =
    LocalStorage ( ports, prefix )


makeReal : String -> (String -> Key -> Cmd msg) -> (String -> Key -> Value -> Cmd msg) -> (String -> Cmd msg) -> LocalStorage String msg
makeReal prefix getPort setPort clearPort =
    let
        ports =
            Ports
                { getItem = \_ -> getPort
                , setItem = \_ -> setPort
                , clear = \_ -> clearPort
                , state = "Move along. Nothing to see here."
                }
    in
    make ports prefix


getItem : LocalStorage state msg -> Key -> Cmd msg
getItem (LocalStorage ( ports, prefix )) key =
    case ports of
        Ports p ->
            p.getItem ports prefix key


setItem : LocalStorage state msg -> Key -> Value -> Cmd msg
setItem (LocalStorage ( ports, prefix )) key value =
    case ports of
        Ports p ->
            p.setItem ports prefix key value


clear : LocalStorage state msg -> Cmd msg
clear (LocalStorage ( ports, prefix )) =
    case ports of
        Ports p ->
            p.clear ports prefix
