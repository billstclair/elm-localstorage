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


module LocalStorage.SharedTypes exposing (Key, Ports, Value)

import Json.Encode


type alias Key =
    String


type alias Value =
    Json.Encode.Value


type alias Ports msg =
    { getItem : String -> Key -> Cmd msg
    , setItem : String -> Key -> Value -> Cmd msg
    , clear : String -> Cmd msg
    , subscription : String -> (( Key, Value ) -> msg) -> Sub msg
    }
