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


module LocalStorage.SharedTypes exposing (Key, Operation(..), Ports(..), Value)

import Json.Encode


type alias Key =
    String


type alias Value =
    Json.Encode.Value


type Operation
    = GetItemOperation
    | SetItemOperation
    | ClearOperation


type Ports state msg
    = Ports
        { getItem : Ports state msg -> String -> Key -> Cmd msg
        , setItem : Ports state msg -> String -> Key -> Value -> Cmd msg
        , clear : Ports state msg -> String -> Cmd msg
        , state : state
        }
