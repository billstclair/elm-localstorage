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


module LocalStorage.SharedTypes
    exposing
        ( DictState
        , Key
        , Operation(..)
        , Ports(..)
        , Value
        , emptyDictState
        )

import Dict exposing (Dict)
import Json.Encode


type alias Key =
    String


type alias Value =
    Json.Encode.Value


type alias DictState =
    Dict Key Value


emptyDictState : DictState
emptyDictState =
    Dict.empty


type Operation
    = GetItemOperation
    | SetItemOperation
    | ClearOperation


type Ports msg
    = Ports
        { getItem : Ports msg -> ( String, Key ) -> Cmd msg
        , setItem : Ports msg -> ( String, Key, Value ) -> Cmd msg
        , clear : Ports msg -> String -> Cmd msg
        , state : DictState
        }
