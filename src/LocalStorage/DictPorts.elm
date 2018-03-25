----------------------------------------------------------------------
--
-- DictPorts.elm
-- Dictionary implementation of LocalStorage Ports, for use during development.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module LocalStorage.DictPorts exposing (CmdWrapper, DictPorts, DictState, make)

import Dict exposing (Dict)
import Json.Encode as JE
import LocalStorage.SharedTypes exposing (Key, Operation(..), Ports(..), Value)


type alias DictState =
    Dict Key Value


type alias DictPorts msg =
    Ports DictState msg


type alias CmdWrapper msg =
    Operation -> DictPorts msg -> Key -> Value -> Cmd msg


make : CmdWrapper msg -> DictPorts msg
make wrapper =
    Ports
        { getItem = getItem wrapper
        , setItem = setItem wrapper
        , clear = clear wrapper
        , state = Dict.empty
        }


getItem : CmdWrapper msg -> DictPorts msg -> String -> Key -> Cmd msg
getItem wrapper ports prefix key =
    case ports of
        Ports p ->
            let
                value =
                    case Dict.get key p.state of
                        Nothing ->
                            JE.null

                        Just value ->
                            value
            in
            wrapper GetItemOperation ports key value


setItem : CmdWrapper msg -> DictPorts msg -> String -> Key -> Value -> Cmd msg
setItem wrapper ports prefix key value =
    case ports of
        Ports p ->
            let
                dict =
                    Dict.insert key value p.state

                newPorts =
                    Ports { p | state = dict }
            in
            wrapper SetItemOperation newPorts key value


clear : CmdWrapper msg -> DictPorts msg -> String -> Cmd msg
clear wrapper ports prefix =
    case ports of
        Ports p ->
            let
                newPorts =
                    Ports { p | state = Dict.empty }
            in
            wrapper ClearOperation newPorts "" JE.null
