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


module LocalStorage.DictPorts exposing (CmdWrapper, make)

import Dict exposing (Dict)
import Json.Encode as JE
import LocalStorage.SharedTypes exposing (Key, Operation(..), Ports(..), Value)


type alias StorageDict =
    Dict Key Value


type alias StoragePorts msg =
    Ports StorageDict msg


type alias CmdWrapper msg =
    Operation -> StoragePorts msg -> ( Key, Value ) -> Cmd msg


make : CmdWrapper msg -> StoragePorts msg
make wrapper =
    let
        state : StorageDict
        state =
            Dict.empty
    in
    Ports
        { getItem = getItem wrapper
        , setItem = setItem wrapper
        , clear = clear wrapper
        , state = state
        }


getItem : CmdWrapper msg -> StoragePorts msg -> String -> Key -> Cmd msg
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
            wrapper GetItemOperation ports ( key, value )


setItem : CmdWrapper msg -> StoragePorts msg -> String -> Key -> Value -> Cmd msg
setItem wrapper ports prefix key value =
    case ports of
        Ports p ->
            let
                dict =
                    Dict.insert key value p.state

                newPorts =
                    Ports { p | state = dict }
            in
            wrapper SetItemOperation newPorts ( key, value )


clear : CmdWrapper msg -> StoragePorts msg -> String -> Cmd msg
clear wrapper ports prefix =
    case ports of
        Ports p ->
            let
                newPorts =
                    Ports { p | state = Dict.empty }
            in
            wrapper ClearOperation newPorts ( "", JE.null )
