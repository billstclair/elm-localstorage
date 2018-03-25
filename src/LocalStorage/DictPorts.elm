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


module LocalStorage.DictPorts exposing (MsgWrapper, make)

{-| Enable development use of the `LocalStorage` module from `elm reactor`.
-}

import Dict exposing (Dict)
import Json.Encode as JE
import LocalStorage.SharedTypes
    exposing
        ( DictState
        , Key
        , Operation(..)
        , Ports(..)
        , Value
        , emptyDictState
        )
import Task


type alias CmdWrapper msg =
    Operation -> Maybe (Ports msg) -> Key -> Value -> Cmd msg


type alias MsgWrapper msg =
    Operation -> Maybe (Ports msg) -> Key -> Value -> msg


wrapperToCmd : MsgWrapper msg -> Operation -> Maybe (Ports msg) -> Key -> Value -> Cmd msg
wrapperToCmd wrapper operation ports key value =
    wrapper operation ports key value
        |> Task.succeed
        |> Task.perform identity


{-| Create a `Ports` object from a message wrapper, usually just one of your Msg options.
-}
make : MsgWrapper msg -> Ports msg
make wrapper =
    let
        cmdWrapper =
            wrapperToCmd wrapper
    in
    Ports
        { getItem = getItem cmdWrapper
        , setItem = setItem cmdWrapper
        , clear = clear cmdWrapper
        , state = emptyDictState
        }


getItem : CmdWrapper msg -> Ports msg -> Key -> Cmd msg
getItem wrapper ports key =
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
            wrapper GetItemOperation (Just ports) key value


setItem : CmdWrapper msg -> Ports msg -> ( Key, Value ) -> Cmd msg
setItem wrapper ports ( key, value ) =
    case ports of
        Ports p ->
            let
                dict =
                    Dict.insert key value p.state

                newPorts =
                    Ports { p | state = dict }
            in
            wrapper SetItemOperation (Just newPorts) key value


clear : CmdWrapper msg -> Ports msg -> String -> Cmd msg
clear wrapper ports prefix =
    case ports of
        Ports p ->
            let
                newPorts =
                    Ports { p | state = Dict.empty }
            in
            wrapper ClearOperation (Just newPorts) "" JE.null
