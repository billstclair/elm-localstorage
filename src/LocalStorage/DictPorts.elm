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


module LocalStorage.DictPorts exposing (make)

{-| Simulate ports for using the `LocalStorage` module from `elm reactor`.


# Functions

@docs make

-}

import Dict exposing (Dict)
import Json.Encode as JE
import LocalStorage.SharedTypes
    exposing
        ( DictState
        , Key
        , MsgWrapper
        , Operation(..)
        , Ports(..)
        , Value
        , emptyDictState
        , stripPrefix
        )
import Task


type alias CmdWrapper msg =
    Operation -> Maybe (Ports msg) -> Key -> Value -> Cmd msg


wrapperToCmd : MsgWrapper msg -> Operation -> Maybe (Ports msg) -> Key -> Value -> Cmd msg
wrapperToCmd wrapper operation ports key value =
    wrapper operation ports key value
        |> Task.succeed
        |> Task.perform identity


{-| Create a `Ports` object from a message wrapper, usually just one of your Msg options.
-}
make : MsgWrapper msg -> String -> Ports msg
make wrapper prefix =
    let
        cmdWrapper =
            wrapperToCmd wrapper
    in
    Ports
        { getItem = getItem cmdWrapper prefix
        , setItem = setItem cmdWrapper prefix
        , clear = clear cmdWrapper prefix
        , listKeys = listKeys cmdWrapper prefix
        , state = emptyDictState
        }


getItem : CmdWrapper msg -> String -> Ports msg -> Key -> Cmd msg
getItem wrapper prefix ports key =
    case ports of
        Ports p ->
            let
                value =
                    case Dict.get key p.state of
                        Nothing ->
                            JE.null

                        Just v ->
                            v
            in
            wrapper GetItemOperation Nothing (stripPrefix prefix key) value


setItem : CmdWrapper msg -> String -> Ports msg -> ( Key, Value ) -> Cmd msg
setItem wrapper prefix ports ( key, value ) =
    case ports of
        Ports p ->
            let
                dict =
                    if value == JE.null then
                        Dict.remove key p.state

                    else
                        Dict.insert key value p.state

                newPorts =
                    Ports { p | state = dict }
            in
            wrapper SetItemOperation (Just newPorts) (stripPrefix prefix key) value


clear : CmdWrapper msg -> String -> Ports msg -> String -> Cmd msg
clear wrapper prefix ports fullPrefix =
    case ports of
        Ports p ->
            let
                newPorts =
                    Ports { p | state = Dict.empty }
            in
            wrapper ClearOperation (Just newPorts) "" JE.null


listKeys : CmdWrapper msg -> String -> Ports msg -> String -> Cmd msg
listKeys wrapper prefix ports fullPrefix =
    case ports of
        Ports p ->
            let
                collect : String -> Value -> List String -> List String
                collect =
                    \key _ res ->
                        if String.startsWith fullPrefix key then
                            key :: res

                        else
                            res

                keys =
                    Dict.foldr collect [] p.state
                        |> List.map (stripPrefix prefix)

                value =
                    JE.list identity <| List.map JE.string keys
            in
            wrapper ListKeysOperation Nothing (stripPrefix prefix fullPrefix) value
