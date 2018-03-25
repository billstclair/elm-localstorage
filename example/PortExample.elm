----------------------------------------------------------------------
--
-- PortExample.elm
-- LocalStorage example for use with real JS localStorage via Elm ports
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


port module PortExample exposing (..)

import Html
import Json.Encode as JE
import LocalStorage
import LocalStorage.DictPorts as DictPorts
import LocalStorage.SharedTypes exposing (Key, Operation(..), Ports, Value)
import SharedUI exposing (Model, Msg(..), getPorts, init, update, view)
import Task


main =
    Html.programWithFlags
        { init = \initialModel -> init initialModel ports
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


ports : Ports Msg
ports =
    LocalStorage.makeRealPorts "example" getItem setItem clear


port getItem : ( String, Key ) -> Cmd msg


port setItem : ( String, Key, Value ) -> Cmd msg


port clear : String -> Cmd msg


port receiveItem : (( Key, Value ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveItem <| \( key, value ) -> UpdatePorts GetItemOperation Nothing key value
