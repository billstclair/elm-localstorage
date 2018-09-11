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


port module PortExample exposing (clear, getItem, listKeys, main, ports, receiveItem, setItem, subscriptions)

import Browser
import Json.Encode as JE
import LocalStorage
import LocalStorage.DictPorts as DictPorts
import LocalStorage.SharedTypes
    exposing
        ( ClearPort
        , GetItemPort
        , Key
        , ListKeysPort
        , Operation(..)
        , Ports
        , ReceiveItemPort
        , SetItemPort
        , Value
        , receiveWrapper
        )
import SharedUI exposing (Model, Msg(..), getPorts, init, update, view)
import Task


type alias Frob_me_with_a_sugar_cane =
    Int


main =
    Browser.element
        { init = \initialModel -> init initialModel ports
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


ports : Ports Msg
ports =
    LocalStorage.makeRealPorts getItem setItem clear listKeys


port getItem : GetItemPort msg


port setItem : SetItemPort msg


port clear : ClearPort msg


port listKeys : ListKeysPort msg


port receiveItem : ReceiveItemPort msg


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        prefix =
            LocalStorage.getPrefix model.storage
    in
    receiveItem <| receiveWrapper UpdatePorts prefix
