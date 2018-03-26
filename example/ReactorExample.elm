----------------------------------------------------------------------
--
-- ReactorExample.elm
-- LocalStorage example for use with elm reactor.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module ReactorExample exposing (..)

import Html
import Json.Encode as JE
import LocalStorage.DictPorts as DictPorts
import LocalStorage.SharedTypes exposing (Key, Operation, Ports, Value)
import SharedUI exposing (Model, Msg(..), init, prefix, update, view)
import Task


main =
    Html.program
        { init = init JE.null ports
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


ports : Ports Msg
ports =
    DictPorts.make UpdatePorts prefix
