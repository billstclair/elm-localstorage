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
import LocalStorage.DictPorts as DictPorts exposing (DictPorts, DictState)
import LocalStorage.SharedTypes exposing (Key, Operation, Ports, Value)
import SharedUI exposing (Model, Msg(..), init, update, view)
import Task


main =
    Html.program
        { init = init JE.null ports
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


wrapper : Operation -> DictPorts (Msg DictState) -> ( Key, Value ) -> Cmd (Msg DictState)
wrapper operation ports kvpair =
    UpdatePorts operation ports kvpair
        |> Task.succeed
        |> Task.perform identity


ports : DictPorts (Msg DictState)
ports =
    DictPorts.make wrapper
