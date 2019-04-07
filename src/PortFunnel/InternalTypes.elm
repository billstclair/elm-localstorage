----------------------------------------------------------------------
--
-- InternalTypes.elm
-- Internal types, needed by the test code, but opaque to user code.
-- Copyright (c) 2018-2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module PortFunnel.InternalTypes exposing (Key, Label, Message(..), Prefix, Value)

import Json.Encode as JE


type alias Key =
    String


type alias Value =
    JE.Value


type alias Prefix =
    String


type alias Label =
    Maybe String


type Message
    = Startup
    | Get Label Key
    | Got Label Key (Maybe Value)
    | Put Key (Maybe Value)
    | ListKeys Label Prefix
    | Keys Label Prefix (List Key)
    | Clear Prefix
    | SimulateGet Label Key
    | SimulatePut Key (Maybe Value)
    | SimulateListKeys Label Prefix
    | SimulateClear Prefix
