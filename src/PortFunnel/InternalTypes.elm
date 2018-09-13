----------------------------------------------------------------------
--
-- InternalTypes.elm
-- Internal types, needed by the test code, but opaque to user code.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module PortFunnel.InternalTypes exposing (Key, Message(..), Prefix, Value)

import Json.Encode as JE


type alias Key =
    String


type alias Value =
    JE.Value


type alias Prefix =
    String


type Message
    = Startup
    | Get Key
    | Put Key (Maybe Value)
    | ListKeys Prefix
    | Keys Prefix (List Key)
    | Clear Prefix
    | SimulateGet Key
    | SimulatePut Key (Maybe Value)
    | SimulateListKeys Prefix
    | SimulateClear Prefix
