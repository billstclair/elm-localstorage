----------------------------------------------------------------------
--
-- PortFunnels.elm
-- Most of the support needed for a PortFunnel application
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


port module PortFunnels exposing
    ( FunnelDict
    , Handler(..)
    , State
    , getCmdPort
    , initialState
    , makeFunnelDict
    , processValue
    , subscriptions
    )

{-| This module is the basis for a `PortFunnels` module of your own.

You need to modify it, adding all the funnel modules you use, and usually removing the `PortFunnel.Echo` and `PortFunnel.AddXY` modules.

See Main.elm for an example of using it as is.

If you rename it, you need to change the name in `site/index.html` to match.

-}

import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h1, input, p, span, text)
import Json.Encode as JE exposing (Value)
import PortFunnel
    exposing
        ( FunnelSpec
        , GenericMessage
        , ModuleDesc
        , StateAccessors
        )
import PortFunnel.LocalStorage as LocalStorage


{-| Add a property to this type for each funnel module you use.
-}
type alias State =
    { storage : LocalStorage.State
    }


{-| Create the initial state record.

The `String` parameter is the prefix for the localStorage.

-}
initialState : String -> State
initialState prefix =
    { storage = LocalStorage.initialState prefix
    }


{-| Make a `StateAccessors` instance for each funnel module.
-}
localStorageAccessors : StateAccessors State LocalStorage.State
localStorageAccessors =
    StateAccessors .storage (\substate state -> { state | storage = substate })


{-| A `Funnel` tags a module-specific `FunnelSpec`.

Add a tag here for each funnel module you use.

-}
type Funnel model msg
    = LocalStorageFunnel (FunnelSpec State LocalStorage.State LocalStorage.Message LocalStorage.Response model msg)


{-| A `Handler` tags a function to handle responses from one funnel module.

Add a tag in this type for each funnel module you use.

-}
type Handler model msg
    = LocalStorageHandler (LocalStorage.Response -> State -> model -> ( model, Cmd msg ))


{-| This packages up everything necessary to dispatch for each module.

Add a clause for each funnel module you use.

-}
handlerToFunnel : Handler model msg -> ( String, Funnel model msg )
handlerToFunnel handler =
    case handler of
        LocalStorageHandler localStorageHandler ->
            ( LocalStorage.moduleName
            , FunnelSpec localStorageAccessors LocalStorage.moduleDesc LocalStorage.commander localStorageHandler
                |> LocalStorageFunnel
            )


{-| Add a tuple to this list for each funnel module you use.
-}
simulatedPortDict : Dict String ((Value -> msg) -> Value -> Cmd msg)
simulatedPortDict =
    Dict.fromList
        [ ( LocalStorage.moduleName, LocalStorage.makeSimulatedCmdPort )
        ]


{-| This is called from `AppFunnel.processValue`.

It unboxes the `Funnel` arg, and calls `PortFunnel.appProcess`.

-}
appTrampoline : (String -> model -> (Value -> Cmd msg)) -> GenericMessage -> Funnel model msg -> State -> model -> Result String ( model, Cmd msg )
appTrampoline portGetter genericMessage funnel state model =
    -- Dispatch on the `Funnel` tag.
    -- This example has only one possibility.
    case funnel of
        LocalStorageFunnel appFunnel ->
            PortFunnel.appProcess (portGetter LocalStorage.moduleName model)
                genericMessage
                appFunnel
                state
                model


{-| Here are the two ports used to communicate with all the backend JavaScript.

You can name them something besides `cmdPort` and `subPort`,
but then you have to change the call to `PortFunnel.subscribe()`
in `site/index.html`.

If you run the application in `elm reactor`, these will go nowhere.

-}
port cmdPort : Value -> Cmd msg


port subPort : (Value -> msg) -> Sub msg


{-| Create a subscription for the `subPort`, given a Msg wrapper.
-}
subscriptions : (Value -> msg) -> model -> Sub msg
subscriptions process model =
    subPort process


{-| Turn the `moduleName` inside a `GenericMessage` into the output port.

    getCmdPort tagger moduleName useSimulator

`tagger` is the same `Msg` that processes input from the subscription port.

`moduleName` will be ignored if `useSimulator` is `False`.

-}
getCmdPort : (Value -> msg) -> String -> Bool -> (Value -> Cmd msg)
getCmdPort tagger moduleName useSimulator =
    if not useSimulator then
        cmdPort

    else
        case Dict.get moduleName simulatedPortDict of
            Just makeSimulatedCmdPort ->
                makeSimulatedCmdPort tagger

            Nothing ->
                cmdPort


{-| A `Dict` that maps a module name to a concretized `FunnelSpec`.

Create one with `makeFunnelDict`. Pass it to `processValue`.

-}
type alias FunnelDict model msg =
    ( Dict String (Funnel model msg), String -> model -> (Value -> Cmd msg) )


{-| Make a `Dict` mapping `moduleName` to tagged concrete `FunnelSpec`.

The `Handler` list is a list of all of your handlers. E.g. for this example, it would be:

    makeFunnelDict
        [ LocalStorageHandler localStorageHandler
        ]
        getCmdPort

Where `echoHandler` and `addXYHandler` are functions in your main application module to handle responses.

-}
makeFunnelDict : List (Handler model msg) -> (String -> model -> (Value -> Cmd msg)) -> FunnelDict model msg
makeFunnelDict handlers portGetter =
    ( List.map handlerToFunnel handlers |> Dict.fromList
    , portGetter
    )


{-| Process a value coming in through the `subPort`.

The `FunnelDict` is the result of calling `makeFunnelDict`.

-}
processValue : FunnelDict model msg -> Value -> State -> model -> Result String ( model, Cmd msg )
processValue ( funnelDict, portGetter ) value state model =
    PortFunnel.processValue funnelDict (appTrampoline portGetter) value state model
