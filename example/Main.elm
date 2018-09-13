----------------------------------------------------------------------
--
-- Main.elm
-- LocalStorage example
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


port module Main exposing (main)

import Browser
import Cmd.Extra exposing (addCmd, addCmds, withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h2, input, p, span, table, td, text, tr)
import Html.Attributes exposing (href, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import PortFunnel exposing (FunnelSpec, GenericMessage, ModuleDesc, StateAccessors)
import PortFunnel.LocalStorage as LocalStorage
    exposing
        ( Key
        , Message
        , Response(..)
        )


port cmdPort : Value -> Cmd msg


port subPort : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    subPort Process


simulatedCmdPort : Value -> Cmd Msg
simulatedCmdPort =
    LocalStorage.makeSimulatedCmdPort Process


getCmdPort : Model -> (Value -> Cmd Msg)
getCmdPort model =
    if model.useSimulator then
        simulatedCmdPort

    else
        cmdPort


type alias FunnelState =
    { storage : LocalStorage.State }


type alias Model =
    { key : Key
    , value : String
    , keysString : String
    , useSimulator : Bool
    , funnelState : FunnelState
    , error : Maybe String
    }


prefix : String
prefix =
    "example"


type Msg
    = SetKey String
    | SetValue String
    | GetItem
    | SetItem
    | ListKeys
    | RemoveItem
    | Clear
    | Process Value


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


{-| Still need to decode `initialModel`
-}
init : () -> ( Model, Cmd Msg )
init () =
    { key = "key"
    , value = ""
    , keysString = ""
    , useSimulator = True
    , funnelState = { storage = LocalStorage.initialState prefix }
    , error = Nothing
    }
        |> withNoCmd


storageAccessors : StateAccessors FunnelState LocalStorage.State
storageAccessors =
    StateAccessors .storage (\substate state -> { state | storage = substate })


type alias AppFunnel substate message response =
    FunnelSpec FunnelState substate message response Model Msg


type Funnel
    = StorageFunnel (AppFunnel LocalStorage.State LocalStorage.Message LocalStorage.Response)


funnels : Dict String Funnel
funnels =
    Dict.fromList
        [ ( LocalStorage.moduleName
          , StorageFunnel <|
                FunnelSpec storageAccessors
                    LocalStorage.moduleDesc
                    LocalStorage.commander
                    storageHandler
          )
        ]


storageHandler : LocalStorage.Response -> FunnelState -> Model -> ( Model, Cmd Msg )
storageHandler response state mdl =
    let
        model =
            { mdl | funnelState = state }
    in
    case response of
        LocalStorage.GetResponse { key, value } ->
            let
                string =
                    case value of
                        Nothing ->
                            "<null>"

                        Just v ->
                            decodeString v
            in
            { model | key = key, value = string } |> withNoCmd

        LocalStorage.ListKeysResponse { keys } ->
            let
                keysString =
                    stringListToString keys
            in
            { model | keysString = keysString } |> withNoCmd

        _ ->
            model |> withNoCmd


stringListToString : List String -> String
stringListToString list =
    let
        quoted =
            List.map (\s -> "\"" ++ s ++ "\"") list

        commas =
            List.intersperse ", " quoted
                |> String.concat
    in
    "[" ++ commas ++ "]"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg modl =
    let
        model =
            { modl | error = Nothing }
    in
    case msg of
        SetKey key ->
            { model | key = key } |> withNoCmd

        SetValue value ->
            { model | value = value } |> withNoCmd

        GetItem ->
            model
                |> withCmd
                    (send (LocalStorage.get model.key) model)

        SetItem ->
            model
                |> withCmd
                    (send
                        (LocalStorage.put model.key
                            (Just <| JE.string model.value)
                        )
                        model
                    )

        ListKeys ->
            model
                |> withCmd
                    (send (LocalStorage.listKeys model.key) model)

        RemoveItem ->
            model
                |> withCmd
                    (send (LocalStorage.put model.key Nothing) model)

        Clear ->
            model
                |> withCmd
                    (send (LocalStorage.clear model.key) model)

        Process value ->
            case PortFunnel.decodeGenericMessage value of
                Err error ->
                    { model | error = Just error } |> withNoCmd

                Ok genericMessage ->
                    let
                        moduleName =
                            genericMessage.moduleName
                    in
                    case Dict.get moduleName funnels of
                        Just funnel ->
                            case funnel of
                                StorageFunnel appFunnel ->
                                    let
                                        ( mdl, cmd ) =
                                            process genericMessage appFunnel model
                                    in
                                    if
                                        mdl.useSimulator
                                            && LocalStorage.isLoaded
                                                mdl.funnelState.storage
                                    then
                                        { mdl | useSimulator = False }
                                            |> withCmd cmd

                                    else
                                        mdl |> withCmd cmd

                        _ ->
                            { model
                                | error =
                                    Just <|
                                        "Unknown moduleName: "
                                            ++ moduleName
                            }
                                |> withNoCmd


process : GenericMessage -> AppFunnel substate message response -> Model -> ( Model, Cmd Msg )
process genericMessage funnel model =
    case
        PortFunnel.appProcess (getCmdPort model)
            genericMessage
            funnel
            model.funnelState
            model
    of
        Err error ->
            { model | error = Just error } |> withNoCmd

        Ok ( model2, cmd ) ->
            model2 |> withCmd cmd


send : Message -> Model -> Cmd Msg
send message model =
    LocalStorage.send (getCmdPort model)
        message
        model.funnelState.storage


decodeString : Value -> String
decodeString value =
    case JD.decodeValue JD.string value of
        Ok res ->
            res

        Err err ->
            JD.errorToString err


br : Html msg
br =
    Html.br [] []


b : String -> Html msg
b string =
    Html.b [] [ text string ]


ps : List (Html msg) -> Html msg
ps paragraphs =
    List.map (\para -> p [] [ para ]) paragraphs
        |> div []


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "LocalStorage Example" ]
        , p []
            [ table []
                [ tr []
                    [ td [] [ b "Key: " ]
                    , td []
                        [ input
                            [ value model.key
                            , onInput SetKey
                            ]
                            []
                        ]
                    ]
                , tr []
                    [ td [] [ b "Value: " ]
                    , td []
                        [ input
                            [ value model.value
                            , onInput SetValue
                            ]
                            []
                        ]
                    ]
                , tr []
                    [ td [] [ b "Keys:" ]
                    , td []
                        [ text model.keysString ]
                    ]
                , tr []
                    [ td [] [ b "Simulated:" ]
                    , td []
                        [ text <|
                            if model.useSimulator then
                                "Yes"

                            else
                                "No"
                        ]
                    ]
                , tr []
                    [ td [] []
                    , td []
                        [ button [ onClick GetItem ]
                            [ text "Get" ]
                        , button [ onClick SetItem ]
                            [ text "Set" ]
                        , button [ onClick ListKeys ]
                            [ text "List" ]
                        , button [ onClick RemoveItem ]
                            [ text "Remove" ]
                        , button [ onClick Clear ]
                            [ text "Clear" ]
                        ]
                    ]
                ]
            ]
        , ps
            [ span []
                [ text "This is an example of the "
                , a [ href "http://package.elm-lang.org/packages/billstclair/elm-localstorage/latest" ]
                    [ text "billstclair/localstorage" ]
                , text " Elm package."
                ]
            , span []
                [ text "Enter a key and press 'Get' to fetch its value."
                , br
                , text "Enter a key and value and press 'Set' to set its value."
                , br
                , text "Enter a key and press 'Remove' to remove that value."
                , br
                , text "Enter a key and press 'List' to list all saved keys beginninng with that prefix."
                , br
                , text "Enter a key and press 'Clear' to remove all key/value pairs beginning with that prefix."
                ]
            , a [ href "https://github.com/billstclair/elm-localstorage" ]
                [ text "Source at GitHub" ]
            ]
        ]
