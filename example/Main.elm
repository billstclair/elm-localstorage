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


module Main exposing (main)

import Browser
import Cmd.Extra exposing (addCmd, addCmds, withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h2, input, p, span, table, td, text, tr)
import Html.Attributes exposing (href, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import PortFunnel.LocalStorage as LocalStorage
    exposing
        ( Key
        , Message
        , Response(..)
        )
import PortFunnels exposing (FunnelDict, Handler(..))


type alias Model =
    { key : Key
    , value : String
    , label : String
    , returnLabel : String
    , keysString : String
    , useSimulator : Bool
    , wasLoaded : Bool
    , funnelState : PortFunnels.State
    , error : Maybe String
    }


prefix : String
prefix =
    "example"


type Msg
    = SetKey String
    | SetValue String
    | SetLabel String
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
        , subscriptions = PortFunnels.subscriptions Process
        }


{-| Still need to decode `initialModel`
-}
init : () -> ( Model, Cmd Msg )
init () =
    { key = "key"
    , value = ""
    , label = ""
    , returnLabel = ""
    , keysString = ""
    , useSimulator = True
    , wasLoaded = False
    , funnelState = PortFunnels.initialState prefix
    , error = Nothing
    }
        |> withNoCmd


doIsLoaded : Model -> Model
doIsLoaded model =
    if not model.wasLoaded && LocalStorage.isLoaded model.funnelState.storage then
        { model
            | useSimulator = False
            , wasLoaded = True
        }

    else
        model


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state mdl =
    let
        model =
            doIsLoaded
                { mdl | funnelState = state }
    in
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            let
                string =
                    case value of
                        Nothing ->
                            "<null>"

                        Just v ->
                            decodeString v
            in
            { model
                | key = key
                , value = string
                , returnLabel =
                    case label of
                        Nothing ->
                            "Nothing"

                        Just lab ->
                            "Just \"" ++ lab ++ "\""
            }
                |> withNoCmd

        LocalStorage.ListKeysResponse { label, keys } ->
            let
                keysString =
                    stringListToString keys
            in
            { model
                | keysString = keysString
                , returnLabel =
                    case label of
                        Nothing ->
                            "Nothing"

                        Just lab ->
                            "Just \"" ++ lab ++ "\""
            }
                |> withNoCmd

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

        SetLabel label ->
            { model | label = label } |> withNoCmd

        GetItem ->
            let
                message =
                    if model.label == "" then
                        LocalStorage.get model.key

                    else
                        LocalStorage.getLabeled model.label model.key
            in
            { model | returnLabel = "" }
                |> withCmd (send message model)

        SetItem ->
            { model | returnLabel = "" }
                |> withCmd
                    (send
                        (LocalStorage.put model.key
                            (Just <| JE.string model.value)
                        )
                        model
                    )

        ListKeys ->
            let
                message =
                    if model.label == "" then
                        LocalStorage.listKeys model.key

                    else
                        LocalStorage.listKeysLabeled model.label model.key
            in
            { model | returnLabel = "" }
                |> withCmd (send message model)

        RemoveItem ->
            { model | returnLabel = "" }
                |> withCmd
                    (send (LocalStorage.put model.key Nothing) model)

        Clear ->
            { model | returnLabel = "" }
                |> withCmd
                    (send (LocalStorage.clear model.key) model)

        Process value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    model.funnelState
                    model
            of
                Err error ->
                    { model | error = Just error } |> withNoCmd

                Ok res ->
                    res


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict [ LocalStorageHandler storageHandler ] getCmdPort


{-| Get a possibly simulated output port.
-}
getCmdPort : String -> Model -> (Value -> Cmd Msg)
getCmdPort moduleName model =
    PortFunnels.getCmdPort Process moduleName model.useSimulator


send : Message -> Model -> Cmd Msg
send message model =
    LocalStorage.send (getCmdPort LocalStorage.moduleName model)
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
        , case model.error of
            Nothing ->
                text ""

            Just err ->
                p [ style "color" "red" ]
                    [ text err ]
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
                    [ td [] [ b "Label: " ]
                    , td []
                        [ input
                            [ value model.label
                            , onInput SetLabel
                            ]
                            []
                        ]
                    ]
                , tr []
                    [ td [] [ b "Return Label: " ]
                    , td []
                        [ input
                            [ value model.returnLabel
                            , onInput SetLabel
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
                , br
                , text "If you enter a 'Label' for 'Get' or 'List', it will be returned in 'Return Label'"
                ]
            , a [ href "https://github.com/billstclair/elm-localstorage" ]
                [ text "Source at GitHub" ]
            ]
        ]
