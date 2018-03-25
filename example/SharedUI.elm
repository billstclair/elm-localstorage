----------------------------------------------------------------------
--
-- SharedUI.elm
-- LocalStorage example
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module SharedUI exposing (Model, Msg(..), getPorts, init, update, view)

import Debug exposing (log)
import Html exposing (Html, button, div, h2, input, p, table, td, text, tr)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Encode as JE
import LocalStorage exposing (LocalStorage, clear, getItem, setItem, setPorts)
import LocalStorage.SharedTypes exposing (Key, Operation(..), Ports, Value)


type alias Model =
    { key : Key
    , value : String
    , storage : LocalStorage Msg
    }


getPorts : Model -> Ports Msg
getPorts model =
    LocalStorage.getPorts model.storage


prefix : String
prefix =
    "example"


type Msg
    = SetKey String
    | SetValue String
    | GetItem
    | SetItem
    | UpdatePorts Operation (Maybe (Ports Msg)) Key Value


{-| Still need to decode `initialModel`
-}
init : Value -> Ports Msg -> ( Model, Cmd Msg )
init initialModel ports =
    { key = "key"
    , value = "value"
    , storage = LocalStorage.make ports prefix
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetKey key ->
            { model | key = key } ! []

        SetValue value ->
            { model | value = value } ! []

        GetItem ->
            model ! [ getItem model.storage model.key ]

        SetItem ->
            model ! [ setItem model.storage model.key (JE.string model.value) ]

        UpdatePorts operation ports key value ->
            let
                mdl =
                    case operation of
                        GetItemOperation ->
                            let
                                val =
                                    decodeValue value
                            in
                            { model | value = val }

                        _ ->
                            model
            in
            { mdl
                | storage =
                    case ports of
                        Nothing ->
                            model.storage

                        Just ps ->
                            setPorts ps model.storage
            }
                ! []


valueDecoder : JD.Decoder String
valueDecoder =
    JD.oneOf
        [ JD.string
        , JD.null "null"
        ]


decodeValue : Value -> String
decodeValue value =
    case JD.decodeValue valueDecoder value of
        Ok res ->
            res

        Err err ->
            err


br : Html msg
br =
    Html.br [] []


b : String -> Html msg
b string =
    Html.b [] [ text string ]


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "LocalStorage Example: " ]
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
                    [ td [] []
                    , td []
                        [ button [ onClick GetItem ]
                            [ text "Get" ]
                        , button [ onClick SetItem ]
                            [ text "Set" ]
                        ]
                    ]
                ]
            ]
        ]
