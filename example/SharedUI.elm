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


module SharedUI exposing (Model, Msg(..), getPorts, init, prefix, update, view)

import Debug exposing (log)
import Html exposing (Html, a, button, div, h2, input, p, span, table, td, text, tr)
import Html.Attributes exposing (href, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Encode as JE
import LocalStorage
    exposing
        ( LocalStorage
        , clear
        , getItem
        , listKeys
        , setItem
        , setPorts
        )
import LocalStorage.SharedTypes
    exposing
        ( Key
        , Operation(..)
        , Ports
        , Value
        , decodeStringList
        )


type alias Model =
    { key : Key
    , value : String
    , keysString : String
    , result : String
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
    | ListKeys
    | RemoveItem
    | Clear
    | UpdatePorts Operation (Maybe (Ports Msg)) Key Value


{-| Still need to decode `initialModel`
-}
init : Value -> Ports Msg -> ( Model, Cmd Msg )
init initialModel ports =
    { key = "key"
    , value = ""
    , result = ""
    , keysString = ""
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

        ListKeys ->
            model ! [ listKeys model.storage model.key ]

        RemoveItem ->
            model ! [ setItem model.storage model.key JE.null ]

        Clear ->
            model ! [ clear model.storage ]

        UpdatePorts operation ports key value ->
            let
                ( mdl, v ) =
                    case operation of
                        GetItemOperation ->
                            let
                                val =
                                    decodeValue value
                            in
                            ( { model | value = val }, val )

                        ListKeysOperation ->
                            let
                                keysString =
                                    case decodeStringList value of
                                        Err msg ->
                                            msg

                                        Ok keys ->
                                            toString keys
                            in
                            ( { model | keysString = keysString }, keysString )

                        _ ->
                            ( model, "" )
            in
            { mdl
                | storage =
                    case ports of
                        Nothing ->
                            model.storage

                        Just ps ->
                            setPorts ps model.storage
                , result =
                    case operation of
                        ListKeysOperation ->
                            toString
                                { operation = operation
                                , prefix = key
                                }

                        _ ->
                            toString
                                { operation = operation
                                , key = key
                                }
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
                    [ td [] [ b "Result:" ]
                    , td []
                        [ text model.result ]
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
