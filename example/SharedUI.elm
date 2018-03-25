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


module SharedUI exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, div, h2, p, text)
import Html.Attributes exposing (style)
import Json.Encode as JE
import LocalStorage exposing (LocalStorage, clear, getItem, setItem, setPorts)
import LocalStorage.SharedTypes exposing (Key, Operation, Ports, Value)


type alias Model state =
    { hello : String
    , storage : LocalStorage state (Msg state)
    }


prefix : String
prefix =
    "example"


type Msg state
    = Hello String
    | UpdatePorts Operation (Ports state (Msg state)) ( Key, Value )


{-| Still need to decode `initialModel`
-}
init : Value -> Ports state (Msg state) -> ( Model state, Cmd (Msg state) )
init initialModel ports =
    { hello = "Hello, World!"
    , storage = LocalStorage.make ports prefix
    }
        ! []


update : Msg state -> Model state -> ( Model state, Cmd (Msg state) )
update msg model =
    case msg of
        UpdatePorts operation ports kvpair ->
            { model | storage = setPorts ports model.storage } ! []

        Hello msg ->
            { model | hello = msg } ! []


view : Model state -> Html (Msg state)
view model =
    div []
        [ h2 [] [ text "LocalStorage Example: " ]
        , p [] [ text <| model.hello ]
        ]
