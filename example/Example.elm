----------------------------------------------------------------------
--
-- Example.elm
-- LocalStorage example
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Example exposing (..)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (style)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { hello : String
    }


type Msg
    = Hello String


init : ( Model, Cmd Msg )
init =
    { hello = "Hello, World!"
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Hello msg ->
            { model | hello = msg } ! []


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "LocalStorage Example: " ]
        , p [] [ text <| model.hello ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
