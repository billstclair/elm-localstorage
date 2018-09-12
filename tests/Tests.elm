module Tests exposing (all)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Json.Decode as JD
import Json.Encode as JE
import PortFunnel.LocalStorage as LS exposing (Message(..), decode, encode)
import Test exposing (..)


{-| This runs all of your tests.

Each line is for a different result type.

-}
all : Test
all =
    Test.concat <|
        List.concat
            [ List.map encodeDecodeTest encodeDecodeData
            ]


expectResult : Result String a -> Result String a -> Expectation
expectResult sb was =
    case was of
        Err err ->
            case sb of
                Err _ ->
                    Expect.true "You shouldn't ever see this." True

                Ok _ ->
                    Expect.false err True

        Ok wasv ->
            case sb of
                Err _ ->
                    Expect.false "Expected an error but didn't get one." True

                Ok sbv ->
                    Expect.equal sbv wasv


doTest : ( String, a, a ) -> Test
doTest ( name, was, sb ) =
    test name
        (\_ ->
            expectResult (Ok sb) (Ok was)
        )


encodeDecodeTest : Message -> Test
encodeDecodeTest message =
    test (LS.toString message)
        (\_ ->
            let
                genericMessage =
                    encode message
            in
            expectResult (Ok message) <| decode genericMessage
        )


encodeDecodeData : List Message
encodeDecodeData =
    [ Startup
    , Get "foo"
    , Put "bar" Nothing
    , Put "bletch" (Just <| JE.int 10)
    , ListKeys "gronk"
    , Keys "gronk" [ "gronk1", "gronk2", "gronk3" ]
    , Clear "skin"
    , SimulateGet "yep"
    , SimulatePut "yep" (Just <| JE.string "froboz")
    , SimulateListKeys "Hurrah!"
    , SimulateClear "whew"
    ]
