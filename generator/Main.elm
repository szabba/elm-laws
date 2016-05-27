-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Main exposing (..)

import Html as H exposing (Html)
import Html.App as App
import Html.Attributes as HA
import Html.Events as HE


main : Program Never
main =
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { pattern : String
    , result : String
    }


init : Model
init =
    { pattern = ""
    , result = ""
    }



-- UPDATE


type Msg
    = NewPattern String
    | NewResult String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewPattern pattern ->
            { model | pattern = pattern }

        NewResult result ->
            { model | result = result }



-- VIEW


view : Model -> Html Msg
view model =
    H.div
        [ HA.style
            [ (,) "font-family" "monospace"
            , (,) "font-size" "11pt"
            ]
        ]
        [ inputWithPrefix "claim d |> " NewPattern
        , inputWithPrefix "claim d " NewResult
        ]


inputWithPrefix : String -> (String -> msg) -> Html msg
inputWithPrefix prefix toMsg =
    H.div
        [ HA.style
            [ (,) "display" "flex"
            , (,) "align-items" "center"
            ]
        ]
        [ H.span
            [ HA.style [ (,) "margin" "10px" ]
            ]
            [ H.text prefix ]
        , H.input
            [ HE.onInput ((++) prefix >> toMsg)
            , HA.style
                [ (,) "flex" "1 0 auto"
                , (,) "margin" "10px"
                ]
            ]
            []
        ]
