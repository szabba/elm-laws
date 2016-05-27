-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module UI.ExprPartInput exposing (Model, init, text, Msg, update, view)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


-- MODEL


type alias Model =
    { prefix : String
    , suffix : String
    }


init : String -> Model
init prefix =
    Model prefix ""


text : Model -> String
text { prefix, suffix } =
    prefix ++ suffix



-- UPDATE


type Msg
    = NewSuffix String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewSuffix suffix ->
            { model | suffix = suffix }



-- VIEW


view : { hasErrors : Bool } -> Model -> Html Msg
view { hasErrors } model =
    H.div
        [ HA.style
            [ (,) "font-family" "monospace"
            , (,) "font-size" "11pt"
            , (,) "display" "flex"
            , (,) "align-items" "center"
            ]
        ]
        [ H.span
            [ HA.style
                ([ (,) "margin" "10px" ]
                    ++ if hasErrors then
                        []
                       else
                        [ (,) "color" "red" ]
                )
            ]
            [ H.text model.prefix ]
        , H.input
            [ HE.onInput NewSuffix
            , HA.style
                [ (,) "flex" "1 0 auto"
                , (,) "margin" "10px"
                ]
            ]
            []
        ]
