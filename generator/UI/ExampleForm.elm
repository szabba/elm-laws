-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module UI.ExampleForm exposing (Model, init, Msg, update, view)

import Html as H exposing (Html)
import Html.App as App
import Html.Attributes as HA
import List.Extra as List
import UI.ExprPartInput as ExprPartInput


-- MODEL


type alias Model =
    { dslExpr : ExprPartInput.Model
    , translation : ExprPartInput.Model
    }


init : Model
init =
    { dslExpr = ExprPartInput.init "claim d |> "
    , translation = ExprPartInput.init "claim d "
    }



-- UPDATE


type Msg
    = DslExpr ExprPartInput.Msg
    | Translation ExprPartInput.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        DslExpr msg ->
            { model | dslExpr = ExprPartInput.update msg model.dslExpr }

        Translation msg ->
            { model | translation = ExprPartInput.update msg model.translation }



-- VIEW


view :
    ({ dslExample : String, translation : String } -> List String)
    -> Model
    -> Html Msg
view getErrors model =
    H.div []
        [ model.dslExpr |> ExprPartInput.view |> App.map DslExpr
        , model.translation |> ExprPartInput.view |> App.map Translation
        , errorList
            <| getErrors
            <| { dslExample = model.dslExpr |> ExprPartInput.text
               , translation = model.translation |> ExprPartInput.text
               }
        ]


viewInput : (ExprPartInput.Msg -> Msg) -> ExprPartInput.Model -> Html Msg
viewInput wrapMsg inputModel =
    inputModel
        |> ExprPartInput.view
        |> App.map wrapMsg


errorList : List String -> Html msg
errorList errors =
    H.div
        [ HA.style [ (,) "padding" "10px" ]
        ]
        (if List.isEmpty errors then
            [ H.p [] [ H.text "No errors" ] ]
         else
            [ H.ol [] <| List.map (H.li [] << List.singleton << H.text) errors ]
        )
