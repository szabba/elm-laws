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
    { getDslErrors : String -> List String
    , getTranslationErrors : String -> List String
    }
    -> Model
    -> Html Msg
view { getDslErrors, getTranslationErrors } model =
    let
        dslErrors =
            model.dslExpr
                |> ExprPartInput.text
                |> getDslErrors

        translationErrors =
            []
    in
        H.div []
            (viewInput DslExpr getDslErrors model.dslExpr
                ++ viewInput Translation getTranslationErrors model.translation
            )


viewInput : (ExprPartInput.Msg -> Msg) -> (String -> List String) -> ExprPartInput.Model -> List (Html Msg)
viewInput wrapMsg getErrors inputModel =
    let
        errors =
            inputModel |> ExprPartInput.text |> getErrors
    in
        [ inputModel
            |> ExprPartInput.view { hasErrors = not (List.isEmpty errors) }
            |> App.map wrapMsg
        , errorList errors
        ]


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
