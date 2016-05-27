-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module ExprPartInput exposing (Model, init, Msg, update, view)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Combine exposing (Parser)
import Combine.Infix exposing (..)
import Ast.BinOp
import Ast.Expression as Ast
import Ast.Helpers


-- MODEL


type alias Model =
    { prefix : String
    , suffix : String
    , errors : List String
    }


init : String -> Model
init prefix =
    Model prefix "" (prefix |> parseExpression |> snd)



-- UPDATE


type Msg
    = NewSuffix String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewSuffix suffix ->
            { model
                | suffix = suffix
                , errors = (model.prefix ++ suffix) |> parseExpression |> snd
            }


parseExpression : String -> ( Maybe Ast.Expression, List String )
parseExpression input =
    case Combine.parse parser input |> Debug.log "parse result" |> fst of
        Ok expr ->
            ( Just expr, [] )

        Err errors ->
            ( Nothing, errors )


parser : Parser Ast.Expression
parser =
    let
        expr =
            Ast.expression Ast.BinOp.operators
    in
        expr <* Ast.Helpers.whitespace <* Combine.end



-- VIEW


view : Model -> Html Msg
view model =
    H.div
        [ HA.style
            [ (,) "font-family" "monospace"
            , (,) "font-size" "11pt"
            ]
        ]
        [ inputWithPrefix (List.isEmpty model.errors) model.prefix
        , errorList model.errors
        ]


inputWithPrefix : Bool -> String -> Html Msg
inputWithPrefix noErrors prefix =
    H.div
        [ HA.style
            [ (,) "display" "flex"
            , (,) "align-items" "center"
            ]
        ]
        [ H.span
            [ HA.style
                ([ (,) "margin" "10px" ]
                    ++ if noErrors then
                        []
                       else
                        [ (,) "color" "red" ]
                )
            ]
            [ H.text prefix ]
        , H.input
            [ HE.onInput NewSuffix
            , HA.style
                [ (,) "flex" "1 0 auto"
                , (,) "margin" "10px"
                ]
            ]
            []
        ]


errorList : List String -> Html msg
errorList errors =
    H.div
        [ HA.style
            [ (,) "padding" "10px"
            ]
        ]
        (if List.isEmpty errors then
            [ H.p [] [ H.text "No errors" ] ]
         else
            [ H.ol [] <| List.map (H.li [] << flip (::) [] << H.text) errors ]
        )
