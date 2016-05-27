-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Main exposing (..)

import Html.App as App
import Example
import Expression
import UI.ExampleForm as ExampleForm


main : Program Never
main =
    App.beginnerProgram
        { model = ExampleForm.init
        , update = ExampleForm.update
        , view =
            ExampleForm.view
                { getDslErrors = getDslErrors
                , getTranslationErrors = getTranslationErrors
                }
        }


getDslErrors : String -> List String
getDslErrors text =
    case Expression.parse text `Result.andThen` Example.extractDslPatternParts of
        Err errors ->
            errors

        Ok _ ->
            []


getTranslationErrors : String -> List String
getTranslationErrors =
    always []
