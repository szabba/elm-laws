-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Main exposing (..)

import Html as H exposing (Html)
import Html.App as App
import Html.Attributes as HA
import Html.Events as HE
import ExprPartInput


main : Program Never
main =
    App.beginnerProgram
        { model = ExprPartInput.init "claim d |> "
        , update = ExprPartInput.update
        , view = ExprPartInput.view
        }
