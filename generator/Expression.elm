-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Expression exposing (Expression, parse)

import Combine exposing (Parser)
import Combine.Infix exposing (..)
import Ast.BinOp
import Ast.Expression as Ast
import Ast.Helpers


type alias Expression =
    Ast.Expression


parse : String -> Result (List String) Ast.Expression
parse src =
    src |> Combine.parse parser |> fst


parser : Parser Ast.Expression
parser =
    let
        expr =
            Ast.expression Ast.BinOp.operators
    in
        expr <* Ast.Helpers.whitespace <* Combine.end
