-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Example exposing (Example, DslPatternPart, extractDslPatternParts)

import String
import Ast.Expression as Ast
import List.Extra as List


type alias Example =
    { dslExample : List (DslPatternPart)
    , translation : Ast.Expression
    }


type alias DslPatternPart =
    { dslKeyword : String
    , args : List Ast.Expression
    }


addArgumentToPatternPart : Ast.Expression -> DslPatternPart -> DslPatternPart
addArgumentToPatternPart arg part =
    { part | args = part.args ++ [ arg ] }


extractDslPatternParts : Ast.Expression -> Result (List String) (List DslPatternPart)
extractDslPatternParts expr =
    case expr of
        Ast.BinOp (Ast.Variable [ "|>" ]) (Ast.Application (Ast.Variable [ "claim" ]) (Ast.Variable [ "d" ])) rest ->
            extractDslPatternParts' rest

        _ ->
            Err [ "DSL pattern lacks the expected prefix `claim d |>`" ]


extractDslPatternParts' : Ast.Expression -> Result (List String) (List DslPatternPart)
extractDslPatternParts' expr =
    case expr of
        Ast.BinOp (Ast.Variable [ "|>" ]) first rest ->
            Result.map2 (++)
                (extractDslPatternPart first |> Result.map List.singleton)
                (extractDslPatternParts' rest)

        _ ->
            expr
                |> extractDslPatternPart
                |> Result.map List.singleton


extractDslPatternPart : Ast.Expression -> Result (List String) DslPatternPart
extractDslPatternPart expr =
    case expr of
        Ast.Variable [ dslKeyword ] ->
            DslPatternPart dslKeyword [] |> Ok

        Ast.Variable varParts ->
            let
                notDslKeyword =
                    varParts
                        |> List.intersperse "."
                        |> String.concat

                msg =
                    "`" ++ notDslKeyword ++ "` can't be used as a DSL keyword "
            in
                Err [ msg ]

        Ast.Application (Ast.Variable [ dslKeyword ]) arg ->
            DslPatternPart dslKeyword [ arg ]
                |> Ok

        Ast.Application innerApp arg ->
            innerApp
                |> extractDslPatternPart
                |> Result.map (addArgumentToPatternPart arg)

        _ ->
            Err [ "a |> should be followed by function application" ]