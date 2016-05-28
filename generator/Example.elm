-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Example exposing (Example, DslPatternPart, parse)

import String
import List.Extra as List
import Ast.Expression as Ast
import Expression


type alias Example =
    { dslExample : List (DslPatternPart)
    , translation : Ast.Expression
    }


type alias DslPatternPart =
    { dslKeyword : String
    , args : Maybe Ast.Expression
    }


parse : { dslExample : String, translation : String } -> Result (List String) Example
parse { dslExample, translation } =
    Result.map2 Example
        (Expression.parse dslExample
            |> Result.formatError (prefixErrorsWith "DSL example")
            |> flip Result.andThen extractDslPatternParts
        )
        (Expression.parse translation
            |> Result.formatError (prefixErrorsWith "DSL example translation")
        )


prefixErrorsWith : String -> List String -> List String
prefixErrorsWith prefix =
    List.map ((++) prefix << (++) ": ")


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
            DslPatternPart dslKeyword Nothing |> Ok

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
            DslPatternPart dslKeyword (Just arg)
                |> Ok

        Ast.Application innerApp arg ->
            Err [ "a DSL keyword must have at most one argument " ]

        _ ->
            Err [ "a |> should be followed by function application" ]
