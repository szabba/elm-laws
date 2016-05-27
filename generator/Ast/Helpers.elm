-- Copyright (c) 2016, Bogdan Paul Popa
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the <organization> nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


module Ast.Helpers exposing (..)

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import String


type alias Name =
    String


type alias QualifiedType =
    List Name


type alias ModuleName =
    List String


type alias Alias =
    String


sequence : List (Parser res) -> Parser (List res)
sequence ps =
    let
        accumulate acc ps cx =
            case ps of
                [] ->
                    ( Ok (List.reverse acc), cx )

                p :: ps ->
                    case app p cx of
                        ( Ok res, rcx ) ->
                            accumulate (res :: acc) ps rcx

                        ( Err ms, ecx ) ->
                            ( Err ms, ecx )
    in
        primitive
            <| \cx ->
                accumulate [] ps cx


reserved : List Name
reserved =
    [ "module"
    , "where"
    , "import"
    , "as"
    , "exposing"
    , "type"
    , "alias"
    , "port"
    , "if"
    , "then"
    , "else"
    , "let"
    , "in"
    , "case"
    , "of"
    ]


reservedOperators : List Name
reservedOperators =
    [ "=", ".", "..", "->", "--", "|", ":" ]


between' : Parser a -> Parser res -> Parser res
between' p =
    between p p


whitespace : Parser String
whitespace =
    regex "[ \x0D\t\n]*"


spaces : Parser String
spaces =
    regex "[ \t]*"


spaces' : Parser String
spaces' =
    regex "[ \t]+"


symbol : String -> Parser String
symbol k =
    between' whitespace (string k)


initialSymbol : String -> Parser String
initialSymbol k =
    string k <* spaces


commaSeparated : Parser res -> Parser (List res)
commaSeparated p =
    sepBy1 (string ",") (between' whitespace p)


commaSeparated' : Parser res -> Parser (List res)
commaSeparated' p =
    sepBy (string ",") (between' whitespace p)


name : Parser Char -> Parser String
name p =
    String.cons <$> p <*> regex "[a-zA-Z0-9-_']*"


loName : Parser String
loName =
    name lower
        `andThen` \n ->
                    if List.member n reserved then
                        fail [ "name '" ++ n ++ "' is reserved" ]
                    else
                        succeed n


upName : Parser String
upName =
    name upper


operator : Parser String
operator =
    between' (string "`") loName <|> symbolicOperator


symbolicOperator : Parser String
symbolicOperator =
    regex "[+-/*=.$<>:&|^?%#@~!]+"
        `andThen` \n ->
                    if List.member n reservedOperators then
                        fail [ "operator '" ++ n ++ "' is reserved" ]
                    else
                        succeed n


functionName : Parser String
functionName =
    loName


moduleName : Parser ModuleName
moduleName =
    between' spaces <| sepBy1 (string ".") upName
