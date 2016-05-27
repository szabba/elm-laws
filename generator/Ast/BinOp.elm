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


module Ast.BinOp exposing (Assoc(..), OpTable, operators)

{-| This module exposes functions and types for working with operator
precedence tables.

# Types
@docs Assoc, OpTable

# Misc
@docs operators

-}

import Dict exposing (Dict)
import Ast.Helpers exposing (Name)


{-| FIXME
-}
type Assoc
    = N
    | L
    | R


{-| FIXME
-}
type alias OpTable =
    Dict Name ( Assoc, Int )


{-| The default operator precedence table.
-}
operators : OpTable
operators =
    Dict.empty
        |> Dict.insert "||" ( L, 2 )
        |> Dict.insert "&&" ( L, 3 )
        |> Dict.insert "==" ( L, 4 )
        |> Dict.insert "/=" ( L, 4 )
        |> Dict.insert "<" ( L, 4 )
        |> Dict.insert ">" ( L, 4 )
        |> Dict.insert ">=" ( L, 4 )
        |> Dict.insert "<=" ( L, 4 )
        |> Dict.insert "++" ( L, 5 )
        |> Dict.insert "+" ( L, 6 )
        |> Dict.insert "-" ( L, 6 )
        |> Dict.insert "*" ( L, 7 )
        |> Dict.insert "/" ( L, 7 )
        |> Dict.insert "%" ( L, 7 )
        |> Dict.insert "//" ( L, 7 )
        |> Dict.insert "rem" ( L, 7 )
        |> Dict.insert "^" ( L, 8 )
        |> Dict.insert "<<" ( L, 9 )
        |> Dict.insert ">>" ( L, 9 )
        |> Dict.insert "<|" ( R, 0 )
        |> Dict.insert "|>" ( R, 0 )
