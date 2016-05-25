-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Laws exposing (..)

{-| An aletrnative to the claim DSL from Elm Check. See the README for more
details.

@docs claim, suite

# Helpers
@docs equivalent, true, false, equal

-}

import Check as C exposing (Claim)
import Check.Producer as CP exposing (Producer)


{-| Creates a claim that groups other claims.

    import Laws exposing (suite, true, false)
    import Check.Producer exposing (unit)

    contradictory =
        suite "the world is full of contradictions"
            [ claim "of course"
                (true <| always True)
                unit
            , claim "of course NOT"
                (false <| always True)
                unit
            ]

-}
suite : String -> List Claim -> Claim
suite =
    C.suite


{-| Creates a claim given a string describing it, a pair of functions and a
producer of values. The two functions are applied to values obtained from the
producer.

    import Laws exposing (claim)
    import Check.Producer exposing (list int)

    notAppliedTwiceReturnsArgument =
        claim "not, applied twice, returns it's initial argument"
            (not << not, identity)
            (list int)

There are helpers around for creating these kinds of function pairs.

-}
claim : String -> ( a -> b, a -> b ) -> Producer a -> Claim
claim descr that prod =
    C.claim descr (fst that) (snd that) prod


{-| Helps test that the result of two functions is the same.

    import Laws exposing (claim, equivalent)
    import Check.Producer exposing (int)

    oneIsIdentityOfMultiplication =
        claim "1 is the identity of multiplication"
            ((*) 1 `equivalent` identity)
            int

-}
equivalent : (a -> b) -> (a -> b) -> ( a -> b, a -> b )
equivalent =
    (,)


{-| Helps test that a property is true.

    import Laws exposing (claim, true)
    import Check.Producer exposing (list, unit)

    halfAListIsNoMoreThanTheWhole =
        claim "half of a list is no longer than the entire list"
            (true <| \l -> List.length l >= List.length // 2)
            (list unit)

-}
true : (a -> Bool) -> ( a -> Bool, a -> Bool )
true f =
    ( f, always True )


{-| Helps test that a property is false.

    import Laws exposing (claim, true)
    import Check.Producer exposing (list, unit)

    empthyIsEmpty
        claim "half a list is longer than i's thole"
            (true <| \l -> List.length l < List.length // 2)
            (list unit)

-}
false : (a -> Bool) -> ( a -> Bool, a -> Bool )
false f =
    ( f, always False )


{-| Helps test value equality.

    import Laws exposing (claim, equal)
    import Check.Producer exposing (unit)

    oneMinusOneIsZero =
        claim "1 - 1 = 0"
            (1 - 1 `equal` 0)
            unit

-}
equal : a -> a -> ( b -> a, b -> a )
equal x y =
    ( always x, always y )
