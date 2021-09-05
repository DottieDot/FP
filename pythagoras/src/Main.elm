module Main exposing (..)

import Html exposing (text, div, span, br)
import Maybe exposing (withDefault)

sqr: Int -> Int
sqr num =
  num^2

isPythTripple: Int -> Int -> Int -> Bool
isPythTripple a b c =
  if (sqr a) + (sqr b) == sqr c then
    True
  else
    False

leg1: Int -> Int -> Maybe Int
leg1 x y =
  if x > y then
    Just (x^2 - y^2)
  else
    Nothing

leg2: Int -> Int -> Maybe Int
leg2 x y =
  if x > y then
    Just (2 * x * y)
  else
    Nothing

hyp: Int -> Int -> Maybe Int
hyp x y =
  if x > y then
    Just (x^2 + y^2)
  else
    Nothing

pythTriple: (Int, Int) -> Maybe (Int, Int, Int)
pythTriple (x, y) = 
  if x > y then
    Just (
      withDefault 0 (leg1 x y),
      withDefault 0 (leg2 x y),
      withDefault 0 (hyp  x y)
    )
  else
    Nothing

boolToString: Bool -> String
boolToString bool =
  if bool then
    "True"
  else
    "False"

tuple3ToString: (Int, Int, Int) -> String
tuple3ToString (a, b, c) = 
  (String.fromInt a) ++ " " ++
  (String.fromInt b) ++ " " ++
  (String.fromInt c) ++ " "

main = div [] [
    span[] [ text "isPythTriple 3 4 5" ],
    br  [] [],
    span[] [ text (boolToString (isPythTripple 3 4 5)) ],
    br  [] [],
    br  [] [],
    span[] [ text "leg1 5 4" ],
    br  [] [],
    span[] [ text (String.fromInt (withDefault 0 (leg1 5 4))) ],
    br  [] [],
    br  [] [],
    span[] [ text "leg2 5 4" ],
    br  [] [],
    span[] [ text (String.fromInt (withDefault 0 (leg2 5 2))) ],
    br  [] [],
    br  [] [],
    span[] [ text "hyp 5 4" ],
    br  [] [],
    span[] [ text (String.fromInt (withDefault 0 (hyp 5 4))) ],
    br  [] [],
    br  [] [],
    span[] [ text "pythTriple (5, 4)" ],
    br  [] [],
    span[] [ text (tuple3ToString (withDefault (0, 0, 0) (pythTriple (5, 4)))) ]
  ]
