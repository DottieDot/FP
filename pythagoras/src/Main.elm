module Main exposing (..)

import Html exposing (text, div, span, br)
import Maybe exposing (withDefault)

type alias PythTriple = (Int, Int, Int)

sqr: Int -> Int
sqr num =
  num^2

isPythTripple: PythTriple -> Bool
isPythTripple (a, b, c) =
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

pythTriple: (Int, Int) -> Maybe PythTriple
pythTriple (x, y) = 
  if x > y then
    Just (
      withDefault 0 (leg1 x y),
      withDefault 0 (leg2 x y),
      withDefault 0 (hyp  x y)
    )
  else
    Nothing

pythTripleMap: List (Int, Int) -> List (Maybe PythTriple)
pythTripleMap list =
  List.map pythTriple list

pythTripleRec: List (Int, Int) -> List (Maybe PythTriple)
pythTripleRec list =
  case list of
    [] -> []
    x :: xs ->
      (pythTriple x) :: (pythTripleRec xs)

arePythTripleFilter: List PythTriple -> List PythTriple
arePythTripleFilter list =
  List.filter isPythTripple list

arePythTripleRec: List PythTriple -> List PythTriple
arePythTripleRec list =
  case list of
    [] -> []
    x :: xs ->
      if isPythTripple x then
        x :: (arePythTripleRec xs)
      else
        arePythTripleRec xs
  
boolToString: Bool -> String
boolToString bool =
  if bool then
    "True"
  else
    "False"

pythTripleToString: (Int, Int, Int) -> String
pythTripleToString (a, b, c) =
  "(" ++
  (String.fromInt a) ++ ", " ++
  (String.fromInt b) ++ ", " ++
  (String.fromInt c) ++ ")"

pythTripleListToString: List PythTriple -> String
pythTripleListToString list =
  list
    |> List.map pythTripleToString
    |> String.join ", "
  
maybePythTripleListToString: List (Maybe PythTriple) -> String
maybePythTripleListToString list =
  list
    |> List.filter (\e -> e /= Nothing)
    |> List.map (\e -> withDefault (0, 0, 0) e)
    |> List.map pythTripleToString
    |> String.join ", "

main = div [] [
    span[] [ text "isPythTriple 3 4 5" ],
    br  [] [],
    span[] [ text (boolToString (isPythTripple (3, 4, 5))) ],
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
    span[] [ text (pythTripleToString (withDefault (0, 0, 0) (pythTriple (5, 4)))) ],
    br  [] [],
    br  [] [],
    span[] [ text "pythTripleMap [(2, 1), (1, 2), (3, 1)]" ],
    br  [] [],
    span[] [ text (maybePythTripleListToString (pythTripleMap [(2, 1), (1, 2), (3, 1)])) ],
    br  [] [],
    br  [] [],
    span[] [ text "arePythTripleRec [(3, 4, 5), (1, 2, 3), (8, 6, 10)]" ],
    br  [] [],
    span[] [ text (pythTripleListToString (arePythTripleRec [(3, 4, 5), (1, 2, 3), (8, 6, 10)])) ]
  ]
