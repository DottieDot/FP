module Main exposing (..)
import Html exposing (text, div, br)
import Debug exposing (toString)

halve: List a -> (List a, List a)
halve list =
  case list of
    [] -> ([], [])
    x :: y :: xs ->
      let
        (a, b) = halve xs
      in
      (x :: a, y :: b)
    x :: _ -> 
      ([x], [])

merge: List comparable -> List comparable -> List comparable
merge l1 l2 =
  case (l1, l2) of
    ([], []) -> []
    (x :: xs, y :: ys) ->
      if x <= y then
        x :: merge xs (y :: ys)
      else
        y :: merge (x :: xs) ys
    (x, []) ->
      x
    ([], y) -> 
      y

mergeSort: List comparable -> List comparable
mergeSort list =
  case halve list of
    ([], []) -> []
    (l, []) -> l
    ([], r) -> r
    (l, r) ->
      merge (mergeSort l) (mergeSort r)

--------------------------

toSort : List Int
toSort = [4,6,1,5,9]

main = div []
  [ text ("To sort: " ++ toString toSort)
  , br [] []
  , text ("Sorted : " ++ toString (mergeSort toSort))
  ]
