module Main exposing (..)
import Html exposing (text, div, br)
import Debug exposing (toString)
import Maybe exposing (andThen)

repeatUntil : (a -> Bool) -> (a -> a) -> a -> a
repeatUntil check fn init =
  if not (check init) then
    repeatUntil check fn (fn init)
  else
    init

repeatUntilList : (a -> Bool) -> (a -> a) -> List a -> Maybe (List a)
repeatUntilList check fn init =
  List.head init
    |> andThen (\x ->
      if not (check x) then
        repeatUntilList check fn ((fn x) :: init)
      else
        Just (init))
  
above100 : Int -> Bool 
above100 x = x > 100 
double : Int -> Int 
double x = x * 2

aboveN : Int -> Int -> Bool
aboveN limit x =
  x > limit

even : Int -> Bool
even x =
  modBy 2 x == 0

collatz : Int -> Int
collatz init =
  if even init then 
    init//2
  else 
    3*init+1

print : String -> a -> Html.Html msg
print str val = 
  div [] 
    [ text str
    , br[] []
    , text (toString val)
    , br[] []]

main = div []
  [ print "repeatUntil above100 double 7:" (repeatUntil above100 double 7)
  , print "repeatUntil above100 ((+) 1) 42" (repeatUntil above100 ((+) 1) 42)
  , print "repeatUntil (aboveN 1000) double 7:" (repeatUntil (aboveN 1000) double 7)
  , print "repeatUntil ((==) 1) collatz 19" (repeatUntil ((==)1) collatz 19)
  , print "repeatUntilList ((==) 1) collatz [19]" (repeatUntilList ((==) 1) collatz [19])
  ]
