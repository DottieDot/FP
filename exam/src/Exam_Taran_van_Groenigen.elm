module Exam_Taran_van_Groenigen exposing (..)

import Html exposing (text, div, hr, br)
import Debug exposing (toString)

insert : comparable -> List comparable -> List comparable
insert value list =
  case list of
    [] -> [value]
    x :: xs ->
      if value <= x then
        value :: list
      else
        x :: (insert value xs)

insertionSort : List comparable -> List comparable -> List comparable
insertionSort unsorted sorted =
  case unsorted of
    [] -> sorted
    x :: xs ->
      insertionSort xs (insert x sorted)

posProduct : List (Int, Int) -> List Int
posProduct list =
  case list of
    [] -> []
    (a, b) :: xs ->
      let
        product = a * b
      in
      -- In the example 0 isn't considered positive
      if product > 0 then
        product :: posProduct xs
      else
        posProduct xs

type Expression 
  = Plus  Expression Expression  
  | Minus Expression Expression  
  | Mult  Expression Expression  
  | Fact  Expression  
  | Val   Int

factorial : Int -> Int
factorial x =
  if x <= 0 then
    1
  else
    x * (factorial(x - 1))

expressionToString : Expression -> String
expressionToString expr =
  case expr of
    Plus  lvalue rvalue -> "(" ++ (expressionToString lvalue) ++ " + " ++ (expressionToString rvalue) ++ ")"
    Minus lvalue rvalue -> "(" ++ (expressionToString lvalue) ++ " - " ++ (expressionToString rvalue) ++ ")"
    Mult  lvalue rvalue -> "(" ++ (expressionToString lvalue) ++ " * " ++ (expressionToString rvalue) ++ ")"

    Fact value -> (expressionToString value) ++ "!"
    Val  value -> String.fromInt(value)

evalExpression : Expression -> Int
evalExpression expr =
  case expr of
    Plus  lvalue rvalue -> (evalExpression lvalue) + (evalExpression rvalue)
    Minus lvalue rvalue -> (evalExpression lvalue) - (evalExpression rvalue)
    Mult  lvalue rvalue -> (evalExpression lvalue) * (evalExpression rvalue)

    Fact value -> factorial (evalExpression value)
    Val  value -> value

simplifyExpression : Expression -> Expression
simplifyExpression expr =
  let
    simplified =
      case expr of
        Plus  lvalue  (Val 0) -> simplifyExpression lvalue
        Plus  (Val 0) rvalue  -> simplifyExpression rvalue
        Minus lvalue  (Val 0) -> simplifyExpression lvalue
        Minus (Val 0) rvalue  -> simplifyExpression rvalue
        Mult  lvalue  (Val 1) -> simplifyExpression lvalue
        Mult  (Val 1) rvalue  -> simplifyExpression rvalue

        -- Not described in the test
        Mult  _       (Val 0) -> Val 0
        Mult  (Val 0) _       -> Val 0
        
        Plus  lvalue rvalue -> Plus  (simplifyExpression lvalue) (simplifyExpression rvalue)
        Minus lvalue rvalue -> Minus (simplifyExpression lvalue) (simplifyExpression rvalue)
        Mult  lvalue rvalue -> Mult  (simplifyExpression lvalue) (simplifyExpression rvalue)

        Fact value -> Fact (simplifyExpression value)
        Val  value -> Val value
  in
  if simplified /= expr then
    simplifyExpression simplified
  else
    expr

e0 = Fact (Val 5)
e1 = Mult (Plus e0 (Val -110)) (Val 3) 

main = div []
  [ text "insert 4 [1,3,5,7,9]"
  , br [] []
  , text (toString (insert 4 [1,3,5,7,9]))
  , br [] []
  , hr [] []
  , text "insertionSort [3,7,4,9,1] []"
  , br [] []
  , text (toString (insertionSort [3,7,4,9,1] []))
  , br [] []
  , hr [] []
  , text "posProduct [ (3,-4),(2,5),(7,0),(-1,-9) ]"
  , br [] []
  , text (toString (posProduct [ (3,-4),(2,5),(7,0),(-1,-9) ]))
  , br [] []
  , hr [] []
  , text (expressionToString e0)
  , br [] []
  , text (toString (evalExpression e0))
  , br [] []
  , text (expressionToString e1)
  , br [] []
  , text (toString (evalExpression e1))
  , br [] []
  , hr [] []
  , text "simplifyExpression (Mult (Val 42) (Minus (Val 73) (Minus (Mult (Plus (Val 0) (Val 0)) (Val 1)) (Val 0))))"
  , br [] []
  , text (expressionToString (simplifyExpression (Mult (Val 42) (Minus (Val 73) (Minus (Mult (Plus (Val 0) (Val 0)) (Val 1)) (Val 0))))))
  ]
