module Main exposing (..)
import Html exposing (text, td, tr, table, tbody, thead, th)
import Debug exposing (toString)
import Dict exposing (Dict)
import Maybe exposing (withDefault)

type Proposition
  = Or Proposition Proposition
  | And Proposition Proposition
  | Not Proposition
  | Impl Proposition Proposition
  | Bi Proposition Proposition
  | Cons Bool
  | Var String

switcheroo: Proposition -> Proposition
switcheroo proposition =
  case proposition of
    Impl lvalue rvalue -> switcheroo (Or lvalue (Not rvalue))
    Bi   lvalue rvalue -> switcheroo (And (Impl lvalue rvalue) (Impl rvalue lvalue))
    Or   lvalue rvalue -> Or  (switcheroo lvalue) (switcheroo rvalue)
    And  lvalue rvalue -> And (switcheroo lvalue) (switcheroo rvalue)
    Not  value         -> Not (switcheroo value)
    Cons value         -> Cons value
    Var  name          -> Var name

evalInternal: Dict String Bool -> Proposition -> (Bool, List (Proposition, Bool))
evalInternal variables proposition = 
  case proposition of
    Or lvalue rvalue ->
      let
        (llast, lres) = evalInternal variables lvalue
        (rlast, rres) = evalInternal variables rvalue
        val = llast || rlast
      in
      (val, (proposition, val) :: (List.concat [lres, rres]))
    And lvalue rvalue ->
      let
        (llast, lres) = evalInternal variables lvalue
        (rlast, rres) = evalInternal variables rvalue
        val = llast && rlast
      in
      (val, (proposition, val) :: (List.concat [lres, rres]))
    Not value ->
      let
        (last, res) = evalInternal variables value
        val = not last
      in
      (val, (proposition, val) :: res)
    Cons value -> (value, [(proposition, value)])
    Var name  ->
      let
        val = (withDefault False (Dict.get name variables))
      in
      (val, [(proposition, val)])
    Impl lvalue rvalue ->
      let
        (llast, lres) = evalInternal variables lvalue
        (rlast, rres) = evalInternal variables rvalue
        val = llast || (not rlast)
      in
      (val, (proposition, val) :: (List.concat [lres, rres]))
    Bi lvalue rvalue ->
      let
        (llast, lres) = evalInternal variables lvalue
        (rlast, rres) = evalInternal variables rvalue
        val = (llast || (not rlast)) && (rlast || (not llast))
      in
      (val, (proposition, val) :: (List.concat [lres, rres]))

eval: Dict String Bool -> Proposition -> List (Proposition, Bool)
eval variables proposition =
  let
    (_, results) = evalInternal variables proposition
  in
  results

-- simplify: Proposition -> Proposition
-- simplify proposition =
--   case proposition of
--     -- Double negaton
--     Not  (Not lvalue)    -> simplify lvalue

--     -- Inverse ????
--     Not  (Constant bool) -> Constant (not bool)

--     -- DeMorgan
--     Not (And lvalue rvalue) -> Or  (simplify (Not lvalue)) (simplify (Not rvalue))
--     Not (Or lvalue rvalue)  -> And (simplify (Not lvalue)) (simplify (Not rvalue))

--     -- Identity Or
--     Or lvalue (Constant False) -> simplify lvalue
--     Or (Constant False) rvalue -> simplify rvalue
--     -- Identity And
--     And lvalue (Constant True) -> simplify lvalue
--     And (Constant True) rvalue -> simplify rvalue
--     -- Identity Impl
--     Impl (Constant True) rvalue -> simplify rvalue

--     -- Swithceroo
--     Impl lvalue rvalue   -> simplify (Or lvalue (Not rvalue))
--     -- Switcheroo
--     Bi   lvalue rvalue   -> simplify (And (Impl lvalue rvalue) (Impl rvalue lvalue))

--     Or   lvalue rvalue   -> And (simplify lvalue) (simplify rvalue)
--     And  lvalue rvalue   -> And (simplify lvalue) (simplify rvalue)
--     Not  lvalue          -> Not (simplify lvalue)
--     any -> any

propositionToString: Proposition -> String
propositionToString proposition =
  case proposition of
    Or   lvalue rvalue -> "(" ++ (propositionToString lvalue) ++ " ∨ " ++ (propositionToString rvalue) ++ ")"
    And  lvalue rvalue -> "(" ++ (propositionToString lvalue) ++ " ∧ " ++ (propositionToString rvalue) ++ ")"
    Not   value        -> "¬" ++ (propositionToString  value)
    Impl lvalue rvalue -> "(" ++ (propositionToString lvalue) ++ " → " ++ (propositionToString rvalue) ++ ")"
    Bi   lvalue rvalue -> "(" ++ (propositionToString lvalue) ++ " ⭤ " ++ (propositionToString rvalue) ++ ")"
    Cons value     -> toString(value)
    Var  name      -> name

getVariablesFromProposition: Proposition -> List String
getVariablesFromProposition proposition =
  case proposition of
    Or lvalue rvalue -> List.concat [getVariablesFromProposition lvalue, getVariablesFromProposition rvalue]
    And lvalue rvalue -> List.concat [getVariablesFromProposition lvalue, getVariablesFromProposition rvalue]
    Impl lvalue rvalue -> List.concat [getVariablesFromProposition lvalue, getVariablesFromProposition rvalue]
    Bi lvalue rvalue -> List.concat [getVariablesFromProposition lvalue, getVariablesFromProposition rvalue]
    Not value -> getVariablesFromProposition value
    Cons _ -> []
    Var name -> [name]
    

getCombinations: Int -> List (List Bool)
getCombinations num =
  if num == 0 then
    []
  else if num == 1 then
    [[True], [False]]
  else
    List.foldl (\i acc -> (True :: i) :: (False :: i) :: acc) [] (getCombinations (num - 1))

combineLists: List a -> List b -> List (a, b)
combineLists l1 l2 =
  case (l1, l2) of
    (x :: xs, y :: ys) ->
      (x, y) :: combineLists xs ys
    _ -> []

listsToDict: List comparable -> List value -> Dict comparable value
listsToDict keys values =
  Dict.fromList (combineLists keys values)

type alias TruthTableHeader = List Proposition
type alias TruthTableRow = List Bool
createTruthTableForProposition: Proposition -> (TruthTableHeader, List TruthTableRow)
createTruthTableForProposition proposition =
  let
    varNames = getVariablesFromProposition proposition
    combinations = getCombinations (List.length varNames)
    varDicts = List.map (\combs -> listsToDict varNames combs) combinations
    resultTuples = List.map (\v -> eval v proposition) varDicts
    flat = List.concat resultTuples
  in
  ([], results)
  
-- main = text (propositionToString (switcheroo (Impl (Or (Var "p") (Not (Not (Cons False)))) (Or (Cons True) (Not (Cons False))))))
prop: Proposition
prop = (Not (And (Var "A") (Or (Impl (Var "B") (Var "C")) (Bi (Var "D") (Var "E")))))
vars: Dict String Bool
vars = Dict.fromList 
  [ ("A", True)
  , ("B", False)
  , ("C", False)
  , ("D", True)
  ]

printTruthTableHeaders: (Bool, List (Proposition, Bool)) -> List (Html.Html msg)
printTruthTableHeaders (result, list) =
  case list of
    [] -> []
    (proposition, _) :: xs -> 
      th [] [ text (propositionToString proposition) ] 
        :: printTruthTableHeaders (result, xs)

printTruthTableRow: (Bool, List (Proposition, Bool)) -> List (Html.Html msg)
printTruthTableRow (result, list) =
  case list of
    [] -> []
    (_, value) :: xs -> td [] [ text (toString value) ] :: printTruthTableRow (result, xs)

main = 
  let 
    truth = (evalInternal vars prop)
  in
  table []
    [ thead []
      [ tr [] (printTruthTableHeaders truth)
      ]
    , tbody []
      [ tr [] (printTruthTableRow truth)
      ]
    ]
