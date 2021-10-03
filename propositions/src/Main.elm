module Main exposing (..)
import Html exposing (text, div, br, strong, table, tr, td, th, tbody, thead)
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
    Impl lvalue rvalue -> switcheroo (Or rvalue (Not lvalue))
    Bi   lvalue rvalue -> switcheroo (And (Impl lvalue rvalue) (Impl rvalue lvalue))
    ----------
    Or   lvalue rvalue -> Or  (switcheroo lvalue) (switcheroo rvalue)
    And  lvalue rvalue -> And (switcheroo lvalue) (switcheroo rvalue)
    Not  value         -> Not (switcheroo value)
    Cons value         -> Cons value
    Var  name          -> Var name

deDeMorgan: Proposition -> Proposition
deDeMorgan proposition =
  case proposition of
    Or  (Not lvalue) (Not rvalue) -> Not (And lvalue rvalue)
    And (Not lvalue) (Not rvalue) -> Not (Or lvalue rvalue)
    ----------
    Impl lvalue rvalue -> Impl (deDeMorgan lvalue) (deDeMorgan rvalue)
    Bi   lvalue rvalue -> Bi   (deDeMorgan lvalue) (deDeMorgan rvalue)
    Or   lvalue rvalue -> Or   (deDeMorgan lvalue) (deDeMorgan rvalue)
    And  lvalue rvalue -> And  (deDeMorgan lvalue) (deDeMorgan rvalue)
    Not  value         -> Not  (deDeMorgan value)
    Cons value         -> Cons value
    Var  name          -> Var  name

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

listAppendUnique: List a -> List a -> List a
listAppendUnique l1 l2 =
  case l2 of
    [] -> l1
    x :: xs ->
      if not (List.member x l1) then
        List.append (listAppendUnique l1 xs) [x]
      else
        listAppendUnique l1 xs

getVarsFromProposition: Proposition -> List String
getVarsFromProposition proposition =
  case proposition of
    Or   lvalue rvalue -> listAppendUnique (getVarsFromProposition lvalue) (getVarsFromProposition rvalue)
    Impl lvalue rvalue -> listAppendUnique (getVarsFromProposition lvalue) (getVarsFromProposition rvalue)
    Bi   lvalue rvalue -> listAppendUnique (getVarsFromProposition lvalue) (getVarsFromProposition rvalue)
    And  lvalue rvalue -> listAppendUnique (getVarsFromProposition lvalue) (getVarsFromProposition rvalue)
    Not   value       ->  getVarsFromProposition value
    Var name          ->  [name]
    Cons _            ->  []

evalProposition: Dict String Bool -> Proposition -> Bool
evalProposition vars proposition =
  let
    eval = evalProposition vars
  in

  case proposition of
    Or   lvalue rvalue -> (eval lvalue) || (eval rvalue)
    And  lvalue rvalue -> (eval lvalue) && (eval rvalue)
    Not   value        -> not (eval value)
    Impl lvalue rvalue -> eval (switcheroo (Impl lvalue rvalue))
    Bi   lvalue rvalue -> eval (switcheroo (Bi lvalue rvalue))
    Cons  value        -> value
    Var  name          -> withDefault False (Dict.get name vars)

getCombinations: Int -> List (List Bool)
getCombinations num =
  if num == 0 then
    []
  else if num == 1 then
    [[True], [False]]
  else
    List.foldl (\i acc -> (True :: i) :: (False :: i) :: acc) [] (getCombinations (num - 1))

listsToDict: List comparable -> List value -> Dict comparable value
listsToDict keys values = 
  case (keys, values) of
    (x :: xs, y :: ys) ->
      Dict.insert x y (listsToDict xs ys)
    _ -> Dict.empty

truthTableRow : Dict String Bool -> Proposition -> Html.Html msg
truthTableRow vars proposition =
  tr [] 
    ((List.map (\(_, v) -> td [] [ text (toString v) ]) (Dict.toList vars)) 
      ++ [td [] [ text (toString (evalProposition vars proposition))]])

truthTable : Proposition -> Html.Html msg
truthTable proposition =
  let
    vars = List.sort (getVarsFromProposition proposition)
    values = getCombinations (List.length vars)
    rows = List.map (\v -> truthTableRow (listsToDict vars v) proposition) values
  in
  table []
    [ thead [] ((List.map (\v -> th [] [ text v ]) vars) ++ [(th [] [ text (propositionToString proposition) ])])
    , tbody [] rows
    ]

----------------------------
inputVars: Dict String Bool
inputVars = Dict.fromList 
  [ ("A", True)
  , ("B", True)
  , ("C", True)
  , ("D", True)
  , ("E", True)]

inputProp: Proposition
inputProp = (Not (And (Var "A") (Or (Impl (Var "B") (Var "C")) (Bi (Var "D") (Var "E")))))

swithcerood: Proposition
swithcerood = switcheroo inputProp
deDemorgand: Proposition
deDemorgand = deDeMorgan inputProp

main = div[]
  [ text ("vars used" ++ toString inputVars)
  , br[] []
  , br[] []
  , text (propositionToString inputProp)
  , br [] []
  , text ("Evaluates to: " ++ toString (evalProposition inputVars inputProp))
  , br [] []
  , text ("Vars: " ++ toString (getVarsFromProposition inputProp))
  , br [] []
  , text "Truth table:"
  , br [] []
  , (truthTable inputProp)
  , br [] []
  , br [] []
  , strong [] [ text "Switcheroo'd:" ]
  , br [] []
  , text (propositionToString swithcerood)
  , br [] []
  , text ("Evaluates to: " ++ toString (evalProposition inputVars swithcerood))
  , br [] []
  , br [] []
  , strong [] [ text "De-De Morgan'd:" ]
  , br [] []
  , text (propositionToString deDemorgand)
  , br [] []
  , text ("Evaluates to: " ++ toString (evalProposition inputVars deDemorgand))]

