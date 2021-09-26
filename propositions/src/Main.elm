module Main exposing (..)
import Html exposing (text, div, br, strong)
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

evalProposition: Dict String Bool -> Proposition -> Bool
evalProposition vars proposition =
  let
    eval = evalProposition vars
  in

  case proposition of
    Or   lvalue rvalue -> (eval lvalue) || (eval rvalue)
    And  lvalue rvalue -> (eval lvalue) && (eval rvalue)
    Not   value        -> not (eval value)
    Impl lvalue rvalue -> (eval lvalue) || (not (eval rvalue))
    Bi   lvalue rvalue -> ((eval lvalue) || (not (eval rvalue))) && ((eval rvalue) || (not (eval lvalue)))
    Cons  value        -> value
    Var  name          -> withDefault False (Dict.get name vars)


----------------------------
inputVars: Dict String Bool
inputVars = Dict.fromList 
  [ ("A", True)
  , ("B", True)
  , ("C", True)
  , ("D", True)]

inputProp: Proposition
inputProp = (Not (And (Var "A") (Or (Impl (Var "B") (Var "C")) (Bi (Var "D") (Var "E")))))

swithcerood: Proposition
swithcerood = switcheroo inputProp

main = div[]
  [ text ("vars used" ++ toString inputVars)
  , br[] []
  , br[] []
  , text (propositionToString inputProp)
  , br [] []
  , text ("Evaluates to: " ++ toString (evalProposition inputVars inputProp))
  , br [] []
  , br [] []
  , strong [] [ text "Switcheroo'd:" ]
  , br [] []
  , text (propositionToString swithcerood)
  , br [] []
  , text ("Evaluates to: " ++ toString (evalProposition inputVars swithcerood))]
