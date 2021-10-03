module MainTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Main exposing (..)
import Dict exposing (Dict)


propositionToStringTests : Test
propositionToStringTests = 
  describe "propositionToString" [
    test "propositionToString (Cons True) = 'True'"
      (\_ -> Expect.equal "True" (propositionToString (Cons True))),
    test "propositionToString (Cons False) = 'False'"
      (\_ -> Expect.equal "False" (propositionToString (Cons False))),
    test "propositionToString (Var 'A') = 'A'"
      (\_ -> Expect.equal "A" (propositionToString (Var "A"))),
    test "propositionToString (Not (Cons True)) = '¬True'"
      (\_ -> Expect.equal "¬True" (propositionToString (Not (Cons True)))),
    test "propositionToString (And (Cons True) (Cons True)) = '(True ∧ True)'"
      (\_ -> Expect.equal "(True ∧ True)" (propositionToString (And (Cons True) (Cons True)))),
    test "propositionToString (Or (Cons True) (Cons True)) = '(True ∨ True)'"
      (\_ -> Expect.equal "(True ∨ True)" (propositionToString (Or (Cons True) (Cons True)))),
    test "propositionToString (Impl (Cons False) (Cons True)) = '(True → True)'"
      (\_ -> Expect.equal "(False → True)" (propositionToString ((Impl (Cons False) (Cons True))))),
    test "propositionToString (Bi (Cons False) (Cons True)) = '(True ⭤ True)'"
      (\_ -> Expect.equal "(False ⭤ True)" (propositionToString ((Bi (Cons False) (Cons True)))))
  ]

switcherooTests : Test
switcherooTests =
  describe "switcheroo" [
    test "switcheroo (Impl (Cons True) (Cons False))) = (Or (Cons True) (Not (Cons False))"
      (\_ -> Expect.equal (Or (Cons False) (Not (Cons True))) (switcheroo (Impl (Cons True) (Cons False)))),
    test "switcheroo (Bi (Cons True) (Cons False))) = (Or (Cons True) (Not (Cons False))"
      (\_ -> Expect.equal (And (Or (Cons False) (Not (Cons True))) (Or (Cons True) (Not (Cons False)))) (switcheroo (Bi (Cons True) (Cons False))))
  ]

evalPropositionTests : Test
evalPropositionTests =
  describe "evalProposition" [
    test "evalProposition Dict.empty (Cons True) = True"
      (\_ -> Expect.equal True (evalProposition Dict.empty (Cons True))),
    test "evalProposition Dict.empty (Cons False) = False"
      (\_ -> Expect.equal False (evalProposition Dict.empty (Cons False))),
    test "evalProposition Dict.empty (Var 'A') = False"
      (\_ -> Expect.equal False (evalProposition Dict.empty (Var "A"))),
    test "evalProposition Dict.empty (Not (Cons True)) = False"
      (\_ -> Expect.equal False (evalProposition Dict.empty (Not (Cons True)))),
    test "evalProposition Dict.empty (And (Cons True) (Cons True)) = True"
      (\_ -> Expect.equal True (evalProposition Dict.empty (And (Cons True) (Cons True)))),
    test "evalProposition Dict.empty (Or (Cons True) (Cons True)) = True"
      (\_ -> Expect.equal True (evalProposition Dict.empty (Or (Cons True) (Cons True)))),
    test "evalProposition Dict.empty (Impl (Cons False) (Cons True)) = True"
      (\_ -> Expect.equal True (evalProposition Dict.empty ((Impl (Cons False) (Cons True))))),
    test "evalProposition Dict.empty (Bi (Cons False) (Cons True)) = False"
      (\_ -> Expect.equal False (evalProposition Dict.empty ((Bi (Cons False) (Cons True)))))
  ]

deDeMorganTests : Test
deDeMorganTests =
  describe "deDeMorgan" [
    test "deDeMorgan Or (Not (Cons True)) (Not (Cons True)) = Not (And (Cons True) (Cons True))"
      (\_ -> Expect.equal (Not (And (Cons True) (Cons True))) (deDeMorgan (Or (Not (Cons True)) (Not (Cons True))))),
          test "deDeMorgan And (Not (Cons True)) (Not (Cons True)) = Not (Or (Cons True) (Cons True))"
      (\_ -> Expect.equal (Not (Or (Cons True) (Cons True))) (deDeMorgan (And (Not (Cons True)) (Not (Cons True)))))
  ]
