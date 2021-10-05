module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Exam_Taran_van_Groenigen exposing (..)


insertTests : Test
insertTests =
  describe "insert"
    [ test "insert 4 [1,3,5,7,9] = [1,3,4,5,7,9]"
      (\_ -> Expect.equal [1,3,4,5,7,9] (insert 4 [1,3,5,7,9]))
    , test "insert -4 [5,3,6,2] = [-4,5,3,6,2]"
      (\_ -> Expect.equal [-4,5,3,6,2] (insert -4 [5,3,6,2]))
    , test "insert 73 [5,3,6,2] = [5,3,6,2,73]"
      (\_ -> Expect.equal [5,3,6,2,73] (insert 73 [5,3,6,2]))
    ]

insertionSortTests : Test
insertionSortTests =
  describe "insertionSort"
    [ test "insertionSort [3,7,4,9,1] [] = [1,3,4,7,9]"
      (\_ -> Expect.equal [1,3,4,7,9] (insertionSort [3,7,4,9,1] []))
    ]

posProductTests : Test
posProductTests =
  describe "posProduct"
    [ test "posProduct [ (3,-4),(2,5),(7,0),(-1,-9) ] = [ 10,9 ]"
        (\_ -> Expect.equal [ 10,9 ] (posProduct [ (3,-4),(2,5),(7,0),(-1,-9) ]))
    ]

expressionToStringTests : Test
expressionToStringTests =
  describe "expressionToString"
    [ test "expressionToString (Fact (Val 5)) = 5!"
      (\_ -> Expect.equal "5!" (expressionToString (Fact (Val 5))))
    , test "expressionToString (Mult (Plus e0 (Val -110)) (Val 3))  = ((5! + -110) * 3)"
      (\_ -> Expect.equal "((5! + -110) * 3)" (expressionToString(Mult (Plus e0 (Val -110)) (Val 3))))
    ]

factorialTests : Test
factorialTests =
  describe "factorial" 
    [ test "factorial -1 = 1"
      (\_ -> Expect.equal 1 (factorial -1))
    , test "factorial 0 = 1"
      (\_ -> Expect.equal 1 (factorial 0))
    , test "factorial 1 = 1"
      (\_ -> Expect.equal 1 (factorial 1))
    , test "factorial 2 = 2"
      (\_ -> Expect.equal 2 (factorial 2))
    , test "factorial 3 = 6"
      (\_ -> Expect.equal 6 (factorial 3))
    , test "factorial 4 = 24"
      (\_ -> Expect.equal 24 (factorial 4))
    ]

evalExpressionTests : Test
evalExpressionTests =
  describe "evalExpression"
    [ test "evalExpression (Fact (Val 5)) = 120"
      (\_ -> Expect.equal 120 (evalExpression (Fact (Val 5))))
    , test "evalExpression (Mult (Plus e0 (Val -110)) (Val 3)) = 30"
      (\_ -> Expect.equal 30 (evalExpression (Mult (Plus e0 (Val -110)) (Val 3))))
    ]

simplifyExpressionTests : Test
simplifyExpressionTests =
  describe "simplifyExpression"
    [ test "simplifyExpression (Mult (Val 1) (Val 5)) = (Val 5)"
      (\_ -> Expect.equal (Val 5) (simplifyExpression (Mult (Val 1) (Val 5))))
    , test "simplifyExpression (Mult (Val 5) (Val 1)) = (Val 5)"
      (\_ -> Expect.equal (Val 5) (simplifyExpression (Mult (Val 5) (Val 1))))
    , test "simplifyExpression (Plus (Val 0) (Val 5)) = (Val 5)"
      (\_ -> Expect.equal (Val 5) (simplifyExpression (Plus (Val 0) (Val 5))))
    , test "simplifyExpression (Plus (Val 5) (Val 0)) = (Val 5)"
      (\_ -> Expect.equal (Val 5) (simplifyExpression (Plus (Val 5) (Val 0))))
    , test "simplifyExpression (Minus (Val 0) (Val 5)) = (Val 5)"
      (\_ -> Expect.equal (Val 5) (simplifyExpression (Minus (Val 0) (Val 5))))
    , test "simplifyExpression (Minus (Val 5) (Val 0)) = (Val 5)"
      (\_ -> Expect.equal (Val 5) (simplifyExpression (Minus (Val 5) (Val 0))))
    , test "simplifyExpression (Mult (Val 0) (Val 5)) = (Val 0)"
      (\_ -> Expect.equal (Val 0) (simplifyExpression (Mult (Val 0) (Val 5))))
    , test "simplifyExpression (Mult (Val 5) (Val 0)) = (Val 0)"
      (\_ -> Expect.equal (Val 0) (simplifyExpression (Mult (Val 5) (Val 0))))
    , test "simplifyExpression (Mult (Val 42) (Minus (Val 73) (Minus (Mult (Plus (Val 0) (Val 0)) (Val 1)) (Val 0))))) = Mult (Val 42) (Val 73)"
      (\_ -> Expect.equal (Mult (Val 42) (Val 73)) (simplifyExpression (Mult (Val 42) (Minus (Val 73) (Minus (Mult (Plus (Val 0) (Val 0)) (Val 1)) (Val 0))))))
    ]
