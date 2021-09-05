module MainTests exposing (leg1Tests, leg2Tests, hypTests, isPythTrippleTests, sqrTests, pythTripleTests)

import Expect exposing (Expectation)
import Test exposing (..)
import Main exposing (leg1, leg2, hyp, isPythTripple, sqr, pythTriple)

leg1Tests: Test
leg1Tests = 
  describe "leg1" [
    test "leg1 5 4 = 9"
      (\_ -> Expect.equal (Just 9) (leg1 5 4)),
    test "leg1 2 1 = 1"
      (\_ -> Expect.equal (Just 3) (leg1 2 1)),
    test "leg1 1 2 = Nothing"
      (\_ -> Expect.equal Nothing (leg1 1 2))
  ]

leg2Tests: Test
leg2Tests = 
  describe "leg2" [
    test "leg2 5 4 = 40"
      (\_ -> Expect.equal (Just 40) (leg2 5 4)),
    test "leg2 2 1 = 4"
      (\_ -> Expect.equal (Just 4) (leg2 2 1)),
    test "leg2 1 2 = Nothing"
      (\_ -> Expect.equal Nothing (leg2 1 2))
  ]

hypTests: Test
hypTests = 
  describe "hyp" [
    test "hyp 5 4 = 41"
      (\_ -> Expect.equal (Just 41) (hyp 5 4)),
    test "hyp 2 1 = 5"
      (\_ -> Expect.equal (Just 5) (hyp 2 1)),
    test "hyp 1 2 = Nothing"
      (\_ -> Expect.equal Nothing (hyp 1 2))
  ]

pythTripleTests: Test
pythTripleTests =
  describe "pythTriple" [
    test "pythTriple (5, 4) = (9, 40, 41)"
      (\_ -> Expect.equal (Just (9, 40, 41)) (pythTriple (5, 4))),
    test "pythTriple (2, 1) = (3, 4, 5)"
      (\_ -> Expect.equal (Just (3, 4, 5)) (pythTriple (2, 1))),
    test "pythTriple (1, 2) = Nothing"
      (\_ -> Expect.equal Nothing (pythTriple (1, 2)))
  ]

isPythTrippleTests: Test
isPythTrippleTests = 
  describe "isTriple" [
    test "isTriple 9 40 41 = True"
      (\_ -> Expect.equal True (isPythTripple 9 40 41)),
    test "isTriple 3 4 5 = True"
      (\_ -> Expect.equal True (isPythTripple 3 4 5))
      
  ]

sqrTests: Test
sqrTests =
  describe "sqr" [
    test "sqr 2 = 4"
      (\_ -> Expect.equal 4 (sqr 2)),
    test "sqr 0 = 0"
      (\_ -> Expect.equal 0 (sqr 0)),
    test "sqr -2 = 4"
      (\_ -> Expect.equal 4 (sqr -2))
  ]
