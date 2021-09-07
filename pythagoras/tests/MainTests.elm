module MainTests exposing (leg1Tests, leg2Tests, hypTests, isPythTrippleTests, sqrTests, pythTripleTests, pythTripleMapTests, pythTripleRecTests, arePythTripleFilterTests, arePythTripleRecTests)

import Expect exposing (Expectation)
import Test exposing (..)
import Main exposing (leg1, leg2, hyp, isPythTripple, sqr, pythTriple)
import Main exposing (pythTripleMap)
import Main exposing (pythTripleRec)
import Main exposing (arePythTripleFilter)
import Main exposing (arePythTripleRec)

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
      (\_ -> Expect.equal True (isPythTripple (9, 40, 41))),
    test "isTriple 3 4 5 = True"
      (\_ -> Expect.equal True (isPythTripple (3, 4, 5)))
      
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

pythTripleMapTests: Test
pythTripleMapTests =
  describe "pythTripleMap" [
    test "pythTripleMap [] = []"
      (\_ -> Expect.equal [] (pythTripleMap [])),
    test "pythTripleMap [(2, 1)] = [Just(3, 4, 5)]"
      (\_ -> Expect.equal [Just(3, 4, 5)] (pythTripleMap [(2, 1)])),
    test "pythTripleMap [(2, 1), (1, 2)] = [Just(3, 4, 5), Nothing]"
      (\_ -> Expect.equal [Just(3, 4, 5), Nothing] (pythTripleMap [(2, 1), (1, 2)]))
  ]

pythTripleRecTests: Test
pythTripleRecTests =
  describe "pythTripleRec" [
    test "pythTripleRec [] = []"
      (\_ -> Expect.equal [] (pythTripleRec [])),
    test "pythTripleRec [(2, 1)] = [Just(3, 4, 5)]"
      (\_ -> Expect.equal [] (pythTripleRec [])),
    test "pythTripleRec [(2, 1), (1, 2)] = [Just(3, 4, 5), Nothing]"
      (\_ -> Expect.equal [] (pythTripleRec []))
  ]

arePythTripleFilterTests: Test
arePythTripleFilterTests =
  describe "arePythTripleFilter" [
    test "arePythTripleFilter [] = []"
      (\_ -> Expect.equal [] (arePythTripleFilter [])),
    test "arePythTripleFilter [(3, 4, 5)] = [(3, 4, 5)]"
      (\_ -> Expect.equal [(3, 4, 5)] (arePythTripleFilter [(3, 4, 5)])),
    test "arePythTripleFilter [(3, 4, 5), (1, 2, 3)] = [(3, 4, 5)]"
      (\_ -> Expect.equal [(3, 4, 5)] (arePythTripleFilter [(3, 4, 5), (1, 2, 3)]))
  ]

arePythTripleRecTests: Test
arePythTripleRecTests =
  describe "arePythTripleRec" [
    test "arePythTripleRec [] = []"
      (\_ -> Expect.equal [] (arePythTripleRec [])),
    test "arePythTripleRec [(3, 4, 5)] = [(3, 4, 5)]"
      (\_ -> Expect.equal [(3, 4, 5)] (arePythTripleRec [(3, 4, 5)])),
    test "arePythTripleRec [(3, 4, 5), (1, 2, 3)] = [(3, 4, 5)]"
      (\_ -> Expect.equal [(3, 4, 5)] (arePythTripleRec [(3, 4, 5), (1, 2, 3)]))
  ]
