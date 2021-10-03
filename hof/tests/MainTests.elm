module MainTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Main exposing (..)


repeatUntilTests : Test
repeatUntilTests =
  describe "repeatUntil" [
    test "repeatUntil above100 double 7 = 112"
      (\_ -> Expect.equal 112 (repeatUntil above100 double 7 )),
    test "repeatUntil above100 ((+) 1) 42 = 101"
      (\_ -> Expect.equal 101 (repeatUntil above100 ((+) 1) 42)),
    test "repeatUntil (aboveN 1000) double 7 = 1792"
      (\_ -> Expect.equal 1792 (repeatUntil (aboveN 1000) double 7)),
    test "repeatUntil ((==) 1) collatz 19 = 1"
      (\_ -> Expect.equal 1 (repeatUntil ((==) 1) collatz 19))
  ]

repeatUntilListTest : Test
repeatUntilListTest =
  describe "repeatUntilList" [
    test "repeatUntilList ((==) 1) collatz [19] = Just [1,2,4,8,16,5,10,20,40,13,26,52,17,34,11,22,44,88,29,58,19]"
      (\_ -> Expect.equal (Just [1,2,4,8,16,5,10,20,40,13,26,52,17,34,11,22,44,88,29,58,19]) (repeatUntilList ((==) 1) collatz [19]))
  ]
