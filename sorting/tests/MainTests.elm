module MainTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Main exposing (..)


halveTests : Test
halveTests =
  describe "halve" [
    test "halve [1,2,3] = ([1,3],[2])"
      (\_ -> Expect.equal ([1,3], [2]) (halve [1,2,3])),
    test "halve [1,2,3,4] = ([1,3],[2,4])"
      (\_ -> Expect.equal ([1,3], [2,4]) (halve [1,2,3,4]))
  ]

margeTests : Test
margeTests =
  describe "merge" [
    test "marge [1,3,5] [2,4] = [1,2,3,4,5]"
      (\_ -> Expect.equal [1,2,3,4,5] (merge [1,3,5] [2,4])),
    test "merge [1,2,3] [] = [1,2,3]"
      (\_ -> Expect.equal [1,2,3] (merge [1,2,3] [])),
    test "marge [] [1,2,3] = [1,2,3]"
      (\_ -> Expect.equal [1,2,3] (merge [] [1,2,3]))
  ]

mergeSortTests : Test
mergeSortTests =
  describe "mergeSort" [
    test "mergeSort [5,4,3,2,1] = [1,2,3,4,5]"
      (\_ -> Expect.equal [1,2,3,4,5] (mergeSort [5,4,3,2,1])),
    test "mergeSort [1,2,3,4,5] = [1,2,3,4,5]"
      (\_ -> Expect.equal [1,2,3,4,5] (mergeSort [1,2,3,4,5])),
    test "mergeSort [1,3,2,4,5] = [1,2,3,4,5]"
      (\_ -> Expect.equal [1,2,3,4,5] (mergeSort [1,3,2,4,5]))
  ]
