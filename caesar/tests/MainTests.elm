module MainTests exposing (encodeCharTests, decodeCharTests, encodeStringTests, decodeStringTests, charToAlphabetIndexTests, alphabetIndexToCharTests)

import Main exposing (encodeChar, decodeChar, encodeString, decodeString)
import Test exposing (..)
import Expect exposing (Expectation)
import Main exposing (encodeString)
import Main exposing (charToAlphabetIndex)
import Main exposing (alphabetIndexToChar)

encodeCharTests: Test
encodeCharTests =
  describe "encodeChar" [
    test "shifts by the defined amount (2)"
      (\_ -> Expect.equal 'C' (encodeChar 'A' 2)),
    test "shifts by the defined amount (5)"
      (\_ -> Expect.equal 'F' (encodeChar 'A' 5)),
    test "preserves upper casing"
      (\_ -> Expect.equal 'F' (encodeChar 'A' 5)),
    test "preserves lower casing"
      (\_ -> Expect.equal 'f' (encodeChar 'a' 5)),
    test "preserves space"
      (\_ -> Expect.equal ' ' (encodeChar ' ' 5)),
    test "preserves numbers"
      (\_ -> Expect.equal '6' (encodeChar '6' 5)),
    test "preserves special characters"
      (\_ -> Expect.equal '*' (encodeChar '*' 5))
  ]

decodeCharTests: Test
decodeCharTests =
  describe "decodeChar" [
    test "shifts by the defined amount (2)"
      (\_ -> Expect.equal 'A' (decodeChar 'C' 2)),
    test "shifts by the defined amount (5)"
      (\_ -> Expect.equal 'A' (decodeChar 'F' 5)),
    test "preserves upper casing"
      (\_ -> Expect.equal 'A' (decodeChar 'F' 5)),
    test "preserves lower casing"
      (\_ -> Expect.equal 'a' (decodeChar 'f' 5)),
    test "preserves space"
      (\_ -> Expect.equal ' ' (decodeChar ' ' 5)),
    test "preserves numbers"
      (\_ -> Expect.equal '6' (decodeChar '6' 5)),
    test "preserves special characters"
      (\_ -> Expect.equal '*' (decodeChar '*' 5)) 
  ]

encodeStringTests: Test
encodeStringTests =
  describe "encodeString" [
    test "shifts all the letters by the defined amount (2)"
      (\_ -> Expect.equal "vguv" (encodeString "test" 2)),
    test "shifts all the letters by the defined amount (5)"
      (\_ -> Expect.equal "yjxy" (encodeString "test" 5)),
    test "preserves special characters"
      (\_ -> Expect.equal "1 *()" (encodeString "1 *()" 5)),
    test "wraps around"
      (\_ -> Expect.equal "test" (encodeString "test" 26))
  ]

decodeStringTests: Test
decodeStringTests =
  describe "decodeString" [
    test "shifts all the letters by the defined amount (2)"
      (\_ -> Expect.equal "test" (decodeString "vguv" 2)),
    test "shifts all the letters by the defined amount (5)"
      (\_ -> Expect.equal "test" (decodeString "yjxy" 5)),
    test "preserves special characters"
      (\_ -> Expect.equal "1 *()" (decodeString "1 *()" 5)),
    test "wraps around"
      (\_ -> Expect.equal "test" (decodeString "test" 26))
  ]

charToAlphabetIndexTests: Test
charToAlphabetIndexTests =
  describe "charToAlphabetIndex" [
    test "returns correct value for lowercase letter"
      (\_ -> Expect.equal 2 (charToAlphabetIndex 'c')),
    test "returns correct value for uppercase letter"
      (\_ -> Expect.equal 2 (charToAlphabetIndex 'C'))
  ]

alphabetIndexToCharTests: Test
alphabetIndexToCharTests =
  describe "alphabetIndexToChar" [
    test "returns correct value for lowercase letter"
      (\_ -> Expect.equal 'c' (alphabetIndexToChar 2 False)),
    test "returns correct value for uppercase letter"
      (\_ -> Expect.equal 'C' (alphabetIndexToChar 2 True))
  ]
