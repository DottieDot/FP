module MainTests exposing (encryptCharTests, decryptCharTests, encryptStringTests, decryptStringTests, charToAlphabetIndexTests, alphabetIndexToCharTests, mapStringTests, filterStringTests, encryptNormalizedStringTests, decryptNormalizedStringTests)

import Main exposing (encryptChar, decryptChar, encryptString, decryptString)
import Test exposing (..)
import Expect exposing (Expectation)
import Main exposing (encryptString)
import Main exposing (charToAlphabetIndex)
import Main exposing (alphabetIndexToChar)
import Main exposing (mapString)
import Main exposing (filterString)
import Main exposing (decryptNormalizedString)
import Main exposing (encryptNormalizedString)

encryptCharTests: Test
encryptCharTests =
  describe "encryptChar" [
    test "shifts by the defined amount (2)"
      (\_ -> Expect.equal 'C' (encryptChar 'A' 2)),
    test "shifts by the defined amount (5)"
      (\_ -> Expect.equal 'F' (encryptChar 'A' 5)),
    test "preserves upper casing"
      (\_ -> Expect.equal 'F' (encryptChar 'A' 5)),
    test "preserves lower casing"
      (\_ -> Expect.equal 'f' (encryptChar 'a' 5)),
    test "preserves space"
      (\_ -> Expect.equal ' ' (encryptChar ' ' 5)),
    test "preserves numbers"
      (\_ -> Expect.equal '6' (encryptChar '6' 5)),
    test "preserves special characters"
      (\_ -> Expect.equal '*' (encryptChar '*' 5))
  ]

decryptCharTests: Test
decryptCharTests =
  describe "decryptChar" [
    test "shifts by the defined amount (2)"
      (\_ -> Expect.equal 'A' (decryptChar 'C' 2)),
    test "shifts by the defined amount (5)"
      (\_ -> Expect.equal 'A' (decryptChar 'F' 5)),
    test "preserves upper casing"
      (\_ -> Expect.equal 'A' (decryptChar 'F' 5)),
    test "preserves lower casing"
      (\_ -> Expect.equal 'a' (decryptChar 'f' 5)),
    test "preserves space"
      (\_ -> Expect.equal ' ' (decryptChar ' ' 5)),
    test "preserves numbers"
      (\_ -> Expect.equal '6' (decryptChar '6' 5)),
    test "preserves special characters"
      (\_ -> Expect.equal '*' (decryptChar '*' 5)) 
  ]

encryptStringTests: Test
encryptStringTests =
  describe "encryptString" [
    test "shifts all the letters by the defined amount (2)"
      (\_ -> Expect.equal "vguv" (encryptString "test" 2)),
    test "shifts all the letters by the defined amount (5)"
      (\_ -> Expect.equal "yjxy" (encryptString "test" 5)),
    test "preserves special characters"
      (\_ -> Expect.equal "1 *()" (encryptString "1 *()" 5)),
    test "wraps around"
      (\_ -> Expect.equal "test" (encryptString "test" 26))
  ]

decryptStringTests: Test
decryptStringTests =
  describe "decryptString" [
    test "shifts all the letters by the defined amount (2)"
      (\_ -> Expect.equal "test" (decryptString "vguv" 2)),
    test "shifts all the letters by the defined amount (5)"
      (\_ -> Expect.equal "test" (decryptString "yjxy" 5)),
    test "preserves special characters"
      (\_ -> Expect.equal "1 *()" (decryptString "1 *()" 5)),
    test "wraps around"
      (\_ -> Expect.equal "test" (decryptString "test" 26))
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

mapStringTests: Test
mapStringTests =
  describe "mapString" [
    test "returns empty string for empty string"
      (\_ -> Expect.equal "" (mapString (\c -> c) "")),
    test "returns identical string map returns the same char"
      (\_ -> Expect.equal "test" (mapString (\c -> c) "test")),
    test "returns new string with actual map function"
      (\_ -> Expect.equal "aaaa" (mapString (\_ -> 'a') "test"))
  ]

filterStringTests: Test
filterStringTests =
  describe "filterString" [
    test "returns empty string for empty string"
      (\_ -> Expect.equal "" (filterString (\_ -> True) "")),
    test "returns empty string when lambda only returns false"
      (\_ -> Expect.equal "" (filterString (\_ -> False) "test")),
    test "returns identical string when lambda only returns true"
      (\_ -> Expect.equal "test" (filterString (\_ -> True) "test")),
    test "only filters correct items"
      (\_ -> Expect.equal "es" (filterString (\c -> c /= 't') "test"))
  ]

encryptNormalizedStringTests: Test
encryptNormalizedStringTests =
  describe "encryptNormalizedString" [
    test "shifts all the letters by the defined amount (2)"
      (\_ -> Expect.equal "vguv" (encryptNormalizedString "test" 2)),
    test "shifts all the letters by the defined amount (5)"
      (\_ -> Expect.equal "yjxy" (encryptNormalizedString "test" 5)),
    test "removes special characters"
      (\_ -> Expect.equal "" (encryptNormalizedString "1 *()" 5)),
    test "wraps around"
      (\_ -> Expect.equal "test" (encryptNormalizedString "test" 26))
  ]

decryptNormalizedStringTests: Test
decryptNormalizedStringTests =
  describe "decryptNormalizedString" [
    test "shifts all the letters by the defined amount (2)"
      (\_ -> Expect.equal "test" (decryptNormalizedString "vguv" 2)),
    test "shifts all the letters by the defined amount (5)"
      (\_ -> Expect.equal "test" (decryptNormalizedString "yjxy" 5)),
    test "removes special characters"
      (\_ -> Expect.equal "" (decryptNormalizedString "1 *()" 5)),
    test "wraps around"
      (\_ -> Expect.equal "test" (decryptNormalizedString "test" 26))
  ]
