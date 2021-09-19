module MainTests exposing (sumListTests, reverseListTests, countIfTests, stringListToIntListTests, stringToDigitsTests, stringToDigitsReverseTests, doubleSecondsTests, getNumDigitsTests, joinIntListTests, biggerIntDivideTests, splitIntegerIntoSingleDigitsTests, splitIntegerIntoSingleDigitsReverseTests, splitMultiDigitIntegersToSingleDigitIntegersTests, areCreditcardDigitsValidTests, isValidTests, isValidWithIntTests, numValidTests, numValidWithIntTests, filterValidCreditCardsTests)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main exposing (..)


sumListTests: Test
sumListTests = describe "sumList" [
    test "sumList [1,2,3] = 6"
      (\_  -> Expect.equal 6 (sumList [1,2,3]))
  ]

reverseListTests: Test
reverseListTests = describe "reverseList" [
    test "reverseList [1,2,3] = [3,2,1]"
      (\_ -> Expect.equal [3,2,1] (reverseList [1,2,3]))
  ]

countIfTests: Test
countIfTests = describe "countIf" [
    test "countIf isEven [1,2,3] = 1"
      (\_ -> Expect.equal 1 (countIf (\i -> (modBy 2 i) == 0) [1,2,3])),
    test "countIf isOdd [1,2,3] = 2"
      (\_ -> Expect.equal 2 (countIf (\i -> (modBy 2 i) /= 0) [1,2,3]))
  ]

stringListToIntListTests: Test
stringListToIntListTests = describe "stringListToIntList" [
    test "stringListToIntList ['123', '321'] = Just([123, 321])"
      (\_ -> Expect.equal (Just([123, 321])) (stringListToIntList ["123", "321"]))
  ]

stringToDigitsTests: Test
stringToDigitsTests = describe "stringToDigits" [
    test "stringToDigits '123' = Just[1,2,3]"
      (\_ -> Expect.equal (Just[1,2,3]) (stringToDigits "123"))
  ]

stringToDigitsReverseTests: Test
stringToDigitsReverseTests = describe "stringToDigitsReverse" [
    test "stringToDigits '321' = Just[3,2,1]"
      (\_ -> Expect.equal (Just[3,2,1]) (stringToDigitsReverse "123"))
  ]

doubleSecondsTests: Test
doubleSecondsTests = describe "doubleSeconds" [
    test "doubleSeconds [2,2,2,2] = [2,4,2,4]"
      (\_ -> Expect.equal [2,4,2,4] (doubleSeconds [2,2,2,2]))
  ]


getNumDigitsTests: Test
getNumDigitsTests = describe "getNumDigits" [
    test "getNumDigits 123 = 3"
      (\_ -> Expect.equal 3 (getNumDigits 123)),
    test "getNumDigits 12 = 2"
      (\_ -> Expect.equal 2 (getNumDigits 12))
  ]

joinIntListTests: Test
joinIntListTests = describe "joinIntList" [
    test "joinIntList [4,7,1,6,3,4,7,1,8,4,8,6,2,9,6,1] = 4716347184862961"
      (\_ -> Expect.equal 4716347184862961 (joinIntList [4,7,1,6,3,4,7,1,8,4,8,6,2,9,6,1]))
  ]

biggerIntDivideTests: Test
biggerIntDivideTests = describe "biggerIntDivide" [
    test "biggerIntDivide 4716347184862961 = 471634718486296"
      (\_ -> Expect.equal 471634718486296 (biggerIntDivide 4716347184862961 10))
  ]

splitIntegerIntoSingleDigitsTests: Test
splitIntegerIntoSingleDigitsTests = describe "splitIntegerIntoSingleDigits" [
    test "splitIntegerIntoSingleDigits 123 = [1,2,3]"
      (\_ -> Expect.equal [1,2,3] (splitIntegerIntoSingleDigits 123))
  ]

splitIntegerIntoSingleDigitsReverseTests: Test
splitIntegerIntoSingleDigitsReverseTests = describe "splitIntegerIntoSingleDigitsReverse" [
    test "splitIntegerIntoSingleDigitsReverse 123 = [3,2,1]"
      (\_ -> Expect.equal [3,2,1] (splitIntegerIntoSingleDigitsReverse 123))
  ]

splitMultiDigitIntegersToSingleDigitIntegersTests: Test
splitMultiDigitIntegersToSingleDigitIntegersTests = describe "splitMultiDigitIntegersToSingleDigitIntegers" [
    test "splitMultiDigitIntegersToSingleDigitIntegers [1,16] = [1,1,6]"
      (\_ -> Expect.equal [1,1,6] (splitMultiDigitIntegersToSingleDigitIntegers [1,16]))
  ]

areCreditcardDigitsValidTests: Test
areCreditcardDigitsValidTests = describe "areCreditcardDigitsValid" [
    test "areCreditcardDigitsValid [4,7,1,6,3,4,7,1,8,4,8,6,2,9,6,1] = True"
      (\_ -> Expect.equal True (areCreditcardDigitsValid [4,7,1,6,3,4,7,1,8,4,8,6,2,9,6,1])),
    test "areCreditcardDigitsValid [4,7,1,6,3,4,7,1,8,4,8,6,2,9,6,2] = False"
      (\_ -> Expect.equal False (areCreditcardDigitsValid [4,7,1,6,3,4,7,1,8,4,8,6,2,9,6,2]))
  ]

isValidTests: Test
isValidTests = describe "isValid" [
    test "isValid '4716347184862961' = True"
      (\_ -> Expect.equal True (isValid "4716347184862961")),
    test "isValid '4716347184862962' = False"
      (\_ -> Expect.equal False (isValid "4716347184862962")) 
  ]

isValidWithIntTests: Test
isValidWithIntTests = describe "isValidWithInt" [
    test "isValid 4716347184862961 = True"
      (\_ -> Expect.equal True (isValidWithInt 4716347184862961)),
    test "isValid 4716347184862962 = False"
      (\_ -> Expect.equal False (isValidWithInt 4716347184862962)) 
  ]


numValidTests: Test
numValidTests = describe "numValid" [
    test "numValid ['4716347184862961','4532899082537349','4929778869082405'] = 2"
      (\_ -> Expect.equal 2 (numValid ["4716347184862961","4532899082537349","4929778869082405"]))
  ]

numValidWithIntTests: Test
numValidWithIntTests = describe "numValidWithInt" [
    test "numValid [4716347184862961,4532899082537349,4929778869082405] = 2"
      (\_ -> Expect.equal 2 (numValidWithInt [4716347184862961,4532899082537349,4929778869082405]))
  ]

filterValidCreditCardsTests: Test
filterValidCreditCardsTests = describe "filterValidCreditCards" [
    test "filterValidCreditCards [4716347184862961,4532899082537349,4929778869082405] = [4716347184862961,4929778869082405]"
      (\_ -> Expect.equal [4716347184862961,4929778869082405] (filterValidCreditCards [4716347184862961,4532899082537349,4929778869082405]))
  ]
