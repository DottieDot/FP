module Main exposing (..)

import Browser
import Html exposing (Html, input, text, div, br, span, hr)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

charToAlphabetIndex: Char -> Int
charToAlphabetIndex char =
  if Char.isUpper char then
    Char.toCode char - 65
  else
    Char.toCode char - 97

alphabetIndexToChar: Int -> Bool -> Char
alphabetIndexToChar alphabetIndex upperCase =
  if upperCase then
    Char.fromCode (alphabetIndex + 65)
  else
    Char.fromCode (alphabetIndex + 97)

encryptChar: Char -> Int -> Char
encryptChar char code =
  if Char.isAlpha char then
    let
      alphabetIndex = charToAlphabetIndex char
    in
    let
      newAlphabetIndex = modBy 26 (alphabetIndex + code)
    in

    alphabetIndexToChar newAlphabetIndex (Char.isUpper char)
  else
    char

decryptChar: Char -> Int -> Char
decryptChar char code =
  encryptChar char -code

filterString: (Char -> Bool) -> String -> String
filterString fn str =
  case String.uncons str of
    Nothing -> str
    Just(head, tail) ->
      if fn head then
        String.cons head (filterString fn tail)
      else
        filterString fn tail

normalizeString: String -> String
normalizeString str =
  String.filter (\char -> Char.isAlpha char) str

mapString: (Char -> Char) -> String -> String
mapString fn str =
  case String.uncons str of
    Nothing -> str
    Just(head, tail) ->
      String.cons (fn head) (mapString fn tail)

encryptString: String -> Int -> String
encryptString str code =
  mapString (\char -> encryptChar char code) str

decryptString: String -> Int -> String
decryptString str code =
  encryptString str -code

encryptNormalizedString: String -> Int -> String
encryptNormalizedString str code =
  encryptString (normalizeString str) code

decryptNormalizedString: String -> Int -> String
decryptNormalizedString str code =
  decryptString (normalizeString str) code

-- MAIN
main: Program () Model Msg
main =
  Browser.sandbox {
    init = init,
    update = update,
    view = view
  }

-- MODEL
type alias Model = 
  {
    message: String,
    code: Int
  }

init: Model
init = 
  {
    message = "",
    code = 0
  }

-- UPDATE
type Msg
  = ChangeMessageText String
  | ChangeCode String

update: Msg -> Model -> Model
update msg model =
  case msg of
    ChangeMessageText newMessage ->
      {
        model |
        message = newMessage
      }
    ChangeCode newCode ->
      {
        model |
        code = Maybe.withDefault 0 (String.toInt newCode)
      }

-- VIEW
view: Model -> Html Msg
view model =
  div [] [
    input [
      placeholder "Message",
      value model.message,
      onInput ChangeMessageText
    ] [],
    input [
      placeholder "Code",
      type_ "number",
      value (String.fromInt model.code),
      onInput ChangeCode
    ] [],
    div [] [
      span[] [ 
        text "encrypted:",
        text (encryptString model.message model.code) 
      ],
      br[] [],
      span[] [ 
        text "decrypted:",
        text (decryptString model.message model.code)
      ],
      br[] [],
      hr[] [],
      span[] [ 
        text "normalized encrypted:",
        text (encryptNormalizedString model.message model.code) 
      ],
      br[] [],
      span[] [ 
        text "normalized decrypted:",
        text (decryptNormalizedString model.message model.code)
      ]
    ]
  ]
