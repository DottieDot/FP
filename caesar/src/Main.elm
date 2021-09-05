module Main exposing (..)

import Browser
import Html exposing (Html, input, text, div, br, span)
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

encodeChar: Char -> Int -> Char
encodeChar char code =
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

decodeChar: Char -> Int -> Char
decodeChar char code =
  encodeChar char -code

encodeString: String -> Int -> String
encodeString str code =
  String.map (\char -> encodeChar char code) str

decodeString: String -> Int -> String
decodeString str code =
  encodeString str -code

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
        text "Encoded:",
        text (encodeString model.message model.code) 
      ],
      br[] [],
      span[] [ 
        text "Decoded:",
        text (decodeString model.message model.code)
      ]
    ]
  ]
