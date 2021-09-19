module Main exposing (..)

import Browser
import Html exposing (Html, input, text, div, br, span, hr)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Debug exposing (toString)

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

stringStartsWith: String -> String -> Bool
stringStartsWith substr str =
  case String.uncons substr of
    Nothing -> True
    Just(h1, t1) -> 
      case String.uncons str of
        Nothing -> False
        Just(h2, t2) ->
          h1 == h2 && stringStartsWith t1 t2

stringContains: String -> String -> Bool
stringContains substr str = 
  if stringStartsWith substr str then
    True
  else
    case String.uncons str of
      Nothing -> False
      Just(_, tail) ->
        stringContains substr tail

stringContainsList: List String -> String -> Bool
stringContainsList list str =
  List.any (\item -> stringContains item str) list

candidates: List String -> String -> List (Int, String)
candidates keywords encrypted =
  List.range 0 25
    |> List.map (\i -> (i, decryptNormalizedString encrypted i))
    |> List.filter (\(_, decrypted) -> stringContainsList keywords decrypted)

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
    code: Int,
    keywords: String
  }

init: Model
init = 
  {
    message = "",
    code = 0,
    keywords = ""
  }

-- UPDATE
type Msg
  = ChangeMessageText String
  | ChangeCode String
  | ChangeKeywords String

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
    ChangeKeywords keywords ->
      {
        model |
        keywords = keywords
      }

-- VIEW
view: Model -> Html Msg
view model =
  div [] [
    text "Message: ",
    input [
      placeholder "Message",
      value model.message,
      onInput ChangeMessageText
    ] [],
    text "Code: ",
    input [
      placeholder "Code",
      type_ "number",
      value (String.fromInt model.code),
      onInput ChangeCode
    ] [],
    text "Keywords: ",
    input [
      placeholder "Keywords (comma seperated)",
      value model.keywords,
      onInput ChangeKeywords
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
      ],
      br[] [],
      hr[] [],
      text "candidated:",
      br[] [],
      text (toString (candidates (String.split "," model.keywords) model.message))
    ]
  ]
