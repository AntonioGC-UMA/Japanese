module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (..)
import Set exposing (..)
import List exposing (..)
import Dict exposing (..)




letras : Dict String String
letras =
  Dict.fromList
    [ 
      ("a", "あ"),
      ("i", "い"),
      ("u", "う"),
      ("e", "え"),
      ("o", "お"),
      ("ka", "か"),
      ("ki", "き"),
      ("ku", "く"),
      ("ke", "け"),
      ("ko", "こ"),
      ("ca", "か"),
      ("ci", "き"),
      ("cu", "く"),
      ("ce", "け"),
      ("co", "こ"),
      ("sa", "さ"),
      ("si", "し"),
      ("shi", "し"),
      ("su", "す"),
      ("se", "せ"),
      ("so", "そ"),
      ("ta", "た"),
      ("ti", "ち"),
      ("chi", "ち"),
      ("tu", "つ"),
      ("tsu", "つ"),
      ("te", "て"),
      ("to", "と"),
      ("na", "な"),
      ("ni", "に"),
      ("nu", "ぬ"),
      ("ne", "ね"),
      ("no", "の"),
      ("ha", "は"),
      ("hi", "ひ"),
      ("hu", "ふ"),
      ("fu", "ふ"),
      ("he", "へ"),
      ("ho", "ほ"),
      ("ma", "ま"),
      ("mi", "み"),
      ("mu", "む"),
      ("me", "め"),
      ("mo", "も"),
      ("ya", "や"),
      ("yu", "ゆ"),
      ("yo", "よ"),
      ("ra", "ら"),
      ("ri", "り"),
      ("ru", "る"),
      ("re", "れ"),
      ("ro", "ろ"),
      ("wa", "わ"),
      ("wo", "を"),
      ("nn", "ん")
    ]

total : List String
total = Set.toList (Set.fromList (values letras))
-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { 
    japones : String
  , entrada : String
  , pendientes : List String
  , actual : String
  }


init : Model
init =
  Model "" "" (Maybe.withDefault [] (tail total)) (Maybe.withDefault "error" (head total))



-- UPDATE


type Msg
  = Write String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Write text ->
      case get text letras of
        Nothing -> { model | entrada = text}
        Just l -> 
          if l == model.actual 
            then { model | entrada = "", japones = "", actual = (Maybe.withDefault "end" (head model.pendientes)), pendientes = drop 1 model.pendientes}
            else { model | entrada = "", japones = "wrong" }




-- VIEW


view : Model -> Html Msg
view model =
  div [] 
  [
    --div []    [ text (concat total) ],
    div []    [ text model.actual ],
    div []    [ text model.japones ],
    div []    [ viewInput "romaji" "Romaji" model.entrada Write ]
  ]



viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []
