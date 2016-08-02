module Algae exposing (..)

import Char
import Html exposing (Html)
import Html.App as App
import Keyboard
import String

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Model

type alias Model =
  { pos : (Int, Int)
  }

init : (Model, Cmd Msg)
init =
  ({pos = (0, 0)}, Cmd.none)

-- update

type Msg
  = Key Char
  | Quit

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  ( case msg of
    Key char ->
      let
        (oldX, oldY) = model.pos
      in
        { model
        | pos =
          case char of
          'a' ->
            (oldX - 1, oldY)
          's' ->
            (oldX, oldY - 1)
          'd' ->
            (oldX + 1, oldY)
          'w' ->
            (oldX, oldY + 1)
          _ ->
            (oldX, oldY)
        }
    Quit ->
      model
  , Cmd.none
  )

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
  Keyboard.presses (\code -> Key <| Char.fromCode code)

-- view

view : Model -> Html Msg
view model =
  model
  |> toString
  |> Html.text
