module SecondPlateau exposing (..)

import Char
import Collage
import Element
import Html exposing (Html)
import Html.App as App
import Keyboard
import String
import Text

import SecondPlateau.Controller as Controller exposing (Controller)

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Model

type alias Model =
  { controller : Controller
  , state : State
  }

type State
  = Default
  | Attacking

init : (Model, Cmd Msg)
init =
  ( { controller = Controller.new
    , state = Default
    }
  , Cmd.none
  )

-- update

type Msg
  = Key Char Bool
  | Quit

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  ( case msg of
    Key char state ->
      let
        cont = model.controller
      in
        { model
        | controller =
          case char of
          'w' ->
            Controller.analog Controller.Up state cont
          's' ->
            Controller.analog Controller.Down state cont
          'a' ->
            Controller.analog Controller.Left state cont
          'd' ->
            Controller.analog Controller.Right state cont
          _ ->
            cont
        , state =
          case char of
          ' ' ->
            Attacking
          _ ->
            Default
        }
    Quit ->
      model
  , Cmd.none
  )

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
  [ Keyboard.presses (\code -> Key (Char.fromCode code) True)
  , Keyboard.ups (\code -> Key (Char.fromCode code) True)
  ]

-- view

view : Model -> Html Msg
view model =
  let
    (x, y) = Controller.analogStick model.controller
  in
    Collage.collage 800 600
      [ model
        |> toString
        |> Text.fromString
        |> Collage.text
        |> Collage.move (toFloat x, toFloat y)
      ]
      |> Element.toHtml
