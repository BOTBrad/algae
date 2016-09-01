module SecondPlateau exposing (..)

import Char
import Collage
import Element
import Html exposing (Html)
import Html.App as App
import Keyboard
import String
import Text
import Time exposing (Time)

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
  | Attacking Int

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
  | Tick Time
  | Quit

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  ( case msg of
    Key char state ->
      let
        cont = model.controller
      in
        case model.state of
        Default ->
          { model
          | controller =
            case char of
            'W' ->
              Controller.analog Controller.Up state cont
            'S' ->
              Controller.analog Controller.Down state cont
            'A' ->
              Controller.analog Controller.Left state cont
            'D' ->
              Controller.analog Controller.Right state cont
            _ ->
              cont
          , state =
            case char of
            ' ' ->
              Attacking 12
            _ ->
              Default
          }
        Attacking _ ->
          model
    Tick _ ->
      case model.state of
      Default ->
        model
      Attacking a ->
        { model
        | state =
          if a <= 0 then
            Default
          else
            Attacking (a - 1)
        }
    Quit ->
      model
  , Cmd.none
  )

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
  [ Keyboard.downs (\code -> Key (Char.fromCode code) True)
  , Keyboard.ups (\code -> Key (Char.fromCode code) False)
  , Time.every (200 * Time.millisecond) (\t -> Tick t)
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
