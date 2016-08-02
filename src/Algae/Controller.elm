module Algae.Controller exposing
  ( Controller
  , Direction(..)
  , DPad
  , analog
  , analogStick
  , new
  )

type Controller =
  Controller ControllerState

type alias ControllerState =
  { dpad : DPad
  }

new : Controller
new = Controller
  { dpad = DPad
    { up = False
    , down = False
    , left = False
    , right = False
    }
  }

type Direction
  = Up
  | Down
  | Left
  | Right

type DPad = DPad
  { up : Bool
  , down : Bool
  , left : Bool
  , right : Bool
  }

analogStick : Controller -> (Int, Int)
analogStick (Controller c) =
  let
    (DPad dpad) = c.dpad
    x = (boolToInt dpad.up) - (boolToInt dpad.down)
    y = (boolToInt dpad.right) - (boolToInt dpad.left)
  in
    (x, y)

boolToInt : Bool -> Int
boolToInt bool =
  if bool then 1 else 0

analog : Direction -> Bool -> Controller -> Controller
analog dir state (Controller cont) =
  let
    (DPad {up, down, left, right}) = cont.dpad
    newUp = if dir == Up then state else up
    newDown = if dir == Down then state else down
    newLeft = if dir == Left then state else left
    newRight = if dir == Right then state else right
  in
    Controller { cont
    | dpad = DPad
      { up = newUp
      , down = newDown
      , left = newLeft
      , right = newRight
      }
    }
