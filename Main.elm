module Main where

import Machine
import Input
import Draw
-- import Text
import Time
import Dict
import Random
import Graphics.Collage(..)
import Graphics.Element(..)
import Graphics.Input(..)
import Signal
import Debug
import Keyboard
import Color

initialState =
  let initialTMState =
        { pos   = (0, 0)
        , board = Dict.empty
        , state = 0
        }
      someTM = fst <| Random.generate (Machine.randomTM 300) (Random.initialSeed 18)
  in
  { tmState = initialTMState
  , zoom    = 1
  , pan     = (0,0)
  , tm      = someTM
  , seed    = Random.initialSeed 0
  }

type alias State =
  { tmState : Machine.TMState
  , zoom : Float
  , pan : (Float, Float)
  , tm : Machine.TM
  , seed : Random.Seed
  }

type Dir = In | Out
type Update
  = Tick Time.Time
  | Zoom (Maybe Dir)
  | Pan (Int, Int)
  | Reset Int -- Num states

resets : Signal.Channel Int
resets = Signal.channel 0

updates =
  Signal.mergeMany
  [ Signal.map Tick (Time.fps 30)
  , Signal.map Pan Input.pans
  , Signal.map (\arr -> case arr.y of {
      1 -> Zoom (Just In); -1 -> Zoom (Just Out); _ -> Zoom Nothing})
      (Signal.sampleOn (Time.fps 30) Keyboard.arrows)
  , Signal.map Reset (Signal.subscribe resets)
  ]

{-
pxToWorld = px * (1/zoom)
worldToPx
-}

stepsPerMs = 400 / 1 * Time.second
nTimes : Int -> (a -> a) -> (a -> a)
nTimes n f x =
  if n == 0 then x else nTimes (n - 1) f (f x)

update : Update -> State -> State
update u s = case u of
  Reset n -> 
    let (tm', seed') = Random.generate (Machine.randomTM n) s.seed in
    { initialState | tm <- tm', seed <- seed' }
  Tick ms         -> {s | tmState <- nTimes (ceiling (ms / stepsPerMs)) (Machine.step s.tm) s.tmState}
  Zoom Nothing    -> s
  Zoom (Just dir) ->
    {s | zoom <- case dir of {In -> s.zoom * 1.1; Out -> s.zoom / 1.1}}
  Pan (x, y)   ->
    let (px,py) = s.pan in {s | pan <- (px + s.zoom * toFloat x, py - s.zoom * toFloat y)}

w = 800
h = 800

resetButton = button (Signal.send resets 400) "Reset"

draw s =
  flow down
  [ (container (w + 5) (h + 5) middle
      (collage w h
        [ move s.pan (scale s.zoom (Draw.board (s.tmState.board))) ]))
  , resetButton
  ]

main =
  Signal.foldp update initialState updates
  |> Signal.map draw

