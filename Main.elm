module Main where

import Window
import Machine
import Input
import Draw
import Text
import Time
import Dict
import Random
import Graphics.Collage(..)
import Graphics.Element(..)
import Graphics.Input(..)
import Graphics.Input.Field as Field
import Signal
import Debug
import Keyboard
import Color

initialSimState = 
  let initialTMState =
        { pos   = (0, 0)
        , board = Dict.empty
        , state = 0
        }
      someTM = fst <| Random.generate (Machine.randomTM 300) (Random.initialSeed 18)
  in
  { tmState        = initialTMState
  , zoom           = 1
  , pan            = (0,0)
  , tm             = someTM
  , seed           = Random.initialSeed 0
  , time           = 0
  , seededWithTime = False
  }

initialState =
  { simState = initialSimState
  , dataField = Field.noContent
  }

type alias AppState =
  { dataField : Field.Content
  , simState  : SimulationState
  }

type alias SimulationState =
  { tmState        : Machine.TMState
  , zoom           : Float
  , pan            : (Float, Float)
  , tm             : Machine.TM
  , seed           : Random.Seed
  , seededWithTime : Bool
  , time           : Time.Time
  }

type Dir = In | Out
type SimUpdate
  = Tick (Time.Time, Time.Time)
  | Zoom (Maybe Dir)
  | Pan (Int, Int)
  | Reset Int -- Num states

type Update
  = SimUpdate SimUpdate
  | Save
  | Load
  | SetDataField Field.Content

simUpdates =
  let time = Time.timestamp (Time.fps 30) in
  Signal.mergeMany
  [ Signal.map Tick time
  , Signal.map Pan Input.pans
  , Signal.map (\arr -> case arr.y of {
      1 -> Zoom (Just In); -1 -> Zoom (Just Out); _ -> Zoom Nothing})
      (Signal.sampleOn (Time.fps 30) Keyboard.arrows)
  , Signal.map Reset (Signal.subscribe resets)
  ]

updates = Signal.mergeMany
  [ Signal.map SimUpdate simUpdates
  , Signal.map (always Save) (Signal.subscribe saves)
  , Signal.map (always Load) (Signal.subscribe loads)
  , Signal.map SetDataField  (Signal.subscribe dataFieldEntry)
  ]

stepsPerMs = 400 / 1 * Time.second
nTimes : Int -> (a -> a) -> (a -> a)
nTimes n f x =
  if n == 0 then x else nTimes (n - 1) f (f x)

updateSim : SimUpdate -> SimulationState -> SimulationState
updateSim u s = case u of
  Reset n ->
    let seed         = if s.seededWithTime then s.seed else Random.initialSeed (floor s.time)
        (tm', seed') = Random.generate (Machine.randomTM n) seed
    in
    { initialSimState | seededWithTime <- True, time <- s.time, seed <- seed', tm <- tm' }
  Tick (t, ms)    -> {s | time <- t, tmState <- nTimes (ceiling (ms / stepsPerMs)) (Machine.step s.tm) s.tmState}
  Zoom Nothing    -> s
  Zoom (Just dir) ->
    {s | zoom <- case dir of {In -> s.zoom * 1.1; Out -> s.zoom / 1.1}}
  Pan (x, y)   ->
    let (wx,wy) = s.pan
        c       = s.zoom
    in {s | pan <- (wx + c * toFloat x, wy - c * toFloat y)}

update u s = case u of
  SimUpdate su   -> {s | simState <- updateSim su s.simState}
  SetDataField c -> {s | dataField <- c}
  Save           ->
    let c = Field.noContent in {s | dataField <- {c | string <- Machine.toString s.simState.tm}}
  Load ->
    case Machine.fromString s.dataField.string of
      Ok tm -> {s | simState <- {initialSimState | tm <- tm}}
      Err e -> 
        let c = Field.noContent in {s | dataField <- {c | string <- "Could not parse machine: " ++ e}}

resets = Signal.channel 0
resetButton = button (Signal.send resets 1000) "Reset"

saves = Signal.channel ()
saveButton = button (Signal.send saves ()) "Save"

loads = Signal.channel ()
loadButton = button (Signal.send loads ()) "Load"

dataFieldEntry = Signal.channel Field.noContent

draw (w, h) s =
  let {zoom,pan,tmState} = s.simState in
  flow down
  [ (container w (h - 50) middle
      (collage (w - 5) (h - 55)
        [ scale zoom (move pan (Draw.board (tmState.board))) ]))
  , flow right [resetButton, saveButton, loadButton]
  , Field.field Field.defaultStyle (Signal.send dataFieldEntry) "" s.dataField
  ]

main =
  let s = Signal.foldp update initialState updates
  in
  Signal.map2 draw Window.dimensions s

