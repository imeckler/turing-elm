module Draw where

import Dict
import Machine(Alphabet(..), Board)
import List((::))
import Color
import Graphics.Collage(..)

magenta = Color.rgb 255 0 255

alphabetToColor : Alphabet -> Color.Color
alphabetToColor a = case a of
  O  -> Color.white
  X  -> Color.red
  A1 -> Color.green
  A2 -> Color.blue
  A3 -> Color.yellow
  A4 -> magenta

board : Board -> Form
board b = 
  group (
    Dict.foldl (\(x,y) a forms ->
      move (5 * toFloat x, 5 * toFloat y) (filled (alphabetToColor a) (square 5)) :: forms) 
      []
      b)

