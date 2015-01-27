module Input where

import Signal
import Mouse
import Debug

diff : (a -> a -> b) -> b -> Signal a -> Signal b
diff f b s =
  Signal.foldp (\x' (t, y) -> 
    (Just x', case t of {Nothing -> y; Just x -> f x x'}))
    (Nothing, b)
    s
  |> Signal.map snd

pans : Signal (Int, Int)
pans =
  Signal.keepWhen Mouse.isDown (0,0)
    (diff (\(x0,y0) (x1,y1) -> (x1 - x0, y1 - y0)) (0,0) Mouse.position)
  |> Signal.map (Debug.watch "pan")

