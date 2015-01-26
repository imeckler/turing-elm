import Random
import Dict

data Alphabet = O | X | A1 | A2 | A3 | A4

type alias AlphabetsWorth a = (a,a,a,a,a,a)
access : Alphabet -> AlphabetsWorth a -> a
access a (xo,xx,x1,x2,x3,x4) = case a of
  O  -> xo
  X  -> xx
  A1 -> x1
  A2 -> x2
  A3 -> x3
  A4 -> x4

intToAlphabet : Int -> Alphabet
intToAlphabet n = case n of
  0 -> O
  1 -> X
  2 -> A1
  3 -> A2
  4 -> A3
  5 -> A4

intToDir : Int -> Dir
intToDir n = case n of
  0 -> U
  1 -> D
  2 -> L
  3 -> R

type alias State         = Int
type alias Rule          = (Dir, State, Alphabet)
type alias RulesForState = AlphabetsWorth Rule
type alias TM            = Array RulesForState

type Dir = U | D | L | R

type Board = Dict.Dict (Int, Int) Alphabet

move m (x, y) = case m of
  U -> (x, y + 1)
  D -> (x, y - 1)
  L -> (x - 1, y)
  R -> (x + 1, y)

randomRule : Int -> Random.Generator Rule
randomRule n = Random.customGenerator (\s ->
  let (i_d, s')   = Random.generate (Random.int 0 4)  s
      (i_s, s'')  = Random.generate (Random.int 0 (n - 1)) s'
      (i_a, s''') = Random.generate (Random.int 0 6) s''
  in
  ((intToDir i_d, i_s, intToAlphabet i_a), s'''))

randomRulesForState : Int -> Random.Generator RulesForState
randomRulesForState n = Random.customGenerator (\s ->
  let ([xo,xx,x1,x2,x3,x4], s') = Random.generate (Random.list 6 (randomRule n)) s in
  ((xo,xx,x1,x2,x3,x4),s'))

randomTM n =
  Random.customGenerator (\s ->
    let (rs, s') = Random.generate (list n (randomRulesForState n)) s in
    (Array.fromList rs, s'))

type TMState = {board : Board, state : State, pos : (Int, Int)}

step : TM -> TMState -> TMState
step tm {board,pos,state} = 
  case Array.get state tm of
    Just rs ->
      let (dir, state', a') = access (Maybe.withDefault O (Dict.get pos board)) rs
      in {board = Dict.insert pos a' board, state = state', pos = move dir pos}

