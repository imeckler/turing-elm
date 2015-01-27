module Machine where

import Basics
import List
import Json.Encode
import Json.Decode
import Random
import Dict
import Maybe
import Array

type Alphabet = O | X | A1 | A2 | A3 | A4

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
type alias TM            = Array.Array RulesForState

type Dir = U | D | L | R

type alias Board = Dict.Dict (Int, Int) Alphabet

move m (x, y) = case m of
  U -> (x, y + 1)
  D -> (x, y - 1)
  L -> (x - 1, y)
  R -> (x + 1, y)

randomRule : Int -> Random.Generator Rule
randomRule n = Random.customGenerator (\s ->
  let (i_d, s')   = Random.generate (Random.int 0 3)  s
      (i_s, s'')  = Random.generate (Random.int 0 (n - 1)) s'
      (i_a, s''') = Random.generate (Random.int 0 5) s''
  in
  ((intToDir i_d, i_s, intToAlphabet i_a), s'''))

randomRulesForState : Int -> Random.Generator RulesForState
randomRulesForState n = Random.customGenerator (\s ->
  let ([xo,xx,x1,x2,x3,x4], s') = Random.generate (Random.list 6 (randomRule n)) s in
  ((xo,xx,x1,x2,x3,x4),s'))

randomTM n =
  Random.customGenerator (\s ->
    let (rs, s') = Random.generate (Random.list n (randomRulesForState n)) s in
    (Array.fromList rs, s'))

type alias TMState = {board : Board, state : State, pos : (Int, Int)}

step : TM -> TMState -> TMState
step tm {board,pos,state} = 
  case Array.get state tm of
    Just rs ->
      let (dir, state', a') = access (Maybe.withDefault O (Dict.get pos board)) rs
      in {board = Dict.insert pos a' board, state = state', pos = move dir pos}

maybeMap3 f x y z = case x y z of
  (Just a, Just b, Just c) -> Just (f a b c)
  _                        -> Nothing

toString : TM -> String
toString = Json.Encode.encode 0 << encodeTM

fromString : String -> Result String TM
fromString = Json.Decode.decodeString decodeTM

encodeTM : TM -> Json.Encode.Value
encodeTM = Json.Encode.array << Array.map encodeRulesForState

encodeRulesForState : RulesForState -> Json.Encode.Value
encodeRulesForState (a,b,c,d,e,f) = Json.Encode.list (List.map encodeRule [a,b,c,d,e,f])

encodeRule : (Dir, State, Alphabet) -> Json.Encode.Value
encodeRule (d, s, a) = Json.Encode.list
  [ Json.Encode.string (Basics.toString d)
  , Json.Encode.int s
  , Json.Encode.string (Basics.toString a)
  ]

decodeTM : Json.Decode.Decoder TM
decodeTM = Json.Decode.array decodeRulesForState

decodeRulesForState : Json.Decode.Decoder RulesForState
decodeRulesForState = Json.Decode.tuple6 (\a b c d e f -> (a,b,c,d,e,f))
  decodeRule decodeRule decodeRule decodeRule decodeRule decodeRule 

decodeRule : Json.Decode.Decoder Rule
decodeRule = Json.Decode.tuple3 (\d s a -> (d,s,a)) decodeDir Json.Decode.int decodeAlphabet

decodeDir : Json.Decode.Decoder Dir
decodeDir = Json.Decode.string `Json.Decode.andThen` \s -> case s of
  "U" -> Json.Decode.succeed U
  "D" -> Json.Decode.succeed D
  "L" -> Json.Decode.succeed L
  "R" -> Json.Decode.succeed R
  _   -> Json.Decode.fail "Expected Dir"

decodeAlphabet : Json.Decode.Decoder Alphabet
decodeAlphabet = Json.Decode.string `Json.Decode.andThen` \s -> case s of
  "O"  -> Json.Decode.succeed O
  "X"  -> Json.Decode.succeed X
  "A1" -> Json.Decode.succeed A1
  "A2" -> Json.Decode.succeed A2
  "A3" -> Json.Decode.succeed A3
  "A4" -> Json.Decode.succeed A4

