module Day5 where

import Data.Char (toLower)
import Data.List (minimum, nub)

type Unit = Char
type Polymer = [Unit]

reduce :: Polymer -> Polymer
reduce []       = []
reduce (x:[])   = [x]
reduce (x:y:xs) =
  if (not (x == y)) && (toLower x == y || x == toLower y)
  then reduce xs
  else (x:(reduce (y:xs)))

streamReduce :: Polymer -> Int
streamReduce xs =
  let rs = reduce xs
  in
    if xs == rs then length xs else streamReduce rs

-- part 2
removeUnit :: Polymer -> Unit -> Polymer
removeUnit [] _ = []
removeUnit (x:xs) u =
  if x == u || toLower x == u || x == toLower u
  then removeUnit xs u
  else (x:(removeUnit xs u))

distinctUnits :: Polymer -> Polymer
distinctUnits = nub . map toLower

streamMaxReduce :: Polymer -> Int
streamMaxReduce xs =
  minimum . map streamReduce $ map (removeUnit xs) (distinctUnits xs)
