module Day5 where

import Data.Char (toUpper)

reduce :: String -> String
reduce []       = []
reduce (x:[])   = [x]
reduce (x:y:xs) =
  if (not (x == y)) && (toUpper x == y || x == toUpper y)
  then reduce xs
  else (x:(reduce (y:xs)))

streamReduce :: String -> Int
streamReduce xs =
  let rs = reduce xs
  in
    if xs == rs then length xs else streamReduce rs
