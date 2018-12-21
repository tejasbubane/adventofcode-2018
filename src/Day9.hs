module Day9 where

import qualified Data.Map as Map

type Circle = [Integer]
type Scores = Map.Map Integer Integer

-- Infinite linked list
-- ref: https://wiki.haskell.org/Tying_the_Knot
circle :: [Integer]
circle = let x = 0 : 1: y
             y = 2 : x
         in x

scoring :: Integer -> Bool
scoring x = (x `mod` 23) == 0

score :: Circle -> (Circle, Integer)
score (current:rest) =
  let list = takeWhile (/= current) rest
      (seventh:sixth:lastFive) = drop (length list - 7) list
      x = (sixth:lastFive) ++ y
      y = (current:(take (length list - 7) list)) ++ x -- tying the knot
  in (x, seventh)

arrange :: Integer -> Integer -> Scores
arrange players limit = go circle 3 3 Map.empty where
  go :: Circle -> Integer -> Integer -> Scores -> Scores
  go circle marble player scores
    | marble > fromIntegral(limit) = scores
    | scoring marble =
      let (updatedCircle, seventh) = score circle
          updatedScores = Map.insertWith (+) player (marble + seventh) scores
      in go updatedCircle (marble + 1) (next player) updatedScores
    | otherwise =
      let (current:first:second:rest) = take (fromIntegral marble) circle
          x = (marble:second:rest) ++ y
          y = (current:first:x) -- tying the knot
      in go x (marble + 1) (next player) scores

  next :: Integer -> Integer
  next x = x `mod` players + 1
