{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Text.Trifecta
import Prelude hiding (filter)
import Data.Map

makeClaim :: Parser [(Integer, Integer)]
makeClaim = do
  _ <- char '#'
  _id <- integer
  _ <- char '@'
  _ <- char ' '
  xs <- integer
  _ <- char ','
  ys <- integer
  _ <- char ':'
  _ <- char ' '
  width <- integer
  _ <- char 'x'
  height <- integer
  let x1 = xs + 1
      y1 = ys + 1
      x2 = xs + width
      y2 = ys + height
    in return $ [(x, y) | x <- [x1..x2], y <- [y1..y2]]

countCommonRec :: Map (Integer, Integer) Integer -> [String] -> Int
countCommonRec acc [] = size $ filter (> 1) acc
countCommonRec acc (x:xs) =
  case parseString makeClaim mempty x of
    Success a ->
      let claimSet = fromList [(k, 1) | k <- a]
      in countCommonRec (unionWith (+) acc claimSet) xs
    Failure _ -> error "could not parse input!"

countCommon :: [String] -> Int
countCommon = countCommonRec empty
