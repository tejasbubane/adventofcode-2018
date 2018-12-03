{-# LANGUAGE OverloadedStrings #-}

module Day3 (countCommon, findIntact) where

import Text.Trifecta
import Prelude hiding (id)
import Data.Map hiding (map)

data Claim =
  Claim {
    id :: Integer
  , xLeft :: Integer
  , xRight :: Integer
  , yLeft :: Integer
  , yRight :: Integer
  }

type Point = (Integer, Integer)
type ClaimedCanvas = Map Point Integer

-- Parse single claim string into `Claim` record
parseClaim :: Parser Claim
parseClaim = do
  _ <- char '#'
  i <- integer
  _ <- char '@'
  _ <- char ' '
  x <- integer
  _ <- char ','
  y <- integer
  _ <- char ':'
  _ <- char ' '
  width <- integer
  _ <- char 'x'
  height <- integer
  let x1 = x + 1
      y1 = y + 1
      x2 = x + width
      y2 = y + height
    in return $ Claim {id=i, xLeft=x1, yLeft=y1, xRight=x2, yRight=y2}

-- parse multiple strings to claims
parseClaims :: [String] -> [Claim]
parseClaims xs =
  case sequenceA . map (parseString parseClaim mempty) $ xs of
    Success a -> a
    Failure _ -> error $ "could not parse input!"

-- Return all points within claim
pointsList :: Claim -> [Point]
pointsList Claim {xLeft=x1, yLeft=y1, xRight=x2, yRight=y2} =
  [(x, y) | x <- [x1..x2], y <- [y1..y2]]

-- Add (claim, 1) to map by adding same claim keys
-- keys with value > 1 give overlapping claims
allClaimed :: ClaimedCanvas -> [Claim] -> ClaimedCanvas
allClaimed acc [] = overlapping acc
allClaimed acc (x:xs) =
  let claimSet = fromList [(k, 1) | k <- pointsList x]
  in allClaimed (unionWith (+) acc claimSet) xs

overlapping :: ClaimedCanvas -> ClaimedCanvas
overlapping = Data.Map.filter (>1)

-- Part 1
-- Area covered by overlapping claims
-- count number of points
countCommon :: [String] -> Int
countCommon = size . overlapping . allClaimed empty . parseClaims

-- Check if point is included in claim
pointInclude :: Point -> Claim -> Bool
pointInclude (x, y) (Claim {xLeft=x1, yLeft=y1, xRight=x2, yRight=y2}) =
  x1 <= x && x <= x2 && y1 <= y && y <= y2

removeOverlapping :: [Point] -> [Claim] -> Claim
removeOverlapping [] claims = head claims
removeOverlapping (x:xs) claims =
  -- remove all claims in which (x,y) lies
  removeOverlapping xs (Prelude.filter (\c -> not $ pointInclude x c) claims)

-- Part 2
-- Find single intact claim - untouched by others
findIntact :: [String] -> Integer
findIntact xs =
  let claims = parseClaims xs
      canvas = overlapping $ allClaimed mempty claims
  in id $ removeOverlapping (keys canvas) claims
