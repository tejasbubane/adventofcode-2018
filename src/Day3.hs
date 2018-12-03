{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Text.Trifecta
import Prelude hiding (id)
import Data.Traversable (sequenceA)
import Data.Matrix

data Claim = Claim {
  id     :: Integer,
  xStart :: Integer,
  yStart :: Integer,
  xEnd   :: Integer,
  yEnd   :: Integer
} deriving (Eq, Show)

makeClaim :: Parser Claim
makeClaim = do
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
  return $ Claim {id=i, xStart=x+1, yStart=y+1, xEnd=x+width, yEnd=y+height}

makeClaims :: [String] -> [Claim]
makeClaims xs =
  case sequenceA . map (parseString makeClaim mempty) $ xs of
    Success a -> a
    Failure err -> error "could not parse input!"

-- Check point is within range
within :: Int -> Integer -> Integer -> Bool
within i x1 x2 = fromIntegral(i) >= x1 && fromIntegral(i) <= x2

claimMatrix :: Claim -> Matrix Integer
claimMatrix (Claim {xStart=xStart, yStart=yStart, xEnd=xEnd, yEnd=yEnd}) =
  matrix 1000 1000 (\(x, y) -> if within x xStart xEnd && within y yStart yEnd then 1 else 0)

commonArea :: [String] -> Matrix Integer
commonArea = foldl (+) (zero 1000 1000) . map claimMatrix . makeClaims

countCommon :: [String] -> Integer
countCommon = foldr (\x acc -> if x >= 2 then acc + 1 else acc) 0 . commonArea
