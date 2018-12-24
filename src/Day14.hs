module Day14 where

import qualified Data.Sequence as S
import Data.List (isSuffixOf, inits, tails, isPrefixOf)

type Pos = Int
type Recipes = S.Seq Int

digits :: Int -> [Int]
digits = map (read . (:[])) . show

recipes :: [Int]
recipes = 3 : 7 : 1 : 0 : (go (S.fromList [3,7,1,0]) 0 1) where
  go scores p1 p2 = newRecipes ++ go newScores newP1 newP2 where
    r1 = scores `S.index` p1
    r2 = scores `S.index` p2
    newRecipes = digits $ r1 + r2
    newScores = (S.><) scores (S.fromList newRecipes)
    l = length newScores
    newP1 = (p1 + 1 + r1) `mod` l
    newP2 = (p2 + 1 + r2) `mod` l

part1 :: Int -> [Int]
part1 n = take 10 $ drop n recipes

part2 :: [Int] -> Int
part2 rs = length $ takeWhile (\r -> not (rs `isPrefixOf` r)) (tails recipes)
