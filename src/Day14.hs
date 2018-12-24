module Day14 where

import qualified Data.Sequence as S

type Pos = Int
type Recipes = S.Seq Int

digits :: Int -> [Int]
digits = map (read . (:[])) . show

makeRecipes :: [Int]
makeRecipes = 3 : 7 : 1 : 0 : (go (S.fromList [3,7,1,0]) 0 1) where
  go scores p1 p2 = newRecipes ++ go newScores newP1 newP2 where
    r1 = scores `S.index` p1
    r2 = scores `S.index` p2
    newRecipes = digits $ r1 + r2
    newScores = (S.><) scores (S.fromList newRecipes)
    l = length newScores
    newP1 = (p1 + 1 + r1) `mod` l
    newP2 = (p2 + 1 + r2) `mod` l

part1 :: Int -> [Int]
part1 n = take 10 $ drop n makeRecipes
