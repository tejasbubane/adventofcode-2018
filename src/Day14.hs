module Day14 where

import qualified Data.Sequence as S
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)

type Pos = Int

digits :: Int -> [Int]
digits = map (read . (:[])) . show

part1 :: Int -> String
part1 reqRecipes = go (S.fromList [3,7,1,0]) 0 1 where
  go :: S.Seq Int -> Pos -> Pos -> String
  go scores p1 p2
    | (S.length scores) >= (reqRecipes + 10) =
      concat . map show . take 10 . drop reqRecipes . toList $ scores
    | otherwise =
        let r1 = fromMaybe (-1) $ S.lookup p1 scores
            r2 = fromMaybe (-1) $ S.lookup p2 scores
            newRecipes = S.fromList $ digits $ r1 + r2
            newScores = (S.><) scores newRecipes
            l = length newScores
            newP1 = (p1 + 1 + r1) `mod` l
            newP2 = (p2 + 1 + r2) `mod` l
        in go newScores newP1 newP2
