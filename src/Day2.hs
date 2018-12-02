module Day2 (checksum) where

import Data.List (sort, group, find)

findLen :: Int -> [String] -> Int
findLen l str =
  case find (\s -> length s == l) str of
    Nothing -> 0
    Just _  -> 1

counts :: [String] -> (Int, Int)
counts xs = (findLen 2 xs, findLen 3 xs)

checksum :: [String] -> Int
checksum xs =
  let cs = map (counts . group . sort) xs
      (count2, count3) = foldr (\(x, y) (acc2, acc3) -> (acc2 + x, acc3 + y)) (0, 0) cs
  in count2 * count3
