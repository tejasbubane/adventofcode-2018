module Day2 (checksum, commonBoxId) where

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

-- part 2

match :: String -> String -> String
match [] [] = ""
match (x:xs) (y:ys) =
  if x == y
  then (x:(match xs ys))
  else match xs ys
match _ _ = error "input list lengths do not match"

-- check if only one common char between two strings
-- assuming two input lists are of equal length
similarity :: String -> String -> (String, Int)
similarity xs ys =
  let common = xs `match` ys
  in (common, length xs - length common)

commonBoxId :: [String] -> String
commonBoxId ids =
  let shiftIds = tail ids ++ [head ids]
      commonBox = find (\(_, diff) -> diff == 1) $ similarity <$> ids <*> shiftIds
  in case commonBox of
    Just (boxId, _) -> boxId
    Nothing       -> error "Box not found!"
