module Day1 (frequency, duplicate) where

import Data.Set

-- part 1
frequency :: [Integer] -> Integer
frequency = sum

-- part 2
findDuplicate :: Integer -> Set Integer -> [Integer] -> Integer
findDuplicate _ _ [] = undefined
findDuplicate currentSum s (x:xs) =
  if currentSum `member` s
  then currentSum
  else findDuplicate (currentSum + x) (insert currentSum s) xs

duplicate :: [Integer] -> Integer
duplicate = findDuplicate 0 empty . cycle
