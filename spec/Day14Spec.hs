module Day14Spec where

import Test.Hspec
import Day14

specs :: SpecWith ()
specs = describe "Day 14" $ do
  describe "Part 1" $ do
    it "works for given examples" $ do
      part1 9 `shouldBe` [5,1,5,8,9,1,6,7,7,9]
      part1 5 `shouldBe` [0,1,2,4,5,1,5,8,9,1]
      part1 18 `shouldBe` [9,2,5,1,0,7,1,0,8,5]
      part1 2018 `shouldBe` [5,9,4,1,4,2,9,8,8,2]
    it "works for puzzle input" $ do
      part1 540561 `shouldBe` [1,4,1,3,1,3,1,3,3,9]

  describe "Part 2" $ do
    it "works for given examples" $ do
      part2 [5,1,5,8,9] `shouldBe` 9
      part2 [0,1,2,4,5] `shouldBe` 5
      part2 [9,2,5,1,0] `shouldBe` 18
      part2 [5,9,4,1,4] `shouldBe` 2018
    it "works for puzzle input" $ do
      part2 [5,4,0,5,6,1] `shouldBe` 20254833
