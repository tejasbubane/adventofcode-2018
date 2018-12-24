module Day14Spec where

import Test.Hspec
import Day14

specs :: SpecWith ()
specs = describe "Day 14" $ do
  describe "part 1" $ do
    it "works for given examples" $ do
      part1 9 `shouldBe` "5158916779"
      part1 5 `shouldBe` "0124515891"
      part1 18 `shouldBe` "9251071085"
      part1 2018 `shouldBe` "5941429882"

    it "works for puzzle input" $ do
      part1 540561 `shouldBe` "1413131339"
