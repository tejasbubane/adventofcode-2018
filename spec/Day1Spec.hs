module Day1Spec where

import Test.Hspec
import Day1

specs :: SpecWith ()
specs = describe "Day 1" $ do
  describe "part 1 - calculate resulting frequency" $ do
    it "works for example inputs" $ do
      frequency [1, -2, 3, 1] `shouldBe` 3
      frequency [1, 1, 1] `shouldBe` 3
      frequency [1, 1, -2] `shouldBe` 0
      frequency [-1, -2, -3] `shouldBe` -6
    it "works for puzzle input" $ do
      frequency puzzleInput `shouldBe` 470

puzzleInput :: [Integer]
puzzleInput = [6, -17, 16, 7, 12, 2, -7, -5, -4, -16, 2, 12, -16, -1, -12, -3, 8, -12, 8, -3, 18, -9, 1, -20, 15, -11, -18, -8, 18, -4, 10, 1, -2, 13, 12, 16, -6, 12, 2, 11, 5, 1, -14, 16, -20, -5, 20, 6, 13, 11, 3, -9, -15, 1, 19, 8, 19, -16, 19, -17, -17, 19, 2, 5, 16, 2, -4, 19, 6, -16, 15, 7, 5, -18, 19, -8, 9, 15, -3, -3, -15, 13, -1, 10, 13, 16, -4, 10, -12, 10, 17, -2, -17, 10, 17, 8, 7, 5, 17, 10, 14, -17, -13, 19, 6, -11, 10, 8, 1, -18, -5, -8, -19, 12, 12, -4, 8, -10, -9, 2, 11, -6, 23, 17, -10, 5, 9, 19, 4, -1, 6, 12, -16, 9, 6, -9, 4, -2, -14, -5, -9, 13, 10, -2, 17, 10, 13, -5, -12, 19, 10, 16, 13, -19, -17, 4, 12, 18, -14, 5, 17, -5, -4, 18, -5, -1, -18, 11, 12, 15, -9, 3, -7, 8, -16, 9, -13, -10, -11, 3, 2, 19, 21, 6, 6, 2, -5, -6, 13, -18, -15, -16, 8, -13, -14, 9, -3, -8, -11, 9, -15, -16, 4, 13, -18, 10, -3, 15, 8, -18, 13, -5, 18, 15, -20, 19, 26, 7, -4, 25, -8, -19, 13, -2, -8, 11, 22, -16, 9, -1, 20, 12, -9, 10, -8, 9, 19, 17, 10, 4, -11, 15, 5, 4, -11, 19, -7, -6, -9, -18, 2, -9, 16, -17, 21, 16, 7, 1, 11, -4, -20, 31, -1, 2, 17, -12, -12, 18, -16, 18, -6, 19, -5, 1, 11, -8, -1, -16, 15, 17, 4, 1, -8, 15, -3, 18, -9, -15, -16, -18, 7, -22, 21, -13, -19, 3, -25, 5, -8, -19, -18, -6, 12, -11, 19, 6, 19, 70, -12, -15, 3, 25, 1, 18, -12, 21, 14, 2, 6, -13, -12, 6, 3, 13, 2, 11, -9, 1, -13, 15, 15, 1, 10, -9, 5, 13, -17, 10, 3, 12, -7, -4, 6, 3, -16, 15, 13, -9, 12, 20, 10, 3, 14, 16, -1, -13, -9, 8, 2, 17, -13, 10, 1, 11, 20, 16, -14, 11, -17, -2, 20, 7, -24, -7, 14, -1, 5, 10, 26, -13, 30, 9, -6, 13, 31, 14, -1, 4, -24, -59, -10, 8, -20, -11, -14, -10, -6, -15, -3, -14, 10, 16, 2, -5, 4, -18, -13, -7, -11, -5, -7, -16, -15, -1, -10, 16, 8, -17, 12, -18, -5, -25, 17, -12, -10, -3, -10, 11, 4, -7, 19, 51, 4, 6, -5, -11, -2, 15, -6, -5, -29, -20, 6, 6, -4, -28, -13, -2, -15, -16, -64, 23, -36, -25, -21, -52, -118, -18, -28, 25, -6, 29, 26, 46, -69, -37, -75, -100, -463, -63213, 11, 13, -21, -4, 17, 29, -10, -16, 2, -16, 7, 11, -16, -9, -21, 6, -1, -8, -3, 7, -5, -18, 14, -10, 1, 7, 9, -19, 7, -18, -7, 8, -7, 10, 4, 7, 9, 18, 55, 21, 16, -13, -61, 11, -45, -35, -14, -8, -10, -12, 6, -7, 17, -2, -13, -20, 11, -13, 12, 13, -2, -8, 13, -23, -8, -9, 4, 2, 4, -12, -11, -11, -9, 11, 14, -18, -16, 19, -13, -14, 7, -6, -9, -16, -5, 15, 1, -8, 15, 15, 12, 10, 16, -18, 3, -17, -10, -16, -15, -16, 1, 4, 14, -10, -6, 12, -9, -8, -20, -7, 4, 6, -22, 10, -15, -13, 15, -12, 3, -14, -17, 18, -20, -19, 14, 2, -9, -16, 11, -19, -1, -19, -3, -12, -16, -4, -2, 7, -12, 14, 4, 14, 13, 1, 4, 10, 14, -16, -2, -19, -8, -8, -22, -23, 10, 1, -14, -17, -17, -6, 9, -16, 4, 11, -14, 11, -17, -9, -8, -17, -13, 17, -15, -8, -16, -3, -5, -8, -17, 2, 5, 2, -19, -1, -12, 1, 16, -8, -19, -18, -2, -15, 7, -11, -6, 12, -14, -18, 15, -18, -12, 10, 12, -17, -13, -18, 13, -11, 15, -9, 8, 11, -20, 17, 9, 18, -29, 3, -14, -14, 2, -12, -5, -19, -19, -16, -15, 13, -8, -3, -17, -4, -9, 17, -12, 17, -10, 22, 6, -9, 6, -5, 17, 17, -5, -16, -16, -13, -13, 5, 6, -24, -3, -16, -12, 7, 18, -4, 13, -8, 13, 8, 13, -18, -5, -14, 17, -11, 1, -21, -13, -10, -16, 4, 16, -18, 13, -19, -10, -2, 7, -23, 8, -3, -2, -36, -5, -6, 10, -19, 11, -19, 7, 17, 13, -6, 12, -3, 14, 11, 18, 7, 13, 4, 19, -1, 12, 1, -5, -6, -6, 1, 2, 7, -14, -3, -2, 33, 14, 25, 28, 8, 8, 11, 19, -6, 12, -16, -13, -9, 7, -2, 9, -19, -19, 12, -8, 26, -16, -16, 19, 18, 19, -18, 15, -10, -2, 8, -5, 1, -9, -25, 7, 16, 17, 12, 7, 3, 2, -16, 30, 4, 5, -13, -14, -16, 7, -12, 18, 6, 25, 24, 1, 9, 6, -3, -11, 20, -2, 18, 12, 18, -11, -2, 15, 8, -11, -16, -6, 20, 11, 11, 16, 2, 18, 12, -17, 12, 11, -9, 16, 8, -10, -7, 19, 2, 13, 12, -20, -25, 2, 9, -13, 19, -4, -10, -17, 7, -9, 10, 15, -13, -25, -1, -10, 13, -12, 7, -18, -7, -17, 9, -4, -17, -11, -7, -5, -1, -16, 14, 20, -13, 42, -25, -27, -39, 12, -41, 3, 19, -2, -23, -12, -48, -35, 6, -116, 16, 18, -2, 7, -11, -40, 41, -155, 5, 125, -238, -1, -63346, -9, -18, -3, -11, 13, -4, -3, -4, -3, 23, 11, 14, 12, 3, 7, 1, -18, 15, -7, -15, -1, -9, 14, -10, -22, -1, -24, -13, 6, -14, -7, -8, -7, -1, -5, 9, 6, 19, 13, 3, -19, -13, -15, -2, 15, 3, 24, 26, 72, -16, 14, 16, 17, 14, -4, -3, 10, 3, 11, 12, 15, 9, 12, 5, 11, -35, -23, 11, -10, -5, 3, -15, -15, 128514]
