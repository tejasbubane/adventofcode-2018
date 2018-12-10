module Day6Spec where

import Test.Hspec
import Day6

specs :: SpecWith ()
specs = describe "Day 6" $ do
  describe "Part 1 - safest area" $ do
    it "works for sample input" $ do
      maxArea exampleInput `shouldBe` 17
    it "works for puzzle input" $ do
      maxArea puzzleInput `shouldBe` 4754

exampleInput :: [(Int, Int)]
exampleInput =
  [ (1, 1)
  , (1, 6)
  , (8, 3)
  , (3, 4)
  , (5, 5)
  , (8, 9)
  ]

puzzleInput :: [(Int, Int)]
puzzleInput =
  [ (137, 282)
  , (229, 214)
  , (289, 292)
  , (249, 305)
  , (90, 289)
  , (259, 316)
  , (134, 103)
  , (96, 219)
  , (92, 308)
  , (269, 59)
  , (141, 132)
  , (71, 200)
  , (337, 350)
  , (40, 256)
  , (236, 105)
  , (314, 219)
  , (295, 332)
  , (114, 217)
  , (43, 202)
  , (160, 164)
  , (245, 303)
  , (339, 277)
  , (310, 316)
  , (164, 44)
  , (196, 335)
  , (228, 345)
  , (41, 49)
  , (84, 298)
  , (43, 51)
  , (158, 347)
  , (121, 51)
  , (176, 187)
  , (213, 120)
  , (174, 133)
  , (259, 263)
  , (210, 205)
  , (303, 233)
  , (265, 98)
  , (359, 332)
  , (186, 340)
  , (132, 99)
  , (174, 153)
  , (206, 142)
  , (341, 162)
  , (180, 166)
  , (152, 249)
  , (221, 118)
  , (95, 227)
  , (152, 186)
  , (72, 330)
  ]
