{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day16Spec where

import Test.Hspec
import Day16
import Data.Array (listArray)
import Text.Trifecta
import Text.RawString.QQ
import Data.Maybe

specs :: SpecWith ()
specs = describe "Day 16" $ do
  it "can perform basic register operations" $ do
    mulr (listArray (0,3) [3,2,1,1]) (9,2,1,2) `shouldBe` (listArray (0,3) [3,2,2,1])
    addi (listArray (0,3) [3,2,1,1]) (9,2,1,2) `shouldBe` (listArray (0,3) [3,2,2,1])
    seti (listArray (0,3) [3,2,1,1]) (9,2,1,2) `shouldBe` (listArray (0,3) [3,2,2,1])
  it "can parse a register input" $ do
    let (Success result) =  parseString parseRegister mempty "[3, 2, 1, 1]"
    result `shouldBe` (listArray (0,3) [3,2,1,1])
  it "can parse a instruction input" $ do
    let (Success result) = parseString parseInstr mempty "9 2 1 2"
    result `shouldBe` (9,2,1,2)
  it "can parse a computation" $ do
    let (Success result) = parseString parseComputation mempty exampleInput
    result `shouldBe` (listArray (0,3) [3,2,1,1], (9,2,1,2), listArray (0,3) [3,2,2,1])
  it "works for example input" $ do
    let (Success result) = parseString parseComputation mempty exampleInput
    matchCount result `shouldBe` 3
  it "works for puzzle input" $ do
    result <- parseFromFile (many parseComputation) "inputs/Day16-p1.txt"
    part1 (fromMaybe [] result) `shouldBe`596

exampleInput :: String
exampleInput = [r|
Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]
|]
