{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day16Spec where

import Test.Hspec hiding (before, after)
import Day16
import Data.Array (listArray)
import Text.Trifecta
import Text.RawString.QQ
import Data.Maybe

reg :: [Int] -> Register
reg = listArray (0,3)

before :: Register
before = reg [3,2,1,1]

instruction :: Instruction
instruction = (9,2,1,2)

after :: Register
after = reg [3,2,2,1]

specs :: SpecWith ()
specs = describe "Day 16" $ do
  describe "basic register operations" $ do
    it "can add" $ do
      addr before instruction `shouldBe` (reg [3,2,3,1])
      addi before instruction `shouldBe` after
      addr (reg [1,2,1,1]) (1,2,0,3) `shouldBe` (reg [1,2,1,2])

    it "can multiply" $ do
      mulr before instruction `shouldBe` after
      muli before instruction `shouldBe` before

    it "can bitwise AND" $ do
      banr before instruction `shouldBe` (reg [3,2,0,1])
      bani before instruction `shouldBe` before
      bani (reg [0,1,3,2]) (0,1,2,1) `shouldBe` (reg [0,0,3,2])

    it "can bitwise OR" $ do
      borr before instruction `shouldBe` (reg [3,2,3,1])
      bori before instruction `shouldBe` before

    it "can set reg" $ do
      seti before instruction `shouldBe` after
      setr (reg [1,2,1,4]) (9,1,2,3) `shouldBe` (reg [1,2,1,2])

    it "can perform greater-than testing" $ do
      gtir (reg [1,2,3,4]) (9,9,2,2) `shouldBe` (reg [1,2,1,4])
      gtir (reg [1,2,3,4]) instruction `shouldBe` (reg [1,2,0,4])
      gtri (reg [1,2,3,4]) instruction `shouldBe` (reg [1,2,1,4])
      gtri (reg [1,2,3,4]) (9,1,2,2) `shouldBe` (reg [1,2,0,4])
      gtrr (reg [1,2,3,4]) instruction `shouldBe` (reg [1,2,1,4])
      gtrr (reg [1,2,3,4]) (9,1,2,2) `shouldBe` (reg [1,2,0,4])

      gtri (reg [2,1,1,1]) (4,2,1,3) `shouldBe` (reg [2,1,1,0])
      gtrr (reg [2,3,0,0]) (14,0,3,1) `shouldBe` (reg [2,1,0,0])
      gtir (reg [3,3,2,3]) (15,2,3,1) `shouldBe` (reg [3,0,2,3])

    it "can perform equality testing" $ do
      eqir (reg [1,2,3,4]) instruction `shouldBe` (reg [1,2,1,4])
      eqir (reg [1,2,3,4]) (9,1,2,2) `shouldBe` (reg [1,2,0,4])
      eqri (reg [1,2,3,4]) (9,1,2,2) `shouldBe` (reg [1,2,1,4])
      eqri (reg [1,2,3,4]) instruction `shouldBe` (reg [1,2,0,4])
      eqrr (reg [1,2,2,4]) instruction `shouldBe` (reg [1,2,1,4])
      eqrr (reg [1,2,3,4]) (9,1,2,2) `shouldBe` (reg [1,2,0,4])

  describe "part 1" $ do
    it "can parse a register input" $ do
      let (Success result) =  parseString parseRegister mempty "[3, 2, 1, 1]"
      result `shouldBe` (reg [3,2,1,1])
    it "can parse a instruction input" $ do
      let (Success result) = parseString parseInstr mempty "9 2 1 2"
      result `shouldBe` (9,2,1,2)
    it "can parse a computation" $ do
      let (Success result) = parseString parseComputation mempty exampleInput
      result `shouldBe` (reg [3,2,1,1], (9,2,1,2), reg [3,2,2,1])
    it "works for example input" $ do
      let (Success result) = parseString parseComputation mempty exampleInput
      matchOps result `shouldBe` [Addi, Mulr, Seti]
    it "works for puzzle input" $ do
      result <- parseFromFile (many parseComputation) "inputs/Day16-p1.txt"
      part1 (fromMaybe [] result) `shouldBe` 596

  describe "part 2" $ do
    it "works for test input" $ do
      samples <- parseFromFile (many parseComputation) "inputs/Day16-p1.txt"
      let codeMappings = finalizeOpcodes (opcodes $ fromMaybe [] samples)
      instrs <- parseFromFile parseInstrs "inputs/Day16-test.txt"
      (part2 codeMappings $ fromMaybe [] instrs) `shouldBe` (reg [1,0,1,0])
    it "works for puzzle input" $ do
      samples <- parseFromFile (many parseComputation) "inputs/Day16-p1.txt"
      let codeMappings = finalizeOpcodes (opcodes $ fromMaybe [] samples)
      instrs <- parseFromFile parseInstrs "inputs/Day16-p2.txt"
      (part2 codeMappings $ fromMaybe [] instrs) `shouldBe` (reg [554,2,3,554])

exampleInput :: String
exampleInput = [r|
Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]
|]
