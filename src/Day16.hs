module Day16 where

import Data.Array
import Data.Bits
import Text.Trifecta

type Register = Array Int Int
type Instruction = (Int, Int, Int, Int)
type Computation = Register -> Instruction -> Register

-- Parsers
parseRegister :: Parser Register
parseRegister = do
  _ <- char '['
  arr <- commaSep integer
  _ <- char ']'
  return $ listArray (0, 3) (map fromIntegral arr)

parseInstr :: Parser Instruction
parseInstr = do
  [op, a, b, c] <- (fmap . fmap) fromIntegral (sepBy integer whiteSpace)
  return $ (op, a, b, c)

parseComputation :: Parser (Register, Instruction, Register)
parseComputation = do
  skipMany newline
  _ <- string "Before: "
  before <- parseRegister
  _ <- newline
  instr <- parseInstr
  _ <- string "After:  "
  after <- parseRegister
  skipMany newline
  return $ (before, instr, after)

-- Matchings
matchCount :: (Register, Instruction, Register) -> Int
matchCount (before, instr, after) =
  let ops = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti,
             gtir, gtri, gtrr, eqir, eqri, eqrr]
  in length $ filter (\f -> (f before instr) == after) ops

part1 :: [(Register, Instruction, Register)] -> Int
part1 = length . filter (>=3) . map matchCount

-- Register Operations
performr :: (Int -> Int -> Int) -> Computation
performr f before (_, a, b, c) =
  let rega = before!a
      regb = before!b
      newVal = f rega regb
  in before // [(c, newVal)]

performi :: (Int -> Int -> Int) -> Computation
performi f before (_, a, b, c) =
  let rega = before!a
      newVal = f rega b
  in before // [(c, newVal)]

performir :: (Int -> Int -> Int) -> Computation
performir f before (_, a, b, c) =
  let regb = before!b
      newVal = f a regb
  in before // [(c, newVal)]

addr :: Computation
addr = performr (+)

addi :: Computation
addi = performi (+)

mulr :: Computation
mulr = performr (*)

muli :: Computation
muli = performi (*)

banr :: Computation
banr = performr (.&.)

bani :: Computation
bani = performi (.&.)

borr :: Computation
borr = performr (.|.)

bori :: Computation
bori = performi (.|.)

setr :: Computation
setr = performr const

seti :: Computation
seti = performir const

gtir :: Computation
gtir = performir (\a b -> if a > b then 1 else 0)

gtri :: Computation
gtri = performi (\a b -> if a > b then 1 else 0)

gtrr :: Computation
gtrr = performr (\a b -> if a > b then 1 else 0)

eqir :: Computation
eqir = performir (\a b -> if a == b then 1 else 0)

eqri :: Computation
eqri = performi (\a b -> if a == b then 1 else 0)

eqrr :: Computation
eqrr = performr (\a b -> if a == b then 1 else 0)
