module Day16 where

import Text.Trifecta
import Data.Array
import Data.Bits
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

type Register = Array Int Int
type Instruction = (Int, Int, Int, Int)
type Computation = Register -> Instruction -> Register
data OpCode =
    Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori
  | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr
  deriving (Eq, Ord, Show)

-- Parsers
parseRegister :: Parser Register
parseRegister = do
  _ <- char '['
  arr <- commaSep integer
  _ <- char ']'
  return $ listArray (0, 3) (map fromIntegral arr)

parseInstr :: Parser Instruction
parseInstr = do
  op <- decimal
  _ <- space
  a <- decimal
  _ <- space
  b <- decimal
  _ <- space
  c <- decimal
  return $ (fromIntegral op, fromIntegral a, fromIntegral b, fromIntegral c)

parseInstrs :: Parser [Instruction]
parseInstrs = (many $ parseInstr <* newline) <* eof

parseComputation :: Parser (Register, Instruction, Register)
parseComputation = do
  skipMany newline
  _ <- string "Before: "
  before <- parseRegister
  _ <- newline
  instr <- parseInstr
  _ <- newline
  _ <- string "After:  "
  after <- parseRegister
  skipMany newline
  return $ (before, instr, after)

matchOps :: (Register, Instruction, Register) -> [OpCode]
matchOps (before, instr, after) =
  let ops =
        [
          (Addr , addr), (Addi, addi), (Mulr, mulr), (Muli, muli)
        , (Banr, banr), (Bani, bani), (Borr, borr), (Bori, bori)
        , (Setr, setr), (Seti, seti), (Gtir, gtir), (Gtri, gtri), (Gtrr, gtrr)
        , (Eqir, eqir), (Eqri, eqri), (Eqrr, eqrr)
        ]
  in map fst $ filter (\(_, f) -> (f before instr) == after) ops

-- How many have more than 3 opcodes matching?
part1 :: [(Register, Instruction, Register)] -> Int
part1 = length . filter (>=3) . map (length . matchOps)

opcodes :: [(Register, Instruction, Register)] -> M.Map Int (S.Set OpCode)
opcodes xs = foldr go M.empty xs where
  go computation@(_, (op, _, _, _), _) acc =
    M.insertWith S.intersection op (S.fromList $ matchOps computation) acc

finalizeOpcodes :: M.Map Int (S.Set OpCode) -> M.Map Int OpCode
finalizeOpcodes codes = go codes M.empty where
  go remCodes acc =
    let singularCodes = filter (\(_, v) -> (S.size v) == 1) $ M.toList remCodes
        -- these codes belong to single number - hence final
    in
      if length singularCodes == 0
      then acc
      else
        let (num, opset) = head $ singularCodes
            opcode = head $ S.toList opset
            newRemCodes = M.map (S.delete opcode) remCodes
        in go newRemCodes $ M.insert num opcode acc

-- Figure out opcode numbers and run sample program
part2 :: M.Map Int OpCode -> [Instruction] -> Register
part2 codes instrs = foldl compute (listArray (0,3) [0,0,0,0]) instrs where
  compute reg instr@(op, _, _, _) =
    let computation = comp (fromMaybe undefined $ M.lookup op codes)
    in computation reg instr

comp :: OpCode -> Computation
comp Addr = addr
comp Addi = addi
comp Mulr = mulr
comp Muli = muli
comp Banr = banr
comp Bani = bani
comp Bori = bori
comp Borr = borr
comp Setr = setr
comp Seti = seti
comp Gtir = gtir
comp Gtri = gtri
comp Gtrr = gtrr
comp Eqir = eqir
comp Eqri = eqri
comp Eqrr = eqrr

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
