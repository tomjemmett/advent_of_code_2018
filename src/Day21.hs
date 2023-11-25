module Day21 (day21) where

import Common
import Control.Lens ( (&), (.~), (+~), Ixed(ix) )
import Data.Bits ( Bits((.|.), (.&.)) )
import Data.HashSet qualified as S
import Data.Vector qualified as V
import Text.Parsec qualified as P

import Debug.Trace

type Registers = V.Vector Int
registers :: Registers
registers = V.fromList $ replicate 6 0

type Instruction = (String, [Int])
type Instructions = V.Vector Instruction


day21 :: AOCSolution
day21 input = show <$> [fst $ p1 rs, p2]
  where
    i@(ip, insts) = parseInput input
    loc = snd (V.head insts) !! 2
    rs = go 0 i
    p1 = part1 ip loc
    p2 = part2 p1 S.empty 0 rs

part1 :: Int -> Int -> [Registers] -> (Int, [Registers])
part1 ip loc (r:rs) = if r V.! ip == 28
  then (r V.! 5, rs)
  else part1 ip loc rs

part2 :: ([Registers] -> (Int, [Registers])) -> S.HashSet Int -> Int -> [Registers] -> Int
part2 p1 s v rs = if v' `S.member` s
  then v
  else part2 p1 (S.insert v' s) v' rs'
  where
    (v', rs') = p1 rs

go v (i, insts) = iterate f reg
  where
    reg = registers & ix 0 .~ v
    f r = run r (insts V.! (r V.! i)) & ix i +~ 1

run :: Registers -> (String, [Int]) -> Registers
run r (inst, [a, b, c]) = case inst of
  "addr" -> run r ("addi", [a, r V.! b, c])
  "addi" -> r & ix c .~ (b + r V.! a)
  "mulr" -> run r ("muli", [a, r V.! b, c])
  "muli" -> r & ix c .~ (b * r V.! a)
  "banr" -> run r ("bani", [a, r V.! b, c])
  "bani" -> r & ix c .~ (b .&. r V.! a)
  "borr" -> run r ("bori", [a, r V.! b, c])
  "bori" -> r & ix c .~ (b .|. r V.! a)
  "setr" -> run r ("seti", [r V.! a, b, c])
  "seti" -> r & ix c .~ a
  "gtir" -> run r ("gtii", [a, r V.! b, c])
  "gtri" -> run r ("gtii", [r V.! a, b, c])
  "gtrr" -> run r ("gtii", [r V.! a, r V.! b, c])
  "eqir" -> run r ("eqii", [a, r V.! b, c])
  "eqri" -> run r ("eqii", [r V.! a, b, c])
  "eqrr" -> run r ("eqii", [r V.! a, r V.! b, c])
  -- mine
  "gtii" -> r & ix c .~ if a > b then 1 else 0
  "eqii" -> r & ix c .~ if a == b then 1 else 0

parseInput :: String -> (Int, Instructions)
parseInput = parse' p id
  where
    p = do
      r <- P.string "#ip " *> number
      P.newline
      i <- q `P.sepBy` P.newline
      pure (r, V.fromList i)
    q = do
      i <- P.letter `P.manyTill` P.char ' '
      n <- number `P.sepBy` P.char ' '
      pure (i, n)

getI = parseInput <$> readFile "inputs/day21.txt"