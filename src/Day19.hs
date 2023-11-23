module Day19 (day19) where

import Common
import Control.Lens ( (&), (.~), (+~), Ixed(ix) )
import Control.Monad (guard)
import Control.Monad.State
import Data.Bits ( Bits((.|.), (.&.)) )
import Data.Vector qualified as V
import Text.Parsec qualified as P

import Debug.Trace

type Registers = V.Vector Int
registers :: Registers
registers = V.fromList $ replicate 6 0

type Instruction = (String, [Int])
type Instructions = V.Vector Instruction

day19 :: AOCSolution
day19 input = show . solve f <$> [0, 1]
  where
    i = parseInput input
    f = findF i $ registers & ix 0 +~ 1
  
solve :: Int -> Int -> Int
solve f a = sum [n + r `div` n | n <- [1..pred s], (r `mod` n) == 0]
  where
    r = f + a * 10550400
    s = floor $ sqrt $ fromIntegral r

findF :: (Int, Instructions) -> Registers -> Int
findF (r, i) reg
  | ptr == 25 = V.maximum reg
  | otherwise = findF (r, i) reg'
    where
      ptr = reg V.! r
      x = i V.! ptr
      reg' = run reg x & ix r +~ 1

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