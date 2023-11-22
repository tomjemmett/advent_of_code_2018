module Day16 (day16) where

import Common
import Control.Lens
import Data.Bifunctor (second)
import Data.Bits
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (foldl')
import Data.Vector qualified as V
import Text.Parsec qualified as P

day16 :: AOCSolution
day16 input = show <$> ([part1, part2] <*> pure (parseInput input))

type D16Input = ([[(Int, [String])]], [[Int]])

type Registers = V.Vector Int
registers :: Registers
registers = V.fromList $ replicate 4 0

parseInput :: String -> D16Input
parseInput = parse' p id
  where
    p = do
      a <- P.manyTill p1 (P.string "\n")
      P.newline
      b <- pInst `P.sepBy` P.newline
      pure (a, b)
    --
    p1 = do
      P.string "Before: ["
      b <- number `P.sepBy` P.string ", "
      P.string "]"
      P.newline
      --
      i <- pInst
      P.newline
      --
      P.string "After:  ["
      a <- number `P.sepBy` P.string ", "
      P.string "]"
      P.newline
      P.newline
      --
      pure $ f b i a
    --
    pInst = number `P.sepBy` P.char ' '
    --
    f :: [Int] -> [Int] -> [Int] -> [(Int, [String])]
    f b (i:x) a= second (:[]) . (i, ) <$> filter ((== V.fromList a) . run (V.fromList b) . (,x)) instructions

instructions :: [String]
instructions = [
  "addr",
  "addi",
  "mulr",
  "muli",
  "banr",
  "bani",
  "borr",
  "bori",
  "setr",
  "seti",
  "gtir",
  "gtri",
  "gtrr",
  "eqir",
  "eqri",
  "eqrr"
  ]

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


part1 :: D16Input -> Int
part1 = countTrue ((>=3) . length) . fst

part2 :: D16Input -> Int
part2 (i, j) = V.head $ foldl' run registers $  map (\(i:x) -> (instructions M.! i, x)) j
  where
    instructions :: M.HashMap Int String
    instructions = M.fromList .
      solve .
      M.map S.fromList .
      foldr (uncurry $ M.insertWith (++)) M.empty .
      concat $ i
    solve :: M.HashMap Int (S.HashSet String) -> [(Int, String)]
    solve r | M.size r == 0 = []
    solve r = (n, i):solve r'
      where
        ((n, S.toList -> i:_):_) = M.toList $ M.filter ((==1) . S.size) r
        r' = M.delete n $ M.map (S.delete i) r
