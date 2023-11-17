module Day03 (day03) where

import Common
import Data.HashMap.Strict qualified as M
import Text.Parsec qualified as P

day03 :: AOCSolution
day03 input = show <$> ([part1, part2 1 i] <*> pure r)
  where
    i = parseInput input
    r = foldl (\m k -> M.insertWith (+) k 1 m) M.empty . concat $ i

parseInput :: String -> [[Point2d]]
parseInput = parse' (p `P.sepBy` P.newline) id
  where
    p = do
      P.manyTill P.anyChar (P.char '@') *> P.space
      --
      a <- number <* P.char ','
      b <- number <* P.string ": "
      c <- number <* P.char 'x'
      d <- number
      pure [(a+i, b+j) | i <- [0..c - 1], j <- [0..d - 1]]

part1 :: M.HashMap Point2d Int -> Int
part1 = M.size . M.filter (>1)

part2 :: Int -> [[Point2d]] -> M.HashMap Point2d Int -> Int
part2 n (x:xs) r = if all ((==1) . (r M.!)) x
  then n
  else part2 (succ n) xs r
