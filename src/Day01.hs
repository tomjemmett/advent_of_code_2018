module Day01 (day01) where

import Common
import Data.HashSet qualified as S

day01 :: AOCSolution
day01 input = show <$> [sum i, part2 (cycle i) 0 S.empty]
  where
    i :: [Int]
    i = map p $ lines input
    p :: String -> Int
    p ('+':xs) = p xs
    p xs = read xs

part2 :: [Int] -> Int -> S.HashSet Int -> Int
part2 (x:xs) n ys = if n `S.member` ys then n else part2 xs (x + n) (S.insert n ys)