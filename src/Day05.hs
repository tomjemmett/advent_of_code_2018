module Day05 (day05) where

import Common
import Data.Char(toUpper)

day05 :: AOCSolution
day05 input = show <$> ([part1, part2] <*> pure input)

part1 :: String -> Int
part1 = length . foldr step ""
  where
    step x (y:ys) | x /= y && toUpper x == toUpper y = ys
    step x ys = x : ys

part2 :: String -> Int
part2 i = minimum $ f <$> x'
  where
    x = ['a'..'z']
    x' = untuplify2 <$> zip (toUpper <$> x) x
    f v = part1 $ filter (not . (`elem` v)) i