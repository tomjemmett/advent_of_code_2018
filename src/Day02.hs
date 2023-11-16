module Day02 (day02) where

import Common
import Data.List (find, transpose)
import Data.HashMap.Strict qualified as M

day02 :: AOCSolution
day02 input = [part1, part2] <*> pure (lines input)

part1 :: [String] -> String
part1 = show . uncurry (*) . tuplify2 . map (countTrue id) . transpose . map f . parseInput
  where
    parseInput :: [String] -> [M.HashMap Char Int]
    parseInput = map (foldr (flip (M.insertWith (+)) 1) M.empty)
    f :: M.HashMap Char Int -> [Bool]
    f x = (`elem` x) <$> [2, 3]

part2 :: [String] -> String
part2 (x:xs) = case find ((==l) . succ . length) m of
    Just r -> r
    _ -> part2 xs
  where
    l = length x
    m = map (match x) xs

match :: Eq a => [a] -> [a] -> [a]
match [] _ = []
match (a:as) (b:bs) = v
  where
    v = if a == b then a:r else r
    r = match as bs
