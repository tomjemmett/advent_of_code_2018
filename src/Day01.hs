module Day01 (day01) where

import Common
import Data.HashSet qualified as S
import Control.Monad.State (State (..), execState, get, put)

day01 :: AOCSolution
day01 input = show <$> [sum i, execState (part2 (cycle i) S.empty) 0]
  where
    i :: [Int]
    i = parseInput input

parseInput :: String -> [Int]
parseInput = map p . lines
  where
    p :: String -> Int
    p ('+':xs) = p xs
    p xs = read xs

part2 :: [Int] -> S.HashSet Int -> State Int ()
part2 (x:xs) ys = do
  s <- get
  if s `S.member` ys
    then pure ()
    else do
      put $ s + x
      part2 xs $ S.insert s ys