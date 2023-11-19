module Day08 (day08) where

import Common
import Control.Monad (replicateM)
import Text.Parsec qualified as P

data Node = Node [Node] [Int] deriving (Show)

day08 :: AOCSolution
day08 input = show <$> ([part1, part2] <*> pure (parseInput input))

parseInput :: String -> Node
parseInput = parse' p id
  where
    numSpace = number <* P.many P.space
    p = do
      children <- numSpace
      meta <- numSpace
      c <- replicateM children p
      m <- replicateM meta numSpace
      pure $ Node c m

part1 :: Node -> Int
part1 (Node c m) = sum (map part1 c) + sum m

part2 :: Node -> Int
part2 (Node [] m) = sum m
part2 (Node c m) = sum $ map f m
  where
    f (pred -> i) = if i >= length c then 0 else part2 (c !! i)
