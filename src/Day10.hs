module Day10 (day10) where

import Common
import Data.HashSet qualified as S
import Text.Parsec qualified as P

day10 :: AOCSolution
day10 input = [view p1, show p2]
  where
    i = parseInput input
    x = bounds $ map fst i
    (p1, p2) = run 0 x i

parseInput :: String -> [(Point2d, Point2d)]
parseInput = parse' (P.sepBy p P.newline) id
  where
    p = do
      p <- P.string "position=" *> q
      v <- P.string" velocity=" *> q
      pure (p, v)
    q = do
      P.char '<'
      a <- P.many P.space *> number
      P.char ','
      b <- P.many P.space *> number
      P.char '>'
      pure (a, b)

run :: Int -> [[Int]] -> [(Point2d, Point2d)] -> ([(Point2d, Point2d)], Int)
run n [[a, b], [c, d]] i
  | ((d - b) * (c - a)) > ((h - f) * (g - e)) = run (succ n) x i'
  | otherwise = (i, n)
  where
    i' = map step i
    x@[[e, f], [g, h]] = bounds $ map fst i'

step :: (Point2d, Point2d) -> (Point2d, Point2d)
step ((x, y), v@(i, j)) = ((x + i, y + j), v)

view :: [(Point2d, Point2d)] -> String
view (map fst -> xs) = line
  where
    s = S.fromList xs
    [[minX, minY], [maxX, maxY]] = bounds xs
    line = unlines
      [
        [
          if (x, y) `S.member` s then '#' else ' '
          | x <- [minX..maxX]
        ]
        | y <- [minY..maxY]
      ]
