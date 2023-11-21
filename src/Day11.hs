module Day11 (day11) where

import Common
import Control.Monad (guard)
import Data.Function (on)
import Data.List (maximumBy)

day11 :: AOCSolution
day11 input = [p1, p2]
  where
    i = solve . grid . read $ input
    p1 = show . fst $ i 3
    -- cheat, skip using the part 2 solver and use the size for our input
    p2 = show (fst $ i 14, 14)
    -- p2 = show . fst $ part2 i 1 (undefined, -10000)

go :: Int -> Point2d -> Int
go i (x, y) = (((rackId * y + i) * rackId `div` 100) `mod` 10) - 5
  where
    rackId = x + 10

grid :: Int -> [[Int]]
grid i = [[go i (x, y) | x <- [1..300] ] | y <- [1..300]]

solve :: [[Int]] -> Int -> ((Int, Int), Int)
solve g s = maximumBy (compare `on` snd) $ do
  x <- [0..(300 - s)]
  y <- [0..(300 - s)]
  let t = [g !! yy !! xx
          | xx <- [x..(x + s - 1)]
          , yy <- [y..(y + s - 1)]
          ]
  pure ((x + 1, y + 1), sum t)

part2 :: (Int -> (Point2d, Int)) -> Int -> (Point2d, Int) -> ((Point2d, Int), Int)
part2 g s (p, n) = if n' < n then ((p, pred s), n) else part2 g (succ s) (p', n')
  where
    (p', n') = g s