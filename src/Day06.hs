{-# OPTIONS_GHC -Wno-x-partial #-}

module Day06 (day06) where

import Common
import Control.Monad (guard)
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.List (elemIndices, sort, group, maximumBy)
import Data.Maybe (isJust, fromJust)

day06 :: AOCSolution
day06 input = show <$> ([part1, part2] <*> pure (parseInput input))

parseInput :: String -> [Point2d]
parseInput = map (tuplify2 . commaSeparatedInts) . lines

part1 :: [Point2d] -> Int
part1 i = maximum $ map snd $ filter (not . (`elem` exclude) . fst) $ zip [0..] $ map length $ group $ sort $ M.elems r
  where
    r = flood i
    exclude = isInfiniteArea r
    [[pred -> minX, pred -> minY], [succ -> maxX, succ -> maxY]] = bounds i
    --
    flood :: [Point2d] -> M.HashMap Point2d Int
    flood i = M.fromList $ do
      x <- [minX..maxX]
      y <- [minY..maxY]
      let
        p = (x, y)
        cp = closestPoint p i
      guard $ isJust cp
      pure (p, fromJust cp)
    --
    isInfiniteArea :: M.HashMap Point2d Int -> [Int]
    isInfiniteArea r = map head $ group $ sort $ do
      x <- [minX..maxX]
      y <- [minY..maxY]
      guard $ x == minX || x == maxX || y == minY || y == maxY
      let
        cp = M.lookup (x, y) r
      guard $ isJust cp
      pure $ fromJust cp
    --
    closestPoint :: Point2d -> [Point2d] -> Maybe Int
    closestPoint p i = case elemIndices m d of
      [x] -> Just x
      _ -> Nothing
      where
        d = map (manhattanDistance p) i
        m = minimum d

part2 :: [Point2d] -> Int
part2 i = flood i
  where
    flood :: [Point2d] -> Int
    flood i = length $ do
      let
        [[pred -> minX, pred -> minY], [succ -> maxX, succ -> maxY]] = bounds i
      x <- [minX..maxX]
      y <- [minY..maxY]
      let
        p = (x, y)
        d = sum $ map (manhattanDistance p) i
      guard $ d < 10000
      pure ()

bounds :: [Point2d] -> [[Int]]
bounds i = map <$> [minimum, maximum] <*> pure (map <$> [fst, snd] <*> pure i)
