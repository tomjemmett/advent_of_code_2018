module Day14 (day14) where

import Common
import Data.Char (intToDigit, digitToInt)
import Data.Sequence qualified as S
import Data.List ( isPrefixOf, tails )

day14 :: AOCSolution
day14 input = [p1, p2]
  where
    p1 = map intToDigit $ take 10 (drop (read input) run)
    p2 = show $ substrLoc (map digitToInt input) run

run :: [Int]
run = 3 : 7 : go [0, 1] (S.fromList [3, 7])
  where
    go players x = nd ++ go players' x'
      where
        pv = map (S.index x) players
        pvs = sum pv
        nd = if pvs >= 10 then [1, pvs - 10] else [pvs]
        x' = x <> S.fromList nd
        players' = map ((`mod` length x') . succ) $ zipWith (+) players pv

substrLoc :: [Int] -> [Int] -> Int
substrLoc xs = length . takeWhile (not . (xs `isPrefixOf`)) . tails
