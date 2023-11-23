{-# OPTIONS_GHC -Wno-x-partial #-}

module Day18 (day18) where

import Common
import Control.Monad.State
import Data.Array qualified as A
import Data.HashMap.Strict qualified as M
import Data.Tuple (swap)

day18 :: AOCSolution
day18 input = show . (f M.!) <$> [p1, p2]
  where
    i = parseInput input
    (p2, s) = runState (go 1 i) M.empty
    p1 = 10
    f = M.map (\x -> countTrue (=='|') x * countTrue (=='#') x) $ M.fromList $ map swap $ M.toList s

go :: Int -> A.Array Point2d Char -> State (M.HashMap String Int) Int
go n g = do
  s <- get
  case M.lookup r s of
    Nothing -> do
      modify $ M.insert r n
      go (succ n) $ A.listArray b r 
    Just i -> do
      let
        d = n - i
        ix = ((1000000000 - i) `mod` d) + i
      pure ix
  where
    r = map step (A.indices g)
    b = A.bounds g
    step p = case g A.! p of
        '.' -> if t >= 3 then '|' else '.'
        '|' -> if l >= 3 then '#' else '|'
        '#' -> if t >= 1 && l >= 1 then '#' else '.'
      where
        n = map (g A.!) $ filter (A.inRange b) $ point2dNeighboursDiags p
        t = countTrue (=='|') n
        l = countTrue (=='#') n

parseInput :: String -> A.Array Point2d Char
parseInput (lines -> i) = A.listArray ((1, 1), (nx, ny)) $ concat i
  where
    nx = length $ head i
    ny = length i
