module Day09 (day09) where

import Common
import Data.Maybe (fromJust)
import Data.List.PointedList.Circular ( PointedList(_focus))
import Data.List.PointedList.Circular qualified as PL
import Data.Vector.Unboxed qualified as V
import Control.Lens ( (&), (+~), Ixed(ix) )
import Data.List(foldl')

day09 :: AOCSolution
day09 input = show . go n <$> ((*) <$> [1, 100] <*> pure m)
  where
    (n, m) = parseInput input

parseInput :: String -> (Int, Int)
parseInput i = tuplify2 $ read <$> ([(!!0), (!!6)] <*> pure (words i))

go :: Int -> Int -> Int
go n m = V.maximum . fst . foldl' f (V.replicate n 0, PL.singleton 0) $ i
  where
    f (!s, !g) (!p, !x) = (s & ix p +~ pts, g')
      where
        (pts, g') = place x g
    i = zip ((`mod` n) <$> [0..]) [1..m]

place :: Int -> PointedList Int -> (Int, PointedList Int)
place x l
    | x `mod` 23 == 0 = (_focus l' + x, fromJust (PL.deleteRight l'))
    | otherwise = (0, (PL.insertLeft x . PL.moveN 2) l)
    where
      l' = PL.moveN (-7) l

