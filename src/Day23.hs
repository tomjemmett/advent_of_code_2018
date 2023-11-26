module Day23 (day23) where

import Common
import Control.Monad (when)
import Control.Monad.State
import Data.Function (on)
import Data.List (foldl', maximumBy, sort, find)
import Data.HashMap.Strict qualified as M
import Text.Parsec qualified as P

day23 :: AOCSolution
day23 input = show <$> ([part1, part2] <*> pure (parseInput input))

part1 :: [(Point3d, Int)] -> Int
part1 i = foldl' (flip f) 0 i
  where
    (pos, range) = maximumBy (compare `on` snd) i
    f (p, _) = if distance pos p <= range then succ else id


part2 :: [(Point3d, Int)] -> Int
part2 i = pred r
  where
    x = sort $
      M.toList $
      foldr (uncurry $ M.insertWith (+)) M.empty $
      concatMap (\(untuplify3 -> p, r) -> [(sum p - r, 1), (sum p + r + 1, -1)]) i
    --
    (_, _, ms) = execState (f x) (0, 0, 0)
    Just r = find (> ms) $ map fst x 
    f :: [(Int, Int)] -> State (Int, Int, Int) ()
    f [] = pure ()
    f ((p, v):xs) = do
      ((+v) -> r, m, ms) <- get
      put (r, m, ms)
      when (r > m) $ do
        put (r, r, p)
        pure()
      f xs

distance :: Point3d -> Point3d -> Int
distance (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

parseInput :: String -> [(Point3d, Int)]
parseInput = parse' (p `P.sepBy` P.newline) id
  where
    p = do
      P.string "pos="
      pos <- (P.char '<' `P.between` P.char '>') (number `P.sepBy` P.char ',')
      r <- P.string ", r=" *> number
      pure (tuplify3 pos, r)
