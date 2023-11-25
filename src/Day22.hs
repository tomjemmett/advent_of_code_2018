module Day22 (day22) where

import Common
import PathFinding
import Control.Monad.State
import Data.HashMap.Strict qualified as M
import Text.Parsec qualified as P

day22 :: AOCSolution
day22 input = show <$> ([part1, part2] <*> pure (target, g))
  where
    (depth, target) = parseInput input
    g = buildGraph depth target

parseInput :: String -> (Int, Point2d)
parseInput = parse' p id
  where
    p = do
      d <- P.string "depth: " *> number
      P.newline
      x <- P.string "target: " *> number
      y <- P.char ',' *> number
      pure (d, (x, y))

modV :: Int -> Int
modV = (`mod` 20183)

part1 :: (Point2d, M.HashMap Point2d Int) -> Int
part1 ((x, y), g) = sum . M.elems . M.filterWithKey (\k _ -> fst k <= x && snd k <= y) $ g

part2 :: (Point2d, M.HashMap Point2d Int) -> Int
part2 (target, g) = fst $ last $ unsafeFindPathWith move (manhattanDistance target . fst) ((0, 0), 1) (target, 1)
  where
    move :: (Point2d, Int) -> [(Int, (Point2d, Int))]
    move (p, e) = (7, (p, e')) : n
      where
        n = map ((1,) . (,e)) $ filter valid $ point2dNeighbours p
        valid p' = (p' `M.member` g) && (e /= g M.! p')
        e' = case g M.! p of
          0 -> if e == 1 then 2 else 1
          1 -> if e == 0 then 2 else 0
          2 -> if e == 0 then 1 else 0

buildGraph :: Int -> Point2d -> M.HashMap Point2d Int
buildGraph depth target = M.map ((`mod` 3) . el) gi
  where
    lines = concat $ getLines target
    el = modV . (+depth)
    gi = M.insert target 0 $ execState (geologicIndex el lines) M.empty

getLines :: Point2d -> [[Point2d]]
getLines (mx, my) = map points [0..mx + my]
  where
    points n = [(i, n - i) | i <- [0..n]]

geologicIndex :: (Int -> Int) -> [Point2d] -> State (M.HashMap Point2d Int) ()
geologicIndex el [p] = modify (M.insert p 0)
geologicIndex el (p@(x, y):ps) = do
  g <- get
  let v | p == (0, 0) = 0
        | x == 0 = modV (y * 48271)
        | y == 0 = modV (x * 16807)
        | otherwise = modV (el (g M.! (x-1, y)) * el (g M.! (x, y-1)))
  modify (M.insert p v)
  geologicIndex el ps

debug :: M.HashMap Point2d Int -> String
debug = point2dGridToString ' ' [[0, 0], [10, 10]]. M.map f
  where
    f 0 = '.'
    f 1 = '='
    f 2 = '|'