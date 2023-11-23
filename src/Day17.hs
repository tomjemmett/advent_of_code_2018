module Day17 (day17) where

import Common
import Control.Monad (guard, when, forM_)
import Control.Monad.State
import Data.Bifunctor (first, second)
import Data.Either (isLeft)
import Data.HashMap.Strict qualified as M
import Text.Parsec qualified as P

type Grid = M.HashMap Point2d Char

day17 :: AOCSolution
day17 input = show <$> [p1, p2]
  where
    i = parseInput input
    y = minimum $ map snd $ M.keys i
    g = execState (down (500, y)) i
    s v = M.size $ M.filter (==v) g
    p2 = s '~'
    p1 = p2 + s '|'

parseInput :: String -> Grid
parseInput = M.fromList . map (,'#') . concat . parse' (p `P.sepBy` P.newline) id
  where
    p = P.choice [P.try px, py]
    px = do
      x <- P.string "x=" *> number
      y1 <- P.string ", y=" *> number
      y2 <- P.string ".." *> number
      pure [(x, y) | y <- [y1..y2]]
    py = do
      y <- P.string "y=" *> number
      x1 <- P.string ", x=" *> number
      x2 <- P.string ".." *> number
      pure [(x, y) | x <- [x1..x2]]

fall :: State Grid ()
fall = do
  g <- get
  let
    f (p:ps) = undefined
  pure ()

down :: Point2d -> State Grid ()
down p = do
  g <- get
  modify $ M.insert p '|'
  let
    maxY = maximum . map snd . M.keys $ g
    p' = second succ p
    v = M.lookupDefault '.' p' g
  check maxY p p' v
  where
    check maxY p p'@(_, y) v
      | y > maxY = pure ()
      | v == '#' = fill p
      | v == '.' = down p'
      | v == '~' = down p'
      | otherwise = pure ()

fill :: Point2d -> State Grid ()
fill p = do
  g <- get
  l <- goLR pred p
  r <- goLR succ p
  block p l r

block :: Point2d -> Either Point2d Point2d -> Either Point2d Point2d -> State Grid ()
block p (Right (xl, y)) (Right (xr, _)) = do
  forM_ [(x, y) | x <- [xl..xr]] $ \q -> modify (M.insert q '~')
  fill $ second pred p
block _ (Left p) (Right _) = do
  modify $ M.insert p '|'
  down p
block _ (Right _) (Left p) = do
  modify $ M.insert p '|'
  down p
block _ (Left pl) (Left pr) = do
  modify $ M.insert pl '|'
  modify $ M.insert pr '|'
  down pl
  down pr

goLR :: (Int -> Int) -> Point2d -> State Grid (Either Point2d Point2d)
goLR f p = do
  modify $ M.insert p '|'
  g <- get
  let
    p' = first f p
    vf = M.lookupDefault '.' p' g
    vd = M.lookupDefault '.' (second succ p') g
  if vf == '#'
    then pure (Right p)
    else if vd `elem` ".|"
      then pure (Left p')
      else goLR f p'
