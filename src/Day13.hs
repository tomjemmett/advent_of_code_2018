{-# OPTIONS_GHC -Wno-x-partial #-}

module Day13 (day13) where

import Common
import Control.Monad (guard)
import Data.Bifunctor (first, second)
import Data.List (foldl', sort)
import Data.HashMap.Strict qualified as M
import Data.Tuple (swap)

import Control.Monad.Writer

data Turn = TurnLeft | TurnStraight | TurnRight deriving (Show)
data Facing = FacingLeft | FacingRight | FacingUp | FacingDown deriving (Show, Eq)
type Cart = (Point2d, (Facing, Turn))
type Carts = M.HashMap Point2d (Facing, Turn)
type Track = M.HashMap Point2d Char

day13 :: AOCSolution
day13 = map show . untuplify2 . run . parseInput

run :: (Track, Carts) -> (Point2d, Point2d)
run (track, carts) = swap $ second head $ runWriter $ move track carts []

move :: Track -> Carts -> [Point2d] -> Writer [Point2d] Point2d
move track carts _ | M.size carts == 1 = pure $ fst $ head $ M.toList carts
move track carts [] = move track carts (sort $ M.keys carts)
move track carts (p:ps)
  | not $ p `M.member` carts = move track carts ps
  | p' `M.member` carts = do
      tell [p']
      move track (M.delete p' carts') ps
  | otherwise = move track (M.insert p' v' carts') ps
  where
      (p', v') = moveCart track (p, carts M.! p)
      carts' = M.delete p carts


moveCart :: Track -> Cart -> Cart
moveCart track (cart, (facing, turn)) = (cart', ft')
  where
    cart' = m facing cart
    m :: Facing -> Point2d -> Point2d
    m = \case
      FacingRight -> first succ
      FacingLeft -> first pred
      FacingDown -> second succ
      FacingUp -> second pred
    v = track M.! cart'
    ft'
      | v == '/' = case facing of
        FacingRight -> (FacingUp, turn)
        FacingLeft -> (FacingDown, turn)
        FacingDown -> (FacingLeft, turn)
        FacingUp -> (FacingRight, turn)
      | v == '\\' = case facing of
        FacingRight -> (FacingDown, turn)
        FacingLeft -> (FacingUp, turn)
        FacingDown -> (FacingRight, turn)
        FacingUp -> (FacingLeft, turn)
      | v == '+' = makeTurn facing turn
      | otherwise = (facing, turn)
    makeTurn f = \case
      TurnStraight -> (f, TurnRight)
      TurnLeft -> case f of
        FacingRight -> (FacingUp, TurnStraight)
        FacingLeft -> (FacingDown, TurnStraight)
        FacingDown -> (FacingRight, TurnStraight)
        FacingUp -> (FacingLeft, TurnStraight)
      TurnRight -> case f of
        FacingRight -> (FacingDown, TurnLeft)
        FacingLeft -> (FacingUp, TurnLeft)
        FacingDown -> (FacingLeft, TurnLeft)
        FacingUp -> (FacingRight, TurnLeft)

parseInput :: String -> (Track, Carts)
parseInput i = (track, carts')
  where
    init = M.fromList do
      (r, line) <- zip [0..] $ lines i
      (c, v) <- zip [0..] line
      guard $ v /= ' '
      pure ((c, r), v)
    carts = M.toList $ M.filter (`elem` "><^v") init
    removeCart m (p, c) = M.insert p (if c `elem` "<>" then '-' else '|') m
    toFacing = \case
      '>' -> (FacingRight, TurnLeft)
      '<' -> (FacingLeft, TurnLeft)
      '^' -> (FacingUp, TurnLeft)
      'v' -> (FacingDown, TurnLeft)
    track = foldl' removeCart init carts
    carts' = M.fromList $ map (second toFacing) carts
