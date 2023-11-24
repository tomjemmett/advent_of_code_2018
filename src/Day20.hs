module Day20 (day20) where

import Common
import Data.Bifunctor (first, second)
import Data.HashSet qualified as S
import Text.Parsec qualified as P

day20 :: AOCSolution
day20 input = show <$> ([maximum, countTrue (>= 1000)] <*> pure d)
  where
    Right i = buildGraph input
    d = distances i

type Parser = P.Parsec String Point2d
buildGraph :: String -> Either P.ParseError (S.HashSet (Point2d, Point2d))
buildGraph = P.runParser ((P.char '^' `P.between` P.char '$') p) (0, 0) ""
  where
    p :: Parser (S.HashSet (Point2d, Point2d))
    p = fmap S.unions . P.many $ P.try pMove P.<|> pBranch
    pMove :: Parser (S.HashSet (Point2d, Point2d))
    pMove = do
      i <- P.getState
      m <- (`move` i) <$> P.oneOf "NESW"
      P.setState m
      S.insert (i, m) <$> p
    pBranch :: Parser (S.HashSet (Point2d, Point2d))
    pBranch = (P.char '(' `P.between` P.char ')') $ do
      i <- P.getState
      fmap S.unions . (`P.sepBy` P.char '|') $ P.setState i >> p
    move :: Char -> Point2d -> Point2d
    move = \case
      'N' -> second pred
      'E' -> first succ
      'S' -> second succ
      'W' -> first pred

distances :: S.HashSet (Point2d, Point2d) -> [Int]
distances es = go 0 S.empty (0, 0)
  where
    go :: Int -> S.HashSet Point2d  -> Point2d -> [Int]
    go n seen p = (n :) $
      concatMap (go (n + 1) (S.insert p seen)) $
      filter ((`S.member` es) . (p, )) $
      filter (not . (`S.member` seen)) $
      point2dNeighbours p