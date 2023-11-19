module Day07 (day07) where

import Common ( AOCSolution )
import Control.Monad.Writer
import Data.Char (ord)
import Data.Map qualified as M
import Data.HashSet qualified as S
import Data.List (sort)

day07 :: AOCSolution
day07 input = [p1, p2]
  where
    i = parseInput input
    (_, p1) = runWriter $ run (0, 1) i
    (p2, _) = runWriter $ run (60, 5) i

parseInput :: String -> M.Map Char (S.HashSet Char)
parseInput = M.fromListWith S.union . concatMap (f . words) . lines
  where
    f :: [String] -> [(Char, S.HashSet Char)]
    f x = [(v, S.empty), (k, S.singleton v)]
      where
        (k:_) = x !! 7
        (v:_) = x !! 1

run :: (Int, Int) -> M.Map Char (S.HashSet Char) -> Writer String String
run (c, w) = go <*> (map (start 0) . take w . M.keys . M.filter S.null)
  where
    start t v = (t + c + ord v - ord 'A' + 1, v)
    go d ((t, k):r) = do
      let
        d' = M.map (S.delete k) $ M.delete k d
        o k a = k `notElem` map snd r && S.null a
        p = map (start t) $ M.keys $ M.filterWithKey o d'
        r' = sort $ r ++ take (w - length r) p
      --
      tell [k]
      --
      if M.null d' && null r'
        then pure $ show t
        else go d' r'