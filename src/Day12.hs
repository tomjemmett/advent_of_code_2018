module Day12 (day12) where

import Common
import Data.HashMap.Strict qualified as M
import Text.Parsec qualified as P

day12 :: AOCSolution
day12 input = show <$> [solve 20 i, part2 i 0 (-1, 0, 0)]
  where
    i = run $ parseInput input

parseInput :: String -> (String, M.HashMap String Char)
parseInput = parse' p id
  where
    s = P.oneOf "#."
    p = do
      i <- P.string "initial state: " *> P.manyTill s P.newline
      P.newline
      x <- P.sepBy q P.newline
      pure (i, M.fromList x)
    q = do
      k <- P.manyTill s (P.string " => ")
      v <- s
      pure (k, v)

part2 :: [String] -> Int -> (Int, Int, Int) -> Int
part2 i n (c, d, a) 
  | c == d && d == (b - a) = b + (50000000000 - n) * d
  | otherwise = part2 i (succ n) (d, b - a, b)
  where
    b = solve n i

solve :: Int -> [String] -> Int
solve n = foldr ((+) . fst) 0 . filter ((== '#') . snd) . zip [-(2 * n)..] . (!! n)

run :: (String, M.HashMap String Char) -> [String]
run (i, s) = iterate (step s) i

step :: M.HashMap String Char -> String -> String
step s i = f $ "...." ++ i ++ "...."
  where
    f xs@(_:xs')
      | length xs < 5 = []
      | otherwise = M.lookupDefault '.' x s : f xs'
      where
        x = take 5 xs

testInput =
  "initial state: #..#.#..##......###...###\n\
  \\n\
  \...## => #\n\
  \..#.. => #\n\
  \.#... => #\n\
  \.#.#. => #\n\
  \.#.## => #\n\
  \.##.. => #\n\
  \.#### => #\n\
  \#.#.# => #\n\
  \#.### => #\n\
  \##.#. => #\n\
  \##.## => #\n\
  \###.. => #\n\
  \###.# => #\n\
  \####. => #"
(i, s) = parseInput testInput

