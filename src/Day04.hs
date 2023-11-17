module Day04 (day04) where

import Common
import Data.Function (on)
import Data.List (sort, maximumBy, elemIndex)
import Data.Maybe (fromJust)
import Data.Time
    ( UTCTime(utctDayTime),
      defaultTimeLocale,
      parseTimeOrError,
      diffUTCTime )
import Data.HashMap.Strict qualified as M
import Text.Parsec qualified as P

data Log = GuardStart Int UTCTime | FallsAsleep UTCTime | WakesUp UTCTime deriving (Show)

day04 :: AOCSolution
day04 input = show . answer r <$> [sum, maximum]
  where
    i = parseInput input
    r = go M.empty 0 i

parseInput :: String -> [Log]
parseInput = map parseLine . sort . lines

parseLine :: String -> Log
parseLine = parse' p id
  where
    p = do
      P.char '['
      t <- parseTime <$> P.manyTill P.anyChar (P.string "] ")
      f <- P.choice $ P.try <$> [pGuard, pFallsAsleep, pWakesUp]
      pure $ f t
    pGuard = GuardStart <$> (P.string "Guard #" *> number)
    pFallsAsleep = FallsAsleep <$ P.string "falls asleep"
    pWakesUp = WakesUp <$ P.string "wakes up"

parseTime :: String -> UTCTime
parseTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M" 

answer :: [(Int, [Int])] -> ([Int] -> Int) -> Int
answer r f = g * t
  where
    (g, getMaxTime -> t) = maximumBy (compare `on` f . snd) r

getMaxTime :: [Int] -> Int
getMaxTime r = fromJust $ maximum r `elemIndex` r

go :: M.HashMap Int [Int] -> Int -> [Log] -> [(Int, [Int])]
go s _ [] = M.toList s
go s _ (GuardStart g _:xs) = go s g xs
go s g (FallsAsleep a:WakesUp b:xs) = go s' g xs
  where
    d = floor (diffUTCTime b a) `div` 60
    a' = floor (utctDayTime a) `div` 60
    t = [a' + i | i <- [0..d-1]]
    v = M.lookupDefault initTimes g s
    v' = zipWith (+) v [fromEnum (i >= a' && i < (a' + d)) | i <- [0..59]]
    s' = M.insert g v' s

initTimes :: [Int]
initTimes = repeat 0
