module Day09Spec (spec) where

import SpecHelper

testInput = "10 players; last marble is worth 1618 points"

spec :: Spec
spec = describe "Day 9" $ do
  it "Sample" $ do
    day09 testInput `shouldBe` ["8317", "74765078"]

  it "Actual" $ do
    withFile
      "inputs/day09.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day09 actualInput `shouldBe` ["370210", "3101176548"]
      )