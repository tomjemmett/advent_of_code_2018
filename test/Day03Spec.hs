module Day03Spec (spec) where

import SpecHelper

testInput =
  "#1 @ 1,3: 4x4\n\
  \#2 @ 3,1: 4x4\n\
  \#3 @ 5,5: 2x2"

spec :: Spec
spec = describe "Day 3" $ do
  it "Sample" $ do
    day03 testInput `shouldBe` ["4", "3"]

  it "Actual" $ do
    withFile
      "inputs/day03.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day03 actualInput `shouldBe` ["118840", "919"]
      )