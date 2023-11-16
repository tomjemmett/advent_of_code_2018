module Day01Spec (spec) where

import SpecHelper

testInput = "+1\n-2\n+3\n+1"

spec :: Spec
spec = describe "Day 1" $ do
  it "Sample" $ do
    day01 testInput `shouldBe` ["3", "2"]

  it "Actual" $ do
    withFile
      "inputs/day01.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day01 actualInput `shouldBe` ["592", "241"]
      )