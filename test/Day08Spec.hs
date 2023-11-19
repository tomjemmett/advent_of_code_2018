module Day08Spec (spec) where

import SpecHelper

testInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

spec :: Spec
spec = describe "Day 8" $ do
  it "Sample" $ do
    day08 testInput `shouldBe` ["138", "66"]

  it "Actual" $ do
    withFile
      "inputs/day08.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day08 actualInput `shouldBe` ["37262", "20839"]
      )