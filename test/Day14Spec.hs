module Day14Spec (spec) where

import SpecHelper

testInput = "2018"

spec :: Spec
spec = describe "Day 14" $ do
  it "Sample" $ do
    day14 testInput `shouldBe` ["5941429882", "86764"]

  it "Actual" $ do
    withFile
      "inputs/day14.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day14 actualInput `shouldBe` ["3138510102", "20179081"]
      )