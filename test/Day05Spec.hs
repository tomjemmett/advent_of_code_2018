module Day05Spec (spec) where

import SpecHelper

testInput = "dabAcCaCBAcCcaDA"

spec :: Spec
spec = describe "Day 5" $ do
  it "Sample" $ do
    day05 testInput `shouldBe` ["10", "4"]

  it "Actual" $ do
    withFile
      "inputs/day05.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day05 actualInput `shouldBe` ["9116", "6890"]
      )