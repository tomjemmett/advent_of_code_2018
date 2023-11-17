module Day06Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 6" $ do
  it "Actual" $ do
    withFile
      "inputs/day06.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day06 actualInput `shouldBe` ["5626", "46554"]
      )