module Day19Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 19" $ do
  it "Actual" $ do
    withFile
      "inputs/day19.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day19 actualInput `shouldBe` ["1500", "18869760"]
      )