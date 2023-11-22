module Day13Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 13" $ do
  it "Actual" $ do
    withFile
      "inputs/day13.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day13 actualInput `shouldBe` ["(91,69)", "(44,87)"]
      )