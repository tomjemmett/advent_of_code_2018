module Day17Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 17" $ do
  it "Actual" $ do
    withFile
      "inputs/day17.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day17 actualInput `shouldBe` ["26910", "22182"]
      )