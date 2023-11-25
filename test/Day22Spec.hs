module Day22Spec (spec) where

import SpecHelper

testInput = "depth: 510\ntarget: 10,10"

spec :: Spec
spec = describe "Day 22" $ do
  it "Sample" $ do
    day22 testInput `shouldBe` ["114", "45"]

  it "Actual" $ do
    withFile
      "inputs/day22.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day22 actualInput `shouldBe` ["5622", "1089"]
      )