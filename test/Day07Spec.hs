module Day07Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 7" $ do
  it "Actual" $ do
    withFile
      "inputs/day07.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day07 actualInput `shouldBe` ["FHMEQGIRSXNWZBCLOTUADJPKVY", "917"]
      )