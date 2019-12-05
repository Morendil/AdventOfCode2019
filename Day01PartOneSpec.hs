module Day01PartOneSpec where

import Test.Hspec
import Day01PartOne

main = hspec $ do
  describe "Fuel requirements" $ do
    it "Should get examples right" $ do
      fuelRequirements 12 `shouldBe` 2
      fuelRequirements 14 `shouldBe` 2
      fuelRequirements 1969 `shouldBe` 654
      fuelRequirements 100756 `shouldBe` 33583
