module Day01PartTwoSpec where

import Test.Hspec
import Day01PartTwo

main = hspec $ do
  describe "Fuel requirements" $ do
    it "Should get examples right" $ do
      fullRequirements 12 `shouldBe` 2
      fullRequirements 14 `shouldBe` 2
      fullRequirements 1969 `shouldBe` 966
      fullRequirements 100756 `shouldBe` 50346
