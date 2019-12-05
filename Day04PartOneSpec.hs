module Day04PartOneSpec where

import Day04PartOne hiding (main)
import Test.Hspec

main = hspec $ do
  describe "Passwords" $ do
    it "Should get examples right" $ do
      accept 111111 `shouldBe` True
      accept 223450 `shouldBe` False
      accept 123789 `shouldBe` False
    it "Should count a range" $ do
      countAccepted 111111 111119 `shouldBe` 9
    