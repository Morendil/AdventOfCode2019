module Day04PartTwoSpec where

import Day04PartTwo hiding (main)
import Test.Hspec

main = hspec $
  describe "Passwords" $ do
    it "Should reject increasing pairs" $
      examinePair (Right (True, 1)) (5, 4) `shouldBe` Left ()
    it "Should not accept rejected passwords" $
      examinePair (Left ()) (5, 4) `shouldBe` Left ()
    it "Should recognize double candidates" $
      examinePair (Right (False, 0)) (5, 5) `shouldBe` Right (True, 5)
    it "Should disregard double candidates within larger patterns" $
      examinePair (Right (True, 1)) (1, 1) `shouldBe` Right (False, 1)
    it "Should not care about larger patterns if we already have a double" $
      examinePair (Right (True, 1)) (5, 5) `shouldBe` Right (True, 1)
    it "Should get examples right" $ do
      accept 111111 `shouldBe` False
      accept 223450 `shouldBe` False
      accept 123789 `shouldBe` False
      accept 112233 `shouldBe` True
      accept 123444 `shouldBe` False
      accept 111122 `shouldBe` True
    it "Should count a range" $ do
      countAccepted 111111 111119 `shouldBe` 0
      countAccepted 112333 112339 `shouldBe` 7
    