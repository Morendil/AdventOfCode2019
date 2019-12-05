module Day02PartOneSpec where

import Day02PartOne
import Test.Hspec

main = hspec $
  describe "IntCode" $
    it "Should get examples right" $ do
      execute "1,0,0,0,99" `shouldBe` [2, 0, 0, 0, 99]
      execute "2,3,0,3,99" `shouldBe` [2, 3, 0, 6, 99]
      execute "2,4,4,5,99,0" `shouldBe` [2, 4, 4, 5, 99, 9801]
      execute "1,1,1,4,99,5,6,0,99" `shouldBe` [30, 1, 1, 4, 2, 5, 6, 0, 99]
