module Day11PartOneSpec where

import IntCode
import Day11PartOne hiding (main)
import Test.Hspec

main = do
    hspec $
        describe "Space Police" $
            it "Should get tiny programs right" $ do
                painted (parse "99") empty `shouldBe` 0
                painted (parse "104,0,104,0,99") empty `shouldBe` 1
