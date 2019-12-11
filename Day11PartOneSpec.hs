module Day11PartOneSpec where

import Test.Hspec

main = do
    hspec $
        describe "Space Police" $
            it "Should get the example right" $ do
                "What example" `shouldBe` "What example"
