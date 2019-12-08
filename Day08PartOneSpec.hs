module Day08PartOneSpec where

import Day08PartOne hiding (main)
import Test.Hspec

main = hspec $
    describe "Password image" $ do
        it "should parse the image into layers" $
            layers 3 2 "123456789012" `shouldBe` [["123","456"],["789","012"]]
        it "should compute the checksum" $
            (checksum $ layers 3 2 "123456789012") `shouldBe` 1
