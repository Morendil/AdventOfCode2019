module Day08PartTwoSpec where

import Day08PartTwo hiding (main)
import Test.Hspec

main = hspec $
    describe "Password image" $
        it "should decode the image" $
            (decode $ layers 2 2 "0222112222120000") `shouldBe` [".*","*."]
