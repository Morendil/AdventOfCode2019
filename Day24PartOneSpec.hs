module Day24PartOneSpec where

import Day24PartOne hiding (main)
import Test.Hspec

main = do
    sample1 <- readFile "Day18Sample1.txt"
    hspec $
        describe "Planet of Discord" $
            it "Should get the examples right" $
                repeating sample1 `shouldBe` 2129920
