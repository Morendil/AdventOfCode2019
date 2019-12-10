module Day10PartOneSpec where

import Day10PartOne hiding (main)
import Test.Hspec

main = do
    sample1 <- readFile "Day10Sample1.txt"
    sample2 <- readFile "Day10Sample2.txt"
    sample3 <- readFile "Day10Sample3.txt"
    sample4 <- readFile "Day10Sample4.txt"
    sample5 <- readFile "Day10Sample5.txt"
    hspec $
        describe "Monitoring Station" $
            it "Should get the examples right" $ do
                best sample1 `shouldBe` (3,4)
                best sample2 `shouldBe` (5,8)
                best sample3 `shouldBe` (1,2)
                best sample4 `shouldBe` (6,3)
                best sample5 `shouldBe` (11,13)
