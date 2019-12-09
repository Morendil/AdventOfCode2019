module GoldenOldie2018Day25PartOneSpec where

import GoldenOldie2018Day25PartOne hiding (main)
import Test.Hspec

main = do
    sample1 <- readFile "GoldenOldie2018Day25Sample1.txt"
    sample2 <- readFile "GoldenOldie2018Day25Sample2.txt"
    sample3 <- readFile "GoldenOldie2018Day25Sample3.txt"
    sample4 <- readFile "GoldenOldie2018Day25Sample4.txt"
    hspec $
        describe "Four-Dimensional Adventure" $
            it "Should get the examples right" $ do
                constellations sample1 `shouldBe` 2
                constellations sample2 `shouldBe` 4
                constellations sample3 `shouldBe` 3
                constellations sample4 `shouldBe` 8
