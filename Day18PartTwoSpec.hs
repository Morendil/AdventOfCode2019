module Day18PartTwopec where

import Data.Tree
import Day18PartOne hiding (main)
import Test.Hspec

main = do
    sample1 <- readFile "Day18PartTwoSample1.txt"
    sample2 <- readFile "Day18PartTwoSample2.txt"
    sample3 <- readFile "Day18PartTwoSample3.txt"
    sample4 <- readFile "Day18PartTwoSample4.txt"
    hspec $
        describe "Many-Worlds Interpretation" $ do
            it "Should get the examples right" $ do
                (bestCost $ toTree sample1) `shouldBe` 8
                (bestCost $ toTree sample2) `shouldBe` 24
                (bestCost $ toTree sample3) `shouldBe` 32
                (bestCost $ toTree sample5) `shouldBe` 72
