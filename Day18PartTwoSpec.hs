module Day18PartTwopec where

import Data.Tree
import Day18PartTwo hiding (main)
import Test.Hspec

main = do
    sample1 <- readFile "Day18PartTwoSample1.txt"
    sample2 <- readFile "Day18PartTwoSample2.txt"
    sample3 <- readFile "Day18PartTwoSample3.txt"
    sample4 <- readFile "Day18PartTwoSample4.txt"
    hspec $
        describe "Many-Worlds Interpretation" $ do
            it "Should get the examples right" $ do
                (bestCost $ toTrees sample1) `shouldBe` 8
                (bestCost $ toTrees sample2) `shouldBe` 24
                (bestCost $ toTrees sample3) `shouldBe` 32
                (bestCost $ toTrees sample4) `shouldBe` 72
