module Day18PartOneSpec where

import Data.Tree
import Day18PartOne hiding (main)
import Test.Hspec

main = do
    sample1 <- readFile "Day18Sample1.txt"
    hspec $
        describe "Many-Worlds Interpretation" $ do
            it "Should parse the maze to a tree" $ do
                (levels $ toTree sample1) `shouldBe` [[('@',0)],[('A',2),('a',2)],[('b',2)]]
            it "Should cost the maze" $ do
                (cost $ toTree sample1) `shouldBe` 8
