module Day18PartOneSpec where

import Data.Tree
import Day18PartOne hiding (main)
import Test.Hspec

main = do
    sample1 <- readFile "Day18Sample1.txt"
    sample2 <- readFile "Day18Sample2.txt"
    sample3 <- readFile "Day18Sample3.txt"
    sample4 <- readFile "Day18Sample4.txt"
    sample5 <- readFile "Day18Sample5.txt"
    hspec $
        describe "Many-Worlds Interpretation" $ do
            it "Should parse the maze to a tree" $ do
                (levels $ toTree sample1) `shouldBe` [[('@',0)],[('A',2),('a',2)],[('b',2)]]
            it "Should list all keys" $ do
                (allKeys $ toTree sample1) `shouldBe` "ab"
                (allKeys $ toTree sample2) `shouldBe` "abcdef"
                (allKeys $ toTree sample3) `shouldBe` "abcdefg"
                (allKeys $ toTree sample4) `shouldBe` "abcdefghijklmnop"
                (allKeys $ toTree sample5) `shouldBe` "abcdefghi"
