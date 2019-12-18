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
                (levels $ fmap strip $ toTree sample1) `shouldBe` [[('@',0)],[('a',2),('A',2)],[('b',2)]]
            it "Should list all keys" $ do
                (allKeys $ toTree sample1) `shouldBe` "ab"
                (allKeys $ toTree sample2) `shouldBe` "abcdef"
                (allKeys $ toTree sample3) `shouldBe` "abcdefg"
                (allKeys $ toTree sample4) `shouldBe` "abcdefghijklmnop"
                (allKeys $ toTree sample5) `shouldBe` "abcdefghi"
            it "Should list final keys" $ do
                (finalKeys $ toTree sample1) `shouldBe` "ab"
                (finalKeys $ toTree sample2) `shouldBe` "df"
                (finalKeys $ toTree sample3) `shouldBe` "fg"
                (finalKeys $ toTree sample4) `shouldBe` "ijklmnop"
                (finalKeys $ toTree sample5) `shouldBe` "bghi"
            it "Should compute costs" $ do
                (cost (toTree sample1) '@' 'a') `shouldBe` 2
                (cost (toTree sample1) 'a' 'b') `shouldBe` 6
            it "Should get the examples right" $ do
                -- these pass with the heuristic
                (bestCost $ toTree sample1) `shouldBe` 8
                (bestCost $ toTree sample2) `shouldBe` 86
                (bestCost $ toTree sample3) `shouldBe` 132
                (bestCost $ toTree sample5) `shouldBe` 81
                -- (bestCost $ toTree sample4) `shouldBe` 136
