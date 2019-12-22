module Day22PartTwoSpec where

import Day22PartOne hiding (main)
import Test.Hspec

main = do
    hspec $
        describe "Slam Shuffle" $ do
            it "Should simplify equivalent shuffles" $ do
                result [Deal 7, NewStack, NewStack] [0..9] `shouldBe` [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]
                result [Deal 7] [0..9] `shouldBe` [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]
                result [NewStack, Cut (-2), Deal 7, Cut 8, Cut (-4), Deal 7, Cut 3, Deal 9, Deal 3, Cut (-1)] [0..9] `shouldBe` [9, 2, 5, 8, 1, 4, 7, 0, 3, 6]
                result [NewStack, Deal 7, Deal 7, Deal 9, Deal 3] [0..9] `shouldBe` [9, 2, 5, 8, 1, 4, 7, 0, 3, 6]
                result [NewStack, Deal (7*7*9*3), Deal 9, Deal 3] [0..9] `shouldBe` [9, 2, 5, 8, 1, 4, 7, 0, 3, 6]
