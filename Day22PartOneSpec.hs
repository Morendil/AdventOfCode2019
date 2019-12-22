module Day22PartOneSpec where

import Day22PartOne hiding (main)
import Test.Hspec

main = do
    hspec $
        describe "Slam Shuffle" $ do
            it "Should get the moves right" $ do
                applyMove (Deal 3) [0..9] `shouldBe` [0, 7, 4, 1, 8, 5, 2, 9, 6, 3]
                applyMove (Deal 7) [0..9] `shouldBe` [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]
                applyMove (Cut 3) [0..9] `shouldBe` [3, 4, 5, 6, 7, 8, 9, 0, 1, 2]
                applyMove (Cut (-4)) [0..9] `shouldBe` [6, 7, 8, 9, 0, 1, 2, 3, 4, 5]
            it "Should get the examples right" $ do
                result [Deal 7, NewStack, NewStack] [0..9] `shouldBe` [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]
                result [Cut 6, Deal 7, NewStack] [0..9] `shouldBe` [3, 0, 7, 4, 1, 8, 5, 2, 9, 6]
                result [Deal 7, Deal 9, Cut (-2)] [0..9] `shouldBe` [6, 3, 0, 7, 4, 1, 8, 5, 2, 9]
                result [NewStack, Cut (-2), Deal 7, Cut 8, Cut (-4), Deal 7, Cut 3, Deal 9, Deal 3, Cut (-1)] [0..9] `shouldBe` [9, 2, 5, 8, 1, 4, 7, 0, 3, 6]
