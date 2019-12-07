module GoldenOldie2018Day22Spec where
-- When one AoC a day just isn't enough and I got feels over not finishing 2018

import GoldenOldie2018Day22 hiding (main)
import Test.Hspec
import Test.QuickCheck

main = hspec $
    describe "Terrain" $ do
        it "Should compute the index base cases" $ do
            let anything = 12
            let target = (10, 10)
            index anything target (0, 0) `shouldBe` 0
            index anything target (1, 0) `shouldBe` 16807
            index anything target (0, 1) `shouldBe` 48271
            index anything target target `shouldBe` 0
        it "Should compute the erosion base cases" $ do
            let depth = 510
            let target = (10, 10)
            erosion depth target (0, 0) `shouldBe` 510
            erosion depth target (1, 0) `shouldBe` 17317
            erosion depth target (0, 1) `shouldBe` 8415
        it "Should compute the erosion recursive cases" $ do
            let depth = 510
            let target = (10, 10)
            erosion depth target (1, 1) `shouldBe` 1805
            erosion depth target target `shouldBe` 510
        it "Should compute the terrain types" $ do
            let depth = 510
            let target = (10, 10)
            terrain depth target (0, 0) `shouldBe` 0
            terrain depth target (1, 0) `shouldBe` 1
            terrain depth target (0, 1) `shouldBe` 0
            terrain depth target (1, 1) `shouldBe` 2
            terrain depth target target `shouldBe` 0
        it "Should get the example right (Part Two)" $ do
            let depth = 510
            let target = (10, 10)
            cost depth target `shouldBe` 45
