module GoldenOldie2018Day23PartOneSpec where

import GoldenOldie2018Day23PartTwo hiding (main)
import Test.Hspec

main = hspec $
    describe "Nanobots" $
        it "Should get the example right" $ do
            let bots = [(10,12,12,2),(12,14,12,2),(16,12,12,4),(14,14,14,6),(50,50,50,200),(10,10,10,5)]
            hotSpotDistance bots `shouldBe` 36