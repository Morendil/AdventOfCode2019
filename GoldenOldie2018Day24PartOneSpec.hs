module GoldenOldie2018Day24PartOneSpec where

import Common
import Data.Maybe
import GoldenOldie2018Day24 hiding (main)
import GoldenOldie2018Day24PartOne hiding (main)
import Test.Hspec

main = do
    content <- readFile "GoldenOldie2018Day24Sample.txt"
    let groups = parse content
    hspec $
        describe "Immune wars" $ do
            it "Should compute effective power" $ do
                let agroup = fromJust $ parseMaybe group sampleGroup
                effectivePower agroup `shouldBe` 144
            it "Should assess potential damage" $
                damageMatrix groups `shouldBe` ([[76619,153238],[24725,24725]], [[185832,185832],[53820,107640]])
            it "Should compute effects of fights killing off a group" $ do
                let group1 = groups !! 2
                    group2 = groups !! 0
                attackGroup group1 group2 `shouldBe` (group2 {units = 0})
            it "Should compute effects of fights damaging a group" $ do
                let group1 = groups !! 3
                    group2 = groups !! 1
                attackGroup group1 group2 `shouldBe` (group2 {units = 905})
            it "Should compute entire fights" $ do
                (map units $ fight groups) `shouldBe` ([905, 797,4434])
            it "Should compute entire wars" $ do
                war groups `shouldBe` 5216

sampleGroup = "18 units each with 729 hit points (weak to fire; immune to cold, slashing) with an attack that does 8 radiation damage at initiative 10"
 