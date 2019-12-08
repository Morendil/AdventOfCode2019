module GoldenOldie2018Day24PartOneSpec where

import Common
import Data.Maybe
import GoldenOldie2018Day24 hiding (main)
import GoldenOldie2018Day24PartOne hiding (main)
import Test.Hspec

main = do
    content <- readFile "GoldenOldie2018Day24Sample.txt"
    let war@(immune, infection) = parse content
    hspec $
        describe "Immune wars" $ do
            it "Should compute effective power" $ do
                let agroup = fromJust $ parseMaybe group sampleGroup
                effectivePower agroup `shouldBe` 144
            it "Should assess potential damage" $
                damageMatrix war `shouldBe` ([[76619,153238],[24725,24725]], [[185832,185832],[53820,107640]])
            it "Should compute effects of fights killing off a group" $ do
                let group1 = groups infection !! 0
                    group2 = groups immune !! 0
                attackGroup group1 group2 `shouldBe` (group2 {units = 0})
            it "Should compute effects of fights damaging a group" $ do
                let group1 = groups infection !! 1
                    group2 = groups immune !! 1
                attackGroup group1 group2 `shouldBe` (group2 {units = 905})
            it "Should compute entire fights" $ do
                let (a1after, a2after) = fight war
                (map units $ groups a1after, map units $ groups a2after) `shouldBe` ([905],[797,4434])
            
sampleGroup = "18 units each with 729 hit points (weak to fire; immune to cold, slashing) with an attack that does 8 radiation damage at initiative 10"
 