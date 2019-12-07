module GoldenOldie2018Day23PartOneSpec where

import GoldenOldie2018Day23PartOne hiding (main)
import Test.Hspec

main = hspec $
    describe "Nanobots" $
        it "Should get the example right" $ do
            let bots = [(0,0,0,4),(1,0,0,1),(4,0,0,3),(0,2,0,1),(0,5,0,3),(0,0,3,1),(1,1,1,1),(1,1,2,1),(1,3,1,1)]
            leaderBots bots `shouldBe` 7