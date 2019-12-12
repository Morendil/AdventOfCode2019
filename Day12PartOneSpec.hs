module Day12PartOneSpec where

import Day12
import Day12PartOne hiding (main)
import Test.Hspec

main = do
    sample1 <- readFile "Day12Sample1.txt"
    sample2 <- readFile "Day12Sample2.txt"
    hspec $
        describe "The N-Body Problem" $
        it "Should get the examples right" $ do
            (gravity $ starting $ parse sample1) `shouldBe` [([-1,0,2],[3,-1,-1]),([2,-10,-7],[1,3,3]),([4,-8,8],[-3,1,-3]),([3,5,-1],[-1,-3,1])]
            (systemEnergy $ steps 10 $ starting $ parse sample1) `shouldBe` 179
            (systemEnergy $ steps 100 $ starting $ parse sample2) `shouldBe` 1940
