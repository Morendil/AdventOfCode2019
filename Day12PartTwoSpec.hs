module Day12PartOneSpec where

import Day12
import Day12PartTwo hiding (main)
import Test.Hspec

main = do
    sample1 <- readFile "Day12Sample1.txt"
    sample2 <- readFile "Day12Sample2.txt"
    hspec $
        describe "The N-Body Problem" $
        it "Should get the examples right" $ do
            (cycleLength $ starting $ parse sample1) `shouldBe` 2772
            (cycleLength $ starting $ parse sample2) `shouldBe` 4686774924
