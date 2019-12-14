module Day14PartTwoSpec where

import Day14PartTwo hiding (main)
import Test.Hspec

main = do
    sample3 <- readFile "Day14Sample3.txt"
    sample4 <- readFile "Day14Sample4.txt"
    sample5 <- readFile "Day14Sample5.txt"
    hspec $
        describe "Space Stoichiometry" $ do
            it "Should get the examples right" $ do
                canProduce (parse sample3) 1000000000000 `shouldBe` 82892753
                canProduce (parse sample4) 1000000000000 `shouldBe` 5586022
                canProduce (parse sample5) 1000000000000 `shouldBe` 460664
