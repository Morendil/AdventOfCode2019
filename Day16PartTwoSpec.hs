module Day16PartTwoSpec where

import Day16PartTwo hiding (main)
import Test.Hspec

main = do
    hspec $
        describe "Flawed Frequency Transmission" $
            it "Should get the examples right" $ do
                fft 100 10000 "03036732577212944063491565474664" `shouldBe` "84462026"
                fft 100 10000 "02935109699940807407585447034323" `shouldBe` "78725270"
                fft 100 10000 "03081770884921959731165446850517" `shouldBe` "53553731"
