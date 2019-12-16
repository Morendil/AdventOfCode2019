module Day16PartOneSpec where

import Day16PartOne hiding (main)
import Test.Hspec

main = do
    hspec $
        describe "Flawed Frequency Transmission" $
            it "Should get the examples right" $ do
                fft 1 "12345678" `shouldBe` "48226158"
                fft 2 "12345678" `shouldBe` "34040438"
                fft 3 "12345678" `shouldBe` "03415518"
                fft 4 "12345678" `shouldBe` "01029498"
                fft 100 "80871224585914546619083218645595" `shouldBe` "24176176"
                fft 100 "19617804207202209144916044189917" `shouldBe` "73745418"
                fft 100 "69317163492948606335995924319873" `shouldBe` "52432133"
