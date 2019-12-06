module Day06PartTwoSpec where

import Day06PartTwo hiding (main)
import Test.Hspec

main = hspec $
    describe "Orbits" $ do
        it "Should compute transfers" $
            transfers [("Foo", "YOU"),("Foo","SAN")] `shouldBe` 0
        it "Should compute transfers" $
            transfers [("Qux","Bar"),("Qux","Foo"),("Bar", "YOU"),("Foo","SAN")] `shouldBe` 2
        it "Should get the example right" $
            transfers sample `shouldBe` 4

sample = [("COM","B"),("B","C"),("C","D"),("D","E"),("E","F"),("B","G"),("G","H"),("D","I"),("E","J"),("J","K"),("K","L"),("K","YOU"),("I","SAN")]
