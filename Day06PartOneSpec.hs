module Day06PartOneSpec where

import Day06PartOne hiding (main)
import Test.Hspec

main = hspec $
    describe "Orbits" $ do
        it "Should find the root" $
            root [("Foo", "Bar")] `shouldBe` "Foo"
        it "Should count one orbit" $
            orbits [("Foo", "Bar")] `shouldBe` 1
        it "Should get the example right" $
            orbits sample `shouldBe` 42

sample = [("COM","B"),("B","C"),("C","D"),("D","E"),("E","F"),("B","G"),("G","H"),("D","I"),("E","J"),("J","K"),("K","L")]
