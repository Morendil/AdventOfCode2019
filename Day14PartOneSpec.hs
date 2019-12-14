module Day14PartOneSpec where

import Day14PartOne hiding (main)
import Test.Hspec

main = do
    sample1 <- readFile "Day14Sample1.txt"
    sample2 <- readFile "Day14Sample2.txt"
    sample3 <- readFile "Day14Sample3.txt"
    sample4 <- readFile "Day14Sample4.txt"
    sample5 <- readFile "Day14Sample5.txt"
    hspec $
        describe "Space Stoichiometry" $ do
            it "Should be done when we have ore on hand" $ do
                fulfill (parse sample1) [(1, "ORE")] `shouldBe` [(1, "ORE")]
            it "Should expand non-ore items" $ do
                fulfillExact (parse sample1) [(1, "FUEL")] `shouldBe` [(7, "A"),(1, "E")]
            it "Should expand non-ore items, giving priority to exact matches" $ do
                fulfillExact (parse sample1) [(7, "A"),(1, "E")] `shouldBe` [(7, "A"),(7, "A"),(1, "D")]
            it "Should expand non-ore items recursively" $ do
                fulfillAllExact (parse sample1) [(1, "FUEL")] `shouldBe` [(7,"A"),(7,"A"),(7,"A"),(7,"A"),(1,"ORE")]
            it "Should aggregate similar items" $ do
                aggregate [(7,"A"),(7,"A"),(7,"A"),(7,"A"),(1,"ORE")] `shouldBe` [(28,"A"),(1,"ORE")]
            it "Should match non-exact requirements" $ do
                fulfillNonExact (parse sample1) [(28,"A"),(1,"ORE")] `shouldBe` [(-2,"A"),(30,"ORE"),(1,"ORE")]
            it "Should get the examples right" $ do
                requires (parse sample1) `shouldBe` 31
                requires (parse sample2) `shouldBe` 165
                requires (parse sample3) `shouldBe` 13312
                requires (parse sample4) `shouldBe` 180697
                requires (parse sample5) `shouldBe` 2210736
