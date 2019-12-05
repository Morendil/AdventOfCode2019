module Day03PartOneSpec where

import Day03
import Day03PartOne hiding (main)
import Test.Hspec

main = hspec $
    describe "Crossed Wires" $ do
        it "Should parse instructions" $
            parse "R8,U5,L5,D3" `shouldBe` [R 8, U 5, L 5, D 3]
        it "Should generate segments" $ do
            segments [R 8] `shouldBe` [H (0, 0, 8)]
            segments [R 8, U 5] `shouldBe` [H (0, 0, 8), V (8, 0, 5)]
            segments [R 8, U 5, L 5] `shouldBe` [H (0, 0, 8), V (8, 0, 5), H (5, 8, 3)]
        it "Should get examples right" $ do
            crossings [V (6, 3, 7)] [H (5, 3, 8)] `shouldBe` [(6,5)]
            crossingsFrom "R8,U5,L5,D3" "U7,R6,D4,L4" `shouldBe` [(3,3),(6,5)]
            distance "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83" `shouldBe` 159
            distance "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" `shouldBe` 135
