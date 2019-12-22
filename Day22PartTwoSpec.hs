module Day22PartTwoSpec where

import Day22PartTwo hiding (main)
import Test.Hspec
import Test.QuickCheck
import Data.Numbers.Primes

instance Arbitrary Move where
    arbitrary =
      oneof
        [ return NewStack
        , Cut <$> arbitrary
        , Deal <$> arbitraryPrime
        ]
      where arbitraryPrime = do i <- arbitrary; return $ primes!!(abs i)

main = do
    hspec $
        describe "Slam Shuffle" $ do
            it "Should simplify equivalent shuffles (QuickCheck)" $ do
                property $ \n -> (result [Cut n, Deal 7] [0..12]) `shouldBe` (result [Deal 7, Cut (n*7)] [0..12])
            it "Should simplify equivalent shuffles (QuickCheck)" $ do
                property $ \moves -> noClash moves 13 ==> (result moves [0..12]) `shouldBe`(result (simplifyAll 13 moves) [0..12])
            it "Should simplify equivalent shuffles" $ do
                -- cancel out newstacks
                result [Deal 7, NewStack, NewStack] [0..9] `shouldBe` [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]
                result [Deal 7] [0..9] `shouldBe` [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]
                -- swap deal and newstack
                -- merge cuts
                result [NewStack, Cut (-2), Deal 7, Cut 8, Cut (-4), Deal 7, Cut 3, Deal 9, Deal 3, Cut (-1)] [0..9] `shouldBe` [9, 2, 5, 8, 1, 4, 7, 0, 3, 6]
                result [NewStack, Deal 7, Deal 7, Deal 9, Deal 3] [0..9] `shouldBe` [9, 2, 5, 8, 1, 4, 7, 0, 3, 6]
                -- merge deals
                result [NewStack, Deal (7*7*9*3)] [0..9] `shouldBe` [9, 2, 5, 8, 1, 4, 7, 0, 3, 6]
                -- cancel out newstacks with intervening deals
                result [Deal 9, NewStack, Deal 7, NewStack, Deal 3] [0..17] `shouldBe` [7,0,12,14,9,2,4,16,11,13,6,1,3,15,8,10,5,17]
                result [Deal 9, Deal 7, Cut (-7+1), Deal 3] [0..17] `shouldBe` [7,0,12,14,9,2,4,16,11,13,6,1,3,15,8,10,5,17]
                -- cancel out newstacks with intervening deals and cuts
                result [Deal 9, NewStack, Deal 7, Cut 4, NewStack, Deal 3] [0..17] `shouldBe` [11,4,16,1,13,6,8,3,15,17,10,5,7,0,12,14,9,2]
                result [Deal 9, Deal 7, Cut (-7+1-4), Deal 3] [0..17] `shouldBe` [11,4,16,1,13,6,8,3,15,17,10,5,7,0,12,14,9,2]
  where noClash moves int = (not.elem int) $ map num $ filter isDeal moves
        num (Deal n) = n