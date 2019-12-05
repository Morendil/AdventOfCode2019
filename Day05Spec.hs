module Day05Spec where

import IntCode
import Test.Hspec

main = hspec $
    describe "IntCode" $ do
    it "Should preserve early opcodes" $ do
        finalProgram "1,0,0,0,99" `shouldBe` [2, 0, 0, 0, 99]
        finalProgram "2,3,0,3,99" `shouldBe` [2, 3, 0, 6, 99]
        finalProgram "2,4,4,5,99,0" `shouldBe` [2, 4, 4, 5, 99, 9801]
        finalProgram "1,1,1,4,99,5,6,0,99" `shouldBe` [30, 1, 1, 4, 2, 5, 6, 0, 99]
    it "Should use new opcodes (Part 1) and wire inputs/outputs" $ do
        step (0, [3, 0, 4, 0, 99], [12], []) `shouldBe` (2, [12, 0, 4, 0, 99], [12], [])
        step (2, [12, 0, 4, 0, 99], [12], []) `shouldBe` (4, [12, 0, 4, 0, 99], [12], [12])
        step (4, [12, 0, 4, 0, 99], [12], [12]) `shouldBe` (4, [12, 0, 4, 0, 99], [12], [12])
        execute [12] "3,0,4,0,99" `shouldBe` [12]
    it "Should recognize parameter modes" $ do
        step (0, [1002, 4, 3, 4, 33], [12], []) `shouldBe` (4, [1002, 4, 3, 4, 99], [12], [])
        step (0, [102, 3, 4, 4, 33], [12], []) `shouldBe` (4, [102, 3, 4, 4, 99], [12], [])
        step (0, [4, 0, 99], [], []) `shouldBe` (2, [4, 0, 99], [], [4])
        step (0, [104, 0, 99], [], []) `shouldBe` (2, [104, 0, 99], [], [0])
    it "Should use new opcodes (Part 2)" $ do
        step (0, [1105, 0, 0, 99], [], []) `shouldBe` (3, [1105, 0, 0, 99], [], [])
        step (0, [1105, 1, 0, 99], [], []) `shouldBe` (0, [1105, 1, 0, 99], [], [])
        step (0, [1106, 0, 0, 99], [], []) `shouldBe` (0, [1106, 0, 0, 99], [], [])
        step (0, [1106, 1, 0, 99], [], []) `shouldBe` (3, [1106, 1, 0, 99], [], [])
        step (0, [1107, 0, 1, 0, 99], [], []) `shouldBe` (4, [1, 0, 1, 0, 99], [], [])
        step (0, [1107, 1, 1, 0, 99], [], []) `shouldBe` (4, [0, 1, 1, 0, 99], [], [])
        step (0, [1107, 1, 0, 0, 99], [], []) `shouldBe` (4, [0, 1, 0, 0, 99], [], [])
        step (0, [1108, 0, 1, 0, 99], [], []) `shouldBe` (4, [0, 0, 1, 0, 99], [], [])
        step (0, [1108, 1, 1, 0, 99], [], []) `shouldBe` (4, [1, 1, 1, 0, 99], [], [])
        step (0, [1108, 1, 0, 0, 99], [], []) `shouldBe` (4, [0, 1, 0, 0, 99], [], [])
    it "Should execute the longer example correctly" $ do
        execute [4] longer `shouldBe` [999]
        execute [8] longer `shouldBe` [1000]
        execute [12] longer `shouldBe` [1001]

longer = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
        