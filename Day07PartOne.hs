module Day07PartOne where

import Data.List
import IntCode

chained :: Program -> [Integer] -> Integer
chained program sequence = go sequence 0
  where go [] result = result
        go (x:xs) lastOutput = go xs (head $ run [x, lastOutput] program)

maxSignal :: String -> Integer
maxSignal program = maximum $ map (chained $ parse program) $ permutations [0..4]

main = do
    contents <- readFile "Day07.txt"
    print $ maxSignal contents