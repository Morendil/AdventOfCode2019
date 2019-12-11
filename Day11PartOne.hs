module Day11PartOne where

import Day11
import IntCode hiding (State)
import qualified Data.HashMap.Strict as Map

painted :: Program -> State -> Int
painted program state = countPainted $ finalState program empty

countPainted :: State -> Int
countPainted (State panels _ _ ) = length $ Map.keys panels

main = do
    contents <- readFile "Day11.txt"
    print $ painted (parse contents) empty