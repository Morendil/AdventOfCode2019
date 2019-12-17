module Day17PartOne where

import IntCode
import Data.Char

main = do
    source <- readFile "Day17.txt"
    solution <- readFile "Day17Solution.txt"
    print $ execute (map (toInteger . ord) solution) source