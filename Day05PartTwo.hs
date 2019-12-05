module Day05PartOne where

import IntCode

main = do
    contents <- readFile "Day05.txt"
    print $ execute [5] contents