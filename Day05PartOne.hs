module Day05PartOne where

import IntCode

main = do
    contents <- readFile "Day05.txt"
    print $ execute [1] contents