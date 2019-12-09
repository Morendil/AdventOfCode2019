module Day09PartOne where

import IntCode

main = do
    contents <- readFile "Day09.txt"
    print $ execute [1] contents