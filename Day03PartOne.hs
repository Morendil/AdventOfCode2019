module Day03PartOne where

import Day03

distance s1 s2 = minimum $ map manhattan $ crossingsFrom s1 s2
    where manhattan (x,y) = (abs x)+(abs y)

main = do
    contents <- readFile "Day03.txt"
    let l = lines contents
    print $ distance (l !! 0) (l !! 1)