module Day17PartOne where

import Common
import IntCode
import Data.Char
import Data.List

main = do
    contents <- readFile "Day17Map.txt"
    let scaffold = lines contents
    let coords = concatMap (\(y, line) -> [(x,y) | x <- findIndices (== 'O') line]) $ indexed scaffold
    print $ sum $ uncurry (zipWith (*)) $ unzip $ coords
