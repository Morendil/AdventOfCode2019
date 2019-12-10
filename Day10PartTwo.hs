module Day10PartTwo where

import Common
import Data.List
import Data.Char
import Day10
import Day10PartOne hiding (main)

vaporizeOrder :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
vaporizeOrder station field = concat $ transpose $ map (sortOn (distance station)) $ sightLines field station

display :: [String] -> [(Int,Int)] -> [String]
display lines targets = foldl insert lines (indexed targets)
  where insert lines (index, (x,y)) = replace y (replace x (intToDigit (index+1)) (lines !! y)) lines

main = do
    contents <- readFile "Day10.txt"
    let field = asteroidCoordinates contents
    print $ (vaporizeOrder (bestOf field) field) !! 199
