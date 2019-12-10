module Day10PartTwo where

import Common
import Data.List
import Data.Char

asteroidCoordinates :: String -> [(Int, Int)]
asteroidCoordinates input = [(x,y) | x <- [0..width-1], y <- [0..height-1], (field !! y) !! x == '#']
  where field = lines input
        width = length $ head field
        height = length field

vaporizeOrder :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
vaporizeOrder station = concat . transpose . map (sortOn (distance station)) . groupBy (sameAngle station) . clockwiseFrom station

clockwiseFrom center field = sortOn (angle center) field
sameAngle center a b = (angle center a) == (angle center b)

angle a b = - (uncurry atan2 $ projections a b)
projections (a, b) (c, d) = (fromIntegral (c-a), fromIntegral (d-b))
distance (a, b) (c, d) = abs (c-a) + abs (d-b)

display :: [String] -> [(Int,Int)] -> [String]
display lines targets = foldl insert lines (indexed targets)
  where insert lines (index, (x,y)) = replace y (replace x (intToDigit (index+1)) (lines !! y)) lines

main = do
    contents <- readFile "Day10.txt"
    print $ ((vaporizeOrder (27, 19)) (asteroidCoordinates contents \\ [(27,19)])) !! 199
