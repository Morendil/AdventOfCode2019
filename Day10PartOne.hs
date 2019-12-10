module Day10PartOne where

import Day10
import Data.List
import Data.Ord

best :: String -> (Int, Int)
best input = bestOf $ asteroidCoordinates input

bestOf :: [(Int, Int)] -> (Int, Int)
bestOf field = maximumBy (comparing (seeing field)) field

seeing :: [(Int, Int)] -> (Int, Int) -> Int
seeing field observer = length $ sightLines field observer

main = do
    contents <- readFile "Day10.txt"
    let field = asteroidCoordinates contents
    let station = best contents
    let answer = seeing field station
    print $ station
    print $ answer
