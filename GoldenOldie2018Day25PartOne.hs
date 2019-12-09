module GoldenOldie2018Day25PartOne where

import Common
import Data.Maybe
import Data.List

type Star = [Integer]

constellations :: String -> Int
constellations = length . linkConstellations . parse

linkConstellations :: [Star] -> [[Star]]
linkConstellations (x:xs) = link [] [] [x] xs

link :: [[Star]] -> [Star] -> [Star] -> [Star] -> [[Star]]
link all [] [] [] = all
link all current [] [] = current:all
link all current [] (x:xs) = link (current:all) [] [x] xs
link all current (x:xs) stars = link all (x:current) (xs++candidates) stars'
  where linkable star = distance x star <= 3
        candidates = filter linkable stars
        stars' = filter (not.linkable) stars
        distance [x1,y1,z1,t1] [x2,y2,z2,t2] = (abs (x2-x1))+(abs (y2-y1))+(abs (z2-z1))+(abs (t2-t1))

parse :: String -> [Star]
parse = mapMaybe (parseMaybe numberList) . lines

main = do
    contents <- readFile "GoldenOldie2018Day25.txt"
    print $ constellations contents