module Day10 where

import Data.List
import Data.Ord

asteroidCoordinates :: String -> [(Int, Int)]
asteroidCoordinates input = [(x,y) | x <- [0..width-1], y <- [0..height-1], (field !! y) !! x == '#']
  where field = lines input
        width = length $ head field
        height = length field

sightLines :: [(Int, Int)] -> (Int, Int) -> [[(Int, Int)]]
sightLines field observer = groupBy (sameAngle observer) $ clockwiseFrom observer $ (field \\ [observer])

clockwiseFrom center field = sortOn (angle center) field
sameAngle center a b = (angle center a) == (angle center b)

angle a b = - (uncurry atan2 $ projections a b)
projections (a, b) (c, d) = (fromIntegral (c-a), fromIntegral (d-b))
distance (a, b) (c, d) = abs (c-a) + abs (d-b)
