module Day01PartOne where

import Day01

result :: IO ()
result = process fuelRequirements
        
fuelRequirements :: Integer -> Integer
fuelRequirements mass = (mass `div` 3) - 2
