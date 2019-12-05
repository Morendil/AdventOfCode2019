module Day01PartTwo where

import Day01
import Day01PartOne (fuelRequirements)

result :: IO ()
result = process fullRequirements

fullRequirements :: Integer -> Integer
fullRequirements = sum .
  tail .                   -- leave out the module mass
  takeWhile (> 0) .        -- stop on negative fuel
  iterate fuelRequirements -- account for fuel for fuel, etc.
