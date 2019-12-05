module Day02PartTwo where

import Common
import Day02
import Data.List (find)

executeWith (m, n) = head . run . supplyInputs
  where supplyInputs = (replace 1 m) . (replace 2 n)

inputRange = [(x, y) | x <- [0..99], y <- [0..99]]
target = 19690720

result = do
  contents <- readFile "Day02.txt"
  let parsed = parse contents
  print $ find (\inputs -> (executeWith inputs parsed) == target) inputRange
