module Day02PartOne where

import Common
import Day02

fixAndExecute = run . fix . parse
  where fix = (replace 1 12) . (replace 2 2)

execute :: String -> Program
execute = run . parse

result = do
  contents <- readFile "Day02.txt"
  print $ head $ fixAndExecute contents