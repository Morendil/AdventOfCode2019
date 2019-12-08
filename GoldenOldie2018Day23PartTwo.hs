module GoldenOldie2018Day23PartTwo where

import Common
import GoldenOldie2018Day23
import Data.List

type Bot = (Integer, Integer, Integer, Integer)

hotSpotDistance :: [Bot] -> Int
hotSpotDistance bots = 0

main = do
    contents <- readFile "GoldenOldie2018Day23Sample.txt"
    print $ hotSpotDistance $ parse $ lines contents