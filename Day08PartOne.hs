module Day08PartOne where

import Common
import Data.List

layers :: Int -> Int -> String -> [[String]]
layers width height raw = map (chunks width) $ chunks (width*height) raw

checksum :: [[String]] -> Int
checksum layers = count1 * count2
  where min0 = head $ sortOn (count '0') $ layers
        count1 = count '1' min0
        count2 = count '2' min0
        count c = sum . map (length . filter (== c))

main = do
    contents <- readFile "Day08.txt"
    print $ checksum $ layers 25 6 contents