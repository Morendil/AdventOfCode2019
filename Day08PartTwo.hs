module Day08PartTwo where

import Common
import Data.List

layers :: Int -> Int -> String -> [[String]]
layers width height raw = map (chunks width) $ chunks (width*height) raw

decode :: [[String]] -> [String]
decode = map (map translate) . foldr1 (zipWith $ zipWith combine)

combine :: Char -> Char -> Char
combine '0' _ = '0'
combine '1' _ = '1'
combine '2' x = x

translate :: Char -> Char
translate '0' = '.'
translate '1' = '*'

main = do
    contents <- readFile "Day08.txt"
    putStrLn $ unlines $ decode $ layers 25 6 contents