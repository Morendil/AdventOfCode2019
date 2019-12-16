module Day16PartOne where

import Common
import Data.Char

fft :: Int -> String -> String
fft phases input = represent $ (iterate step (toDigits input)) !! phases

represent :: [Int] -> String
represent = take 8 . map intToDigit

step :: [Int] -> [Int]
step digits = map (apply digits) [1..(length digits)]

apply :: [Int] -> Int -> Int
apply digits n = (abs $ sum $ zipWith (*) digits (pattern n)) `mod` 10

toDigits :: String -> [Int]
toDigits = map digitToInt

pattern :: Int -> [Int]
pattern n = tail $ cycle $ concatMap (replicate n) [0, 1, 0, -1]

main = do
    contents <- readFile "Day16.txt"
    print $ fft 100 contents