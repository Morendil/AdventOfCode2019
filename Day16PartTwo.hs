module Day16PartTwo where

import Common
import Data.Char

fft :: Int -> Int -> String -> String
fft phases repeats input = represent $ (iterate step (process repeats input)) !! phases

represent :: [Int] -> String
represent signal = take 8 $ map intToDigit $ drop (offset signal) signal

offset :: [Int] -> Int
offset signal = read (map intToDigit $ take 7 signal)

process :: Int -> String -> [Int]
process repeats = concat . replicate repeats . toDigits

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
    print $ fft 100 10000 contents