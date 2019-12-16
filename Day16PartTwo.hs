module Day16PartTwo where

import Common
import Data.Char

fft :: Int -> Int -> String -> String
fft phases repeats signal = map intToDigit $ take 8 $ (iterate step signalTail) !! phases
  where signalTail = seed (offset signal) repeats $ toDigits signal

step :: [Int] -> [Int]
step = scanr1 (\a b -> (a + b) `mod` 10)

seed :: Int -> Int -> [Int] -> [Int]
seed offset repeats signal = reverse $ take (((length signal)*repeats)-offset) $ cycle $ reverse signal

offset :: String -> Int
offset signal = read $ take 7 signal

toDigits :: String -> [Int]
toDigits = map digitToInt

main = do
    signal <- readFile "Day16.txt"
    print $ fft 100 10000 signal