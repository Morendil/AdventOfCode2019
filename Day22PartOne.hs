module Day22PartOne where

import Common
import Data.Maybe
import Data.Tuple
import Data.List
import Text.ParserCombinators.ReadP

data Move = Cut Int | Deal Int | NewStack deriving (Eq, Show)

parseCut = do
    string "cut "
    result <- number
    return $ Cut $ fromInteger result

parseNewStack = do
    string "deal into new stack"
    return NewStack

parseDeal = do
    string "deal with increment "
    result <- number
    return $ Deal $ fromInteger result

parseShuffle = sepBy1 (choice [parseCut, parseDeal, parseNewStack]) (string "\n")

parse :: String -> [Move]
parse = fromJust . parseMaybe parseShuffle

result :: [Move] ->  [Int] -> [Int]
result moves deck = foldl (flip applyMove) deck moves

applyMove :: Move -> [Int] -> [Int]
applyMove NewStack deck = reverse deck
applyMove (Cut pos) deck = if pos >= 0 then (drop m deck) ++ (take m deck) else (drop (len-m) deck) ++ (take (len-m) deck)
  where m = (abs pos) `mod` len
        len = length deck
applyMove (Deal pos) deck = map (\p -> deck !! snd p) $ sort $ map (\n -> (n*pos `mod` len, n)) [0..(len-1)]
  where len = length deck

main = do
    contents <- readFile "Day22.txt"
    print $ elemIndex 2019 $ result (parse contents) [0..10006]