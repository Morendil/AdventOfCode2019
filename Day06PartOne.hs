module Day06PartOne where

import Common
import Data.Maybe
import Data.Char
import Data.List
import Data.Tree
import Text.ParserCombinators.ReadP

parsePair :: ReadP (String, String)
parsePair = do
    one <- ident
    char ')'
    two <- ident
    return (one, two)
  where ident = many1 (satisfy isAlphaNum)

orbits :: [(String, String)] -> Int
orbits pairs = sum $ zipWith (\index leaves -> index * (length leaves)) [0..count] levs
  where levs = levels $ tree pairs
        count = length levs

root :: [(String, String)] -> String
root pairs = head $ ((nub . map fst) pairs) \\ ((nub . map snd) pairs)

tree :: [(String, String)] -> Tree String
tree pairs = unfoldTree (\origin -> (origin, map snd $ filter (\(a,b) -> a == origin) pairs)) (root pairs)

main = do
    contents <- readFile "Day06.txt"
    let l = lines contents
    let pairs = mapMaybe (parseMaybe parsePair) l
    print $ orbits pairs