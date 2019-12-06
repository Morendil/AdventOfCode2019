module Day06PartOne where

import Common
import Data.Maybe
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

parsePair :: ReadP (String, String)
parsePair = do
    one <- ident
    char ')'
    two <- ident
    return (one, two)
  where ident = many1 (satisfy isAlphaNum)

ancestors :: [(String, String)] -> String -> [Maybe String]
ancestors pairs individual = tail $ takeWhile isJust $ iterate (findAncestor pairs) (Just individual)
  where findAncestor pairs individual = fmap fst $ find (\(a, b) -> Just b == individual) pairs

countAncestors :: [(String, String)] -> String -> Int
countAncestors pairs individual = length $ ancestors pairs individual

orbits :: [(String, String)] -> Int
orbits pairs = sum $ map (countAncestors pairs) $ individuals pairs
  where individuals = nub . map snd

main = do
    contents <- readFile "Day06.txt"
    let l = lines contents
    let pairs = mapMaybe (parseMaybe parsePair) l
    print $ orbits pairs