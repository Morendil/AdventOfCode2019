module Day06PartTwo where

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

contains :: String -> Tree String -> Bool
contains target = foldTree (cata target)
  where cata target label children = (target == label) || (or children)

path :: String -> Tree String -> [String]
path target = foldTree (cata target)
  where cata target label children = label : (fromMaybe [] $ find (endsWith target) children)
        endsWith target list = (not . null) list && last list == target
   
transfers :: [(String, String)] -> Int
transfers pairs = (length $ union (a \\ intersect a b) (b \\ intersect a b)) - 2
  where a = path "YOU" atree
        b = path "SAN" atree
        atree = tree pairs

root :: [(String, String)] -> String
root pairs = head $ ((nub . map fst) pairs) \\ ((nub . map snd) pairs)

tree :: [(String, String)] -> Tree String
tree pairs = unfoldTree (\origin -> (origin, map snd $ filter (\(a,b) -> a == origin) pairs)) (root pairs)

main = do
    contents <- readFile "Day06.txt"
    let l = lines contents
    let pairs = mapMaybe (parseMaybe parsePair) l
    print $ transfers pairs