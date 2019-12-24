module Day24PartTwo where

import Data.Maybe    
import Data.List
import Data.List.Tools
import qualified Data.HashMap.Strict as Map

type Position = (Int, Int, Int)
type Plutonian = Map.HashMap Int [String]

empty :: [String]
empty = replicate 5 $ replicate 5 '.'

parse :: String -> [String]
parse = lines

start :: String -> Plutonian
start input = Map.insert 0 (parse input) Map.empty

countBugs :: Plutonian -> Int
countBugs = sum . map countOne . Map.elems

countOne :: [String] -> Int
countOne = length . filter (== '#') . concat

step :: Plutonian -> Plutonian
step space = Map.fromList $ map (\k -> (k, stepOne space k $ on k)) newKeys
  where newKeys = ((minimum oldKeys)-1):((maximum oldKeys)+1):oldKeys
        oldKeys = Map.keys space
        on depth = fromJust $ Map.lookup depth space

stepOne :: Plutonian -> Int -> [String] -> [String]
stepOne space depth _ = [[rule (x,y,depth) | x <- [0..4]] | y <- [0..4]]
  where rule pos = case at space pos of
            '#' -> if near pos == 1 then '#' else '.'
            '.' -> let n = near pos in if n == 1 || n == 2 then '#' else '.'
            '?' -> '?'
        near pos = length $ filter (== '#') $ map (at space) $ from pos

at :: Plutonian -> Position -> Char
at space (2,2,_) = '?'
at space (x,y,z) = (tiles !! y) !! x
  where tiles = fromMaybe empty $ Map.lookup z space

display :: Plutonian -> String
display space = unlines $ concatMap displayOne $ sort $ Map.keys space
  where displayOne k = (show k):(fromJust $ Map.lookup k space)

from (0,0,n) = [(0,1,n),(1,0,n),(1,2,n-1),(2,1,n-1)]
from (4,4,n) = [(3,4,n),(4,3,n),(2,3,n-1),(3,2,n-1)]
from (4,0,n) = [(3,0,n),(4,1,n),(2,1,n-1),(3,2,n-1)]
from (0,4,n) = [(0,3,n),(1,4,n),(1,2,n-1),(2,3,n-1)]
from (1,1,n) = [(0,1,n),(1,0,n),(2,1,n),(1,2,n)]
from (1,3,n) = [(0,3,n),(1,2,n),(2,3,n),(1,4,n)]
from (3,1,n) = [(2,1,n),(3,0,n),(4,1,n),(3,2,n)]
from (3,3,n) = [(2,3,n),(3,2,n),(4,3,n),(3,4,n)]
from (1,0,n) = [(0,0,n),(1,1,n),(2,0,n),(2,1,n-1)]
from (0,1,n) = [(0,0,n),(1,1,n),(0,2,n),(1,2,n-1)]
from (3,0,n) = [(2,0,n),(3,1,n),(4,0,n),(2,1,n-1)]
from (4,1,n) = [(3,1,n),(4,0,n),(4,2,n),(3,2,n-1)]
from (0,3,n) = [(0,2,n),(1,3,n),(0,4,n),(1,2,n-1)]
from (1,4,n) = [(1,3,n),(0,4,n),(2,4,n),(2,3,n-1)]
from (3,4,n) = [(2,4,n),(3,3,n),(4,4,n),(2,3,n-1)]
from (4,3,n) = [(3,3,n),(4,2,n),(4,4,n),(3,2,n-1)]
from (2,0,n) = [(1,0,n),(2,1,n),(3,0,n),(2,1,n-1)]
from (0,2,n) = [(0,1,n),(1,2,n),(0,3,n),(1,2,n-1)]
from (2,4,n) = [(1,4,n),(2,3,n),(3,4,n),(2,3,n-1)]
from (4,2,n) = [(3,2,n),(4,1,n),(4,3,n),(3,2,n-1)]
from (2,1,n) = [(1,1,n),(2,0,n),(3,1,n)]++[(x,0,n+1) | x<-[0..4]]
from (2,3,n) = [(1,3,n),(2,4,n),(3,3,n)]++[(x,4,n+1) | x<-[0..4]]
from (1,2,n) = [(1,1,n),(0,2,n),(1,3,n)]++[(0,y,n+1) | y<-[0..4]]
from (3,2,n) = [(4,2,n),(3,1,n),(3,3,n)]++[(4,y,n+1) | y<-[0..4]]
from (2,2,n) = []

main = do
    contents <- readFile "Day24.txt"
    let final = (iterate step (start contents)) !! 200
    print $ countBugs final