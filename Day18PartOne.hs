module Day18PartOne where

import Common
import Data.List
import Data.Tree
import Data.Char
import Data.Maybe
import qualified Data.HashMap.Strict as Map

type Position = (Int, Int)
type Visited = Map.HashMap Position Int
type Distances = Tree (Char, Int)
type Goals = [((Int,Int), Int)]

cost :: Distances -> Int
cost tree = costWithKeys [] tree

costWithKeys :: [Char] -> Distances -> Int
costWithKeys keys tree = 0

allKeys :: Distances -> [Char]
allKeys tree = sort $ foldTree collectKeys tree
  where collectKeys (key,_) keys = if isLower key then key:(concat keys) else (concat keys)

-- which of the reachable subgoals are either keys or doors I can open
pruneByKeys :: [Char] -> Distances -> Char
pruneByKeys keys tree = undefined

toTree :: String -> Distances
toTree maze = unfoldTree (visit $ lines maze) (start maze, 0, Map.empty)
    -- Node {rootLabel = ('@',0), subForest = []}

visit :: [String] -> (Position, Int, Visited) -> ((Char, Int), [(Position, Int, Visited)])
visit tiles (pos, distance, visited) = ((at tiles pos, distance), [(p, dist, visited') | (p,dist) <- reached])
    where (visited', reached) = spread tiles visited [] pos 0

at :: [String] -> Position -> Char
at tiles (x,y) = (tiles !! y) !! x

start :: String -> (Int, Int)
start maze = head $ concatMap (\(y, line) -> [(x,y) | x <- findIndices (== '@') line]) $ indexed $ lines maze

add (x1, y1) (x2, y2) = (x1+x2, y1+y2)
offsets = [(0,1),(0,-1),(1,0),(-1,0)]
from pos = map (add pos) offsets

spread ::  [String] -> Visited -> Goals -> Position -> Int -> (Visited, Goals)
spread tiles visited reached pos time = if null next then (visit (time+1) visited goals, goals) else foldr stepTime (Map.insert pos time visited, goals) next
  where stepTime pos' (visited, reached) = merge (visited, reached) $ spread tiles (Map.insert pos' (time+1) visited) reached pos' (time+1)
        merge (v,r) (v',r') = (v', r++r')
        next = filter (\adjacent -> blank adjacent && floor adjacent) (from pos)
        visit time visited = foldr (\pos visited -> Map.insert pos time visited) visited . map fst
        goals = [(p, time+1) | p <- filter (isAlpha . at tiles) $ filter blank $ from pos]
        blank pos = isNothing $ Map.lookup pos visited
        floor pos = at tiles pos == '.'

main = do
    contents <- readFile "Day18.txt"
    print $ start contents