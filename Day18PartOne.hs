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
cost tree = 0

fullPath tree order = nub $ concatMap (\k-> pathTo k tree) order
heuristic tree = fullPath tree $ sortOn (\k->length $ pathTo k tree) (finalKeys tree)

pathTo :: Char -> Distances -> [Char]
pathTo target tree = nub $ foldTree findPath tree
  where findPath :: (Char, Int) -> [[Char]] -> [Char]
        findPath (key,_) paths | key == target = [key]
        findPath (key,_) paths | isLower key = if null prefix then [] else key : (head prefix)
          where prefix = filter (not.null) paths
        findPath (key,_) paths | isUpper key = if null prefix then [] else (pathTo (toLower key) tree) ++ (head prefix)
          where prefix = filter (not.null) paths
        findPath (key,_) paths = if null prefix then [] else head prefix
          where prefix = filter (not.null) paths

allKeys :: Distances -> [Char]
allKeys tree = sort $ foldTree collectKeys tree
  where collectKeys (key,_) keys = if isLower key then key:(concat keys) else (concat keys)

finalKeys :: Distances -> [Char]
finalKeys tree = sort $ foldTree collectKeys tree
  where collectKeys (key,_) [] = if isLower key then [key] else []
        collectKeys (key,_) xs = concat xs

toTree :: String -> Distances
toTree maze = unfoldTree (visit $ lines maze) (start maze, 0, Map.empty)
    -- Node {rootLabel = ('@',0), subForest = []}

visit :: [String] -> (Position, Int, Visited) -> ((Char, Int), [(Position, Int, Visited)])
visit tiles (pos, distance, visited) = ((at tiles pos, distance), explore tiles visited pos 0)

explore :: [String] -> Visited -> Position -> Int -> [(Position, Int, Visited)]
explore tiles visited pos distance = if length next == 1 && all boring next
    -- if the path does not branch, then keep track of distance but do not report
    then explore tiles visited' (head next) (distance + 1)
    else [(pos', distance+1, visited'') | pos' <- next]
  where visited' = Map.insert pos distance visited
        visited'' = foldr (\p v -> Map.insert p (distance+1) v) visited' next
        next = filter (\adjacent -> unknown adjacent && floor adjacent) (from pos)
        unknown pos = isNothing $ Map.lookup pos visited
        floor pos = at tiles pos /= '#'
        boring pos = at tiles pos == '.'

at :: [String] -> Position -> Char
at tiles (x,y) = (tiles !! y) !! x

start :: String -> (Int, Int)
start maze = head $ concatMap (\(y, line) -> [(x,y) | x <- findIndices (== '@') line]) $ indexed $ lines maze

add (x1, y1) (x2, y2) = (x1+x2, y1+y2)
offsets = [(0,1),(0,-1),(1,0),(-1,0)]
from pos = map (add pos) offsets

main = do
    contents <- readFile "Day18.txt"
    print $ start contents