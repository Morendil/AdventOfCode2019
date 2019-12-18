module Day18PartOne where

import Common
import Data.List
import Data.Tree
import Data.Char
import Data.Ord
import Data.Maybe
import qualified Data.HashMap.Strict as Map

type Position = (Int, Int)
type Visited = Map.HashMap Position Int
type Point = (Char,  Position, Int)
type Distances = Tree Point
type Goals = [((Int,Int), Int)]

fullPath tree order = "@" ++ (nub $ concatMap (\k-> pathTo k tree) order)
heuristic tree = fullPath tree $ sortOn (\k->length $ pathTo k tree) (finalKeys tree)

bestPath :: Distances -> [Char]
bestPath tree = minimumBy (comparing (\p -> costOf tree p)) $ map (\p -> fullPath tree p) $ permutations $ finalKeys tree

bestCost :: Distances -> Int
bestCost tree = minimum $ map (\p -> costOf tree $ fullPath tree p) $ permutations $ finalKeys tree

costOf :: Distances-> [Char] -> Int
costOf tree order = sum $ map (uncurry (cost tree)) $ oneAndNext $ order

cost :: Distances -> Char -> Char -> Int
cost tree one two = go (descendTo one tree) (descendTo two tree)
  where go a [] = sum $ map cost a
        go [] b = sum $ map cost b
        go a b | head a == head b = go (tail a) (tail b)
        go a b = (go a []) + (go [] b)
        cost (key, pos, dist) = dist

descendTo :: Char -> Distances -> [Point]
descendTo target tree = foldTree findPath tree
  where findPath :: Point -> [[Point]] -> [Point]
        findPath label@(key,_,_) paths | key == target = [label]
        findPath label@(key,_,_) paths = if null prefix then [] else label : (head prefix)
          where prefix = filter (not.null) paths

pathTo :: Char -> Distances -> [Char]
pathTo target tree = nub $ foldTree findPath tree
  where findPath :: Point -> [[Char]] -> [Char]
        findPath (key,_,_) paths | key == target = [key]
        findPath (key,_,_) paths | isLower key = if null prefix then [] else key : (head prefix)
          where prefix = filter (not.null) paths
        findPath (key,_,_) paths | isUpper key = if null prefix then [] else (pathTo (toLower key) tree) ++ (head prefix)
          where prefix = filter (not.null) paths
        findPath (key,_,_) paths = if null prefix then [] else head prefix
          where prefix = filter (not.null) paths

allKeys :: Distances -> [Char]
allKeys tree = nub $ sort $ foldTree collectKeys tree
  where collectKeys (key,_,_) keys = if isLower key then key:(concat keys) else (concat keys)

finalKeys :: Distances -> [Char]
finalKeys tree = nub $ sort $ foldTree collectKeys tree
  where collectKeys (key,_,_) [] = if isLower key then [key] else []
        collectKeys (key,_,_) xs = concat xs

initialKeys :: Distances -> [Char]
initialKeys tree = nub $ sort $ foldTree collectKeys tree
  where collectKeys (key,_,_) _ | isLower key = [key]
        collectKeys (key,_,_) xs = concat xs

toTree :: String -> Distances
toTree maze = simplify $ unfoldTree (visit $ lines maze) (start maze, 0, Map.empty)

simplify :: Distances -> Distances
simplify = foldTree coalesce
  where coalesce :: Point -> [Tree Point] -> Tree Point
        coalesce label@('.',_,cost) [(Node (key,pos,cost') trees)] = Node (key,pos,cost+cost') trees
        coalesce label forest = Node label forest

strip (char, pos, dist) = (char, dist)

visit :: [String] -> (Position, Int, Visited) -> ((Char, Position, Int), [(Position, Int, Visited)])
visit tiles (pos, distance, visited) = ((at tiles pos, pos, distance), explore tiles visited pos 0)

explore :: [String] -> Visited -> Position -> Int -> [(Position, Int, Visited)]
explore tiles visited pos distance = [(pos', distance+1, visited'') | pos' <- next]
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