module Day20PartOne where

import Common
import Data.Char
import Data.List
import Data.Maybe
import Data.Tree
import Control.Monad.State
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Graph.AStar

import Debug.Trace

type Position = (Int, Int)
type Portal = (String, Position)
type Connections = Map.HashMap Portal Distances

type Visited = Map.HashMap Position Int
type Point = (String, Position, Int)
type Distances = Tree Point

connections :: [String] -> [Portal] -> Connections
connections maze gates = foldr add Map.empty gates
  where add portal map = if isNothing $ Map.lookup portal map then Map.insert portal (toTree maze portal) map else map

findPortal :: [Portal] ->  String -> [Portal]
findPortal portals target = filter (\p -> fst p == target) portals

letters :: [String] -> [Position]
letters maze = concatMap (\(y, line) -> [(x,y) | x <- findIndices isAlpha line]) $ indexed $ maze

portals :: [String] -> [Portal]
portals maze = nub $ map (portal maze locations) locations
  where locations = letters maze

portal :: [String] -> [Position] -> Position -> Portal
portal maze letters letter = (label, head $ filter isFloor $ concatMap from (letter:matching))
    where label = map (at maze) $ sort (letter:matching)
          matching = filter (match letter) letters
          match (x1,y1) (x2,y2) = (y1 == y2 && abs(x1-x2)==1) || (x1 == x2 && abs(y1-y2)==1)
          isFloor pos@(x,y) = x>0 && y> 0 && y < height && x < width && at maze pos == '.'
          width = length $ head maze
          height = length maze

at :: [String] -> Position -> Char
at tiles (x,y) = (tiles !! y) !! x

labels :: Distances -> [Portal]
labels = foldTree findLabels
  where findLabels (".",_,_) xs = concat xs
        findLabels (label,pos,_) xs = (label,pos):(concat xs)

cost :: Distances -> String -> String -> Int
cost tree one two = (go d1 d2)
  where d1 = descendTo tree one
        d2 = descendTo tree two
        go a [] = sum $ map cost a
        go [] b = sum $ map cost b
        go a b | head a == head b = go (tail a) (tail b)
        go a b = (go a []) + (go [] b)
        cost (key, pos, dist) = dist
        coords (key, pos, dist) = pos

descendTo :: Distances -> String -> [Point]
descendTo tree target = foldTree findPath tree
  where findPath :: Point -> [[Point]] -> [Point]
        findPath label@(key,_,_) paths | key == target = [label]
        findPath label@(key,_,_) paths = if null prefix then [] else label : (head prefix)
          where prefix = filter (not.null) paths

simplify :: [Portal] -> Distances -> Distances
simplify portals = foldTree coalesce
  where coalesce :: Point -> [Tree Point] -> Tree Point
        coalesce label@(_,pos,cost) forest | any (\p -> snd p == pos) portals = Node (portal,pos,cost) forest
          where portal = fst $ head $ filter (\p -> snd p == pos) portals
        coalesce label@(_,_,cost) [(Node (key,pos,cost') trees)] = Node (key,pos,cost+cost') trees
        coalesce label forest = Node label (filter (goes portals) forest)

goes :: [Portal] -> Distances -> Bool
goes portals = foldTree leadsToPortal
  where leadsToPortal (key,pos,_) [] = any (\p -> snd p == pos) portals
        leadsToPortal _ xs = or xs

toTree :: [String] -> Portal -> Distances
toTree maze portal = simplify (portals maze) $ evalState unfoldWithState Map.empty
  where unfoldWithState :: State Visited Distances
        unfoldWithState = (unfoldTreeM (visit $ maze) (snd portal, 0)) 

visit :: [String] -> (Position, Int) -> State Visited (Point, [(Position, Int)])
visit tiles (pos, distance) = do
    explored <- explore tiles pos 0
    return $ (([at tiles pos], pos, distance), explored)

explore :: [String] -> Position -> Int -> State Visited [(Position, Int)]
explore tiles pos distance = do
    visited <- get
    let next = filter (\adjacent -> unknown adjacent && floor adjacent) (from pos)
        unknown pos = isNothing $ Map.lookup pos visited
        visited' = Map.insert pos distance visited
        visited'' = foldr (\p v -> Map.insert p (distance+1) v) visited' next
        result = [(pos', distance+1) | pos' <- next]
    put visited''
    return result
  where floor pos = at tiles pos == '.'

solution :: Connections -> [Portal] -> Portal -> [Portal]
solution links gates initial = initial:(fromJust $ aStar neighbours (distance links) heuristic goal initial)
  where neighbours :: Portal -> Set.HashSet Portal
        neighbours p = Set.fromList $ (onFoot p) ++ (viaGate p)
          where onFoot portal = (labels $ fromJust $ Map.lookup portal links) \\ [p]
                viaGate p = findPortal gates (fst p) \\ [p]
        heuristic :: Portal -> Int
        heuristic _ = 0
        goal :: Portal -> Bool
        goal portal = fst portal == "ZZ"

distance :: Connections -> Portal -> Portal -> Int
distance links (l1,_) (l2,_) | l1==l2 = 1
distance links p1 p2 = cost (fromJust $ Map.lookup p1 links) (fst p1) (fst p2)

add (x1, y1) (x2, y2) = (x1+x2, y1+y2)
offsets = [(0,1),(0,-1),(1,0),(-1,0)]
from pos = map (add pos) offsets

main = do
    contents <- readFile "Day20.txt"
    let maze = lines contents
        gates = portals maze
        start = head $ findPortal gates "AA"
        links = connections maze gates
        path = solution links gates start
    print path
    print $ sum $ map (uncurry $ distance links) $ oneAndNext $ path