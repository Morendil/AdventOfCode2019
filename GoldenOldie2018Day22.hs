module GoldenOldie2018Day22 where

import Data.Graph.AStar
import Data.HashSet hiding (map, filter)
import Data.List
import Data.Maybe
import Data.MemoTrie

depth = 5616
target = (10,785)

index :: Int -> (Int, Int) -> (Int, Int) -> Int
index _ _ (0, 0) = 0
index _ _ (x, 0) = x * 16807
index _ _ (0, y) = y * 48271
index depth target coords | target == coords = 0
index depth target (x, y) = (erosionmem depth target (x-1,y)) * (erosionmem depth target (x,y-1))

erosion :: Int -> (Int, Int) -> (Int, Int) -> Int
erosion depth target coords = ((indexmem depth target coords) + depth) `mod` 20183

terrain :: Int -> (Int, Int) -> (Int, Int) -> Int
terrain depth target coords = (erosionmem depth target coords) `mod` 3

indexmem = memo3 index
erosionmem = memo3 erosion
terrainmem = memo3 terrain

landscape :: Int -> (Int, Int) -> (Int, Int) -> [[Char]]
landscape depth target (tx, ty) = [[t (terrain depth target (x, y)) | x <- [0..tx]] | y <- [0..ty]]
  where t 0 = '.'
        t 1 = '='
        t 2 = '|'

moveCost :: (Int, Int, Int) -> (Int, Int, Int) -> Int
moveCost (fx, fy, ft) (tx, ty, tt) = if ft == tt then 1 else 7

offsets :: [(Int, Int, Int)]
offsets = [(-1,0,0),(0,1,0),(0,-1,0),(1,0,0),(0,0,-1),(0,0,1),(0,0,-2),(0,0,2)]

okFor :: Int -> Int -> Bool
okFor tool 0 = tool == 0 || tool == 1
okFor tool 1 = tool == -1 || tool == 1
okFor tool 2 = tool == -1 || tool == 0

add (x1, y1, t1) (x2, y2, t2) = (x1+x2, y1+y2, t1+t2)

path :: Int -> (Int, Int) -> Maybe [(Int, Int, Int)]
path depth target@(tx, ty) = aStar (neighboursmem depth target) moveCost heuristic goal start
  where start = (0, 0, 0)
        goal pos = pos == (tx, ty, 0)
        heuristic (x,y,_) = x+y

neighboursmem = memo3 neighbours

neighbours :: Int -> (Int, Int) -> (Int, Int, Int) -> HashSet (Int, Int, Int)
neighbours depth target cell = fromList $ filter allowed $ map (add cell) offsets
  where allowed (a,b,c)= a >= 0 && b >= 0 && c >= -1 && c <= 1 && suitable (a,b) c
        suitable coord tool = okFor tool (terrainmem depth target coord)

cost :: Int -> (Int, Int) -> Int
cost depth target = sum $ zipWith moveCost steps (tail steps)
  where steps = [(0,0,0)] ++ (fromMaybe [] $ path depth target)

-- main = putStr $ unlines $ landscape 510 (10,10) (15,15)
main = do
  print $ path 510 (10,10)
  print $ cost 510 (10,10)
