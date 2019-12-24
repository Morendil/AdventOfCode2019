module Day24PartOne where

import Data.List
import Data.List.Tools

type Position = (Int, Int)
    
parse :: String -> [String]
parse = lines

repeating :: String -> Int
repeating = repeated . last . takeUntil repeats . inits . map asNumber . iterate step . parse

asNumber :: [String] -> Int
asNumber grid = foldr  (\b a -> (2*a) + value b) 0 $ concat grid 
  where value '#' = 1
        value '.' = 0

repeats :: [Int] -> Bool
repeats list = nub list /= list

repeated :: [Int] -> Int
repeated list = head $ list \\ nub list

step :: [String] -> [String]
step grid = [[rule (x,y) | x <- [0..4]] | y <- [0..4]]
  where rule pos = case at grid pos of
            '#' -> if near pos == 1 then '#' else '.'
            '.' -> let n = near pos in if n == 1 || n == 2 then '#' else '.'
        near pos = length $ filter (== '#') $ map (at grid) $ neighbours pos
        neighbours pos = filter (inGrid 5 5) (from pos)

add (x1, y1) (x2, y2) = (x1+x2, y1+y2)
offsets = [(0,1),(0,-1),(1,0),(-1,0)]
from pos = map (add pos) offsets
inGrid w h (x,y) = x >=0 && y >= 0 && x < w && y < h

at :: [String] -> Position -> Char
at tiles (x,y) = (tiles !! y) !! x

main = do
    contents <- readFile "Day24.txt"
    print $ repeating contents