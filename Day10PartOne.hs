module Day10PartOne where

import Data.List
import Data.Ord

best :: String -> (Int, Int)
best field = bestFor $ lines field

bestFor :: [String] -> (Int, Int)
bestFor field = maximumBy (comparing $ seeing coords) coords
  where coords = asteroidCoordinates field

asteroidCoordinates :: [String] -> [(Int, Int)]
asteroidCoordinates field = [(x,y) | x <- [0..width-1], y <- [0..height-1], (field !! y) !! x == '#']
  where width = length $ head field
        height = length field

scores :: [(Int, Int)] -> [Int]
scores coords = map (seeing coords) coords

seeing :: [(Int, Int)] -> (Int, Int) -> Int
seeing coords observer = length $ filter (seenFrom coords observer) coords

seenFrom :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Bool
seenFrom field observer target = (target /= observer) && (not $ any (blocking field observer target) field)

blocking :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
blocking field observer target between = (between /= observer) && (between /= target) && (obstructs observer between target)

obstructs :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
obstructs (ox, oy) (bx, by) (tx, ty) = (between ox bx tx) && (between oy by ty)
    && ((ox * (by - ty)) + (bx * (ty - oy)) + (tx * (oy - by))) == 0
  where between o b t = ((o <= b) && (b <= t)) || ((o >= b) && (b >= t))

main = do
    contents <- readFile "Day10.txt"
    print $ seeing (asteroidCoordinates $ lines contents) (27, 19)
    -- print $ best $ contents