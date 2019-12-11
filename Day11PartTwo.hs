module Day11PartTwo where

import Day11
import IntCode hiding (State)
import qualified Data.HashMap.Strict as Map

display :: Program -> State -> [String]
display program initial = [[panel x y | x <- [minx..maxx]] | y <- [miny..maxy]]
  where panel x y = if Map.lookup (x,y) panels == Just True then '#' else '.'
        minx = minimum $ xs
        maxx = maximum $ xs
        miny = minimum $ ys
        maxy = maximum $ ys
        xs = map fst positions
        ys = map snd positions
        (State panels _ _) = finalState program initial
        positions = Map.keys panels

main = do
    contents <- readFile "Day11.txt"
    let (State panels _ _ ) = finalState (parse contents) empty
    putStrLn $ unlines $ reverse $ display (parse contents) empty