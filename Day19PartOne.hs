module Day19PartOne where

import Common
import IntCode
import Data.List

inputs :: [[Integer]]
inputs = [[x,y] | x <- [0..49], y <- [0..49]]

disp 0 = '.'
disp 1 = '#'

main = do
    program <- readFile "Day19.txt"
    let grid = concatMap (\pair -> execute pair program) inputs
        ones = length $ filter (>0) grid
    print ones
    putStrLn $ unlines $ chunks 50 $ map disp grid