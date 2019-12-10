module Day10Animated where

import Common
import Data.List
import Data.Char
import Data.Foldable
import Day10
import Day10PartOne hiding (main)

import System.Console.ANSI
import Control.Concurrent

vaporizeOrder :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
vaporizeOrder station field = concat $ transpose $ map (sortOn (distance station)) $ sightLines field station

display :: [String] -> [(Int,Int)] -> [String]
display lines targets = foldl insert lines (indexed targets)
  where insert lines (index, (x,y)) = replace y (replace x (intToDigit (index+1)) (lines !! y)) lines

displayOne :: [String] -> (Int,Int) -> [String]
displayOne lines (x,y) = replace y ((replace x '.') (lines !! y)) lines
  
main = do
    contents <- readFile "Day10Sample5.txt"
    let field = asteroidCoordinates contents
    clearScreen
    setCursorPosition 0 0
    putStrLn contents
    let frames = vaporizeOrder (bestOf field) field
    flip mapM frames (\(x,y) -> do
        setSGR [SetColor Foreground Vivid Red]
        setCursorPosition y x
        putStr "#"
        threadDelay 40000
        setSGR [SetColor Foreground Dull Red]
        setCursorPosition y x
        putStr "#"
        threadDelay 40000
        setSGR [SetColor Foreground Vivid Black]
        setCursorPosition y x
        putStr "."
        threadDelay 10000)
    setCursorPosition (length field) (length $ head field) 
