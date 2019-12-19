module Day19PartOne where

import Common
import IntCode hiding (step)
import Data.List

inputs :: [[Integer]]
inputs = [[x,y] | y <- [1730..1779], x <- [1350..1399]]

disp 0 = '.'
disp 1 = '#'

-- (1590,1760)

step (x,y,a1,a2) = (x',y',a1',a2')
  where x' = ((100*a2)+100)/(a1-a2)
        y' = x' * a1
        a1' = y'/x'
        a2' = (y'-100)/(x'+100)

lookRight program [x, y] = last $ takeWhile (\i -> (head $ run i program) == 0) $ [[x',y]|x'<-[x..]]
lookDown program [x, y] = last $ takeWhile (\i -> (head $ run i program) == 0) $ [[x,y']|y'<-[y..]]

main = do
    program <- readFile "Day19.txt"
    let grid = concatMap (\pair -> run pair (parse program)) inputs
    putStrLn $ unlines $ chunks 50 $ map disp grid