module GoldenOldie2018Day23PartOne where

import Common
import GoldenOldie2018Day23
import Data.List

leaderBots :: [(Integer, Integer, Integer, Integer)] -> Int
leaderBots bots = length $ filter (inRangeOf leader) bots
  where leader = last $ sortOn (\(x,y,z,r) -> r) $ bots
        inRangeOf (lx,ly,lz,lr) (bx,by,bz,_) = ((abs (bx-lx))+(abs (by-ly))+(abs (bz-lz))) <= lr

main = do
    contents <- readFile "GoldenOldie2018Day23.txt"
    print $ leaderBots $ parse $ lines contents