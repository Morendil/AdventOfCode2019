module Day04 where

import Data.Char
  
digits :: Int -> [Int]
digits = map digitToInt . show

pairs :: Int -> [(Int,Int)]
pairs n = zip (digits n) (tail $ digits n)
