module Day04PartOne where

import Day04

accept :: Int -> Bool
accept n = (any same npairs) && (all increase npairs)
  where npairs = pairs n
        same (a, b) = a == b
        increase (a, b) = a <= b

countAccepted :: Int -> Int -> Int
countAccepted low high = length $ filter accept [low..high]

main = print $ countAccepted 152085 670283