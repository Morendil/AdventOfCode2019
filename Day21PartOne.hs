module Day21PartOne where

import Common
import IntCode
import Data.Char
import Data.List

fromAscii = map (chr.fromInteger)
toAscii = map (toInteger.ord)

main = do
    contents <- readFile "Day21.txt"
    putStrLn $ fromAscii $ execute (toAscii "NOT A J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\nWALK\n") contents
