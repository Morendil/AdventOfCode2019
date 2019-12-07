module GoldenOldie2018Day23 where

import Common
import Data.Maybe
import Text.ParserCombinators.ReadP

triple = do
    x <- number; comma;  y <- number;  comma ; z <- number
    return (x, y, z)

bot = do
    (x,y,z) <- between (string "pos=<") (string ">, r=") triple
    r <- number
    return (x, y, z, r)

parseBot :: String -> (Integer, Integer, Integer, Integer)
parseBot = fromJust . parseMaybe bot

parse :: [String] -> [(Integer, Integer, Integer, Integer)]
parse = map parseBot    