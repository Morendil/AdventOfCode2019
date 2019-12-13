module GoldenOldie2018Day23 where

import Common
import Data.Maybe
import Text.ParserCombinators.ReadP
import Data.Algorithm.MaximalCliques

triple = do
    x <- number; comma;  y <- number;  comma ; z <- number
    return (x, y, z)

bot = do
    (x,y,z) <- between (string "pos=<") (string ">, r=") triple
    r <- number
    return (x, y, z, r)

parseBot :: String -> (Integer, Integer, Integer, Integer)
parseBot = fromJust . parseMaybe bot

toPlanes (x,y,z,r) = [(x+y+z-r,x+y+z+r),(x-y+z-r,x-y+z+r),(x+y-z-r,x-y-z+r),(x-y-z-r,x-y-z+r)]
intersect (x1,y1,z1,r1) (x2,y2,z2,r2) = abs(x2-x1)+abs(y2-y1)+abs(z2-z1) <= (r1+r2)
overlap (a,b) (c,d) = (max a c, min b d)
maxClique bots = head $ getMaximalCliques intersect bots
intersection bots = foldr1 (zipWith overlap) $ map toPlanes maxClique
result = foldr1 (zipWith overlap) $ map toPlanes $ maxClique bots

parse :: [String] -> [(Integer, Integer, Integer, Integer)]
parse = map parseBot