module Day12 where

import Common
import Data.Char
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

assign :: ReadP Int
assign = do
    many1 (satisfy isAlpha)
    char '='
    fromInteger <$> number

star = between (char '<') (char '>') $ sepBy1 assign (string ", ")
stars = sepBy1 star (char '\n')

parse :: String -> [Position]
parse contents = fromMaybe [] $ parseMaybe stars contents

type Position = [Int]
type Velocity = [Int]
type Star = (Position, Velocity)
type Stars = [Star]

starting :: [Position] -> Stars
starting stars = zip stars $ replicate (length stars) [0,0,0]

gravity :: Stars -> Stars
gravity stars = map (\star -> effectOf (stars \\ [star]) star) stars

effectOf :: Stars -> Star -> Star
effectOf otherStars target = foldr effectOfOne target otherStars

effectOfOne :: Star -> Star -> Star
effectOfOne source target = (fst target, zipWith (+) (snd target) (zipWith delta (fst source) (fst target)))

delta source target = (fromEnum $ compare source target) - 1

velocity :: Stars -> Stars
velocity = map (\star -> (uncurry (zipWith (+)) star, snd star))

step :: Stars -> Stars
step = velocity . gravity
