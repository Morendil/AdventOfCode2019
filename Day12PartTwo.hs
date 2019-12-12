module Day12PartTwo where

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

cycleLength :: Stars -> Int
cycleLength stars = lcm (lcm (onX stars) (onY stars)) (onZ stars)
  where coordCycle coord stars = 1 + (length $ takeWhile (/= map coord stars) $ map (map coord) $ tail $ iterate step stars)
        onX = coordCycle xcoord
        onY = coordCycle ycoord
        onZ = coordCycle zcoord
        xcoord star = (fst star !! 0, snd star !! 0)
        ycoord star = (fst star !! 1, snd star !! 1)
        zcoord star = (fst star !! 2, snd star !! 2)

main = do
    contents <- readFile "Day12.txt"
    print $ cycleLength $ starting $ parse contents