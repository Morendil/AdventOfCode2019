module Day12PartOne where

import Common
import Data.Char
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

assign :: ReadP Int
assign = do
    many1 (satisfy isAlpha)
    char '='
    value <- number
    return $ fromInteger value

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
velocity = map (\star -> (zipWith (+) (fst star) (snd star), snd star))

step :: Stars -> Stars
step = velocity . gravity

steps :: Int -> Stars -> Stars
steps n = last . take (n+1) . iterate step

energy :: Position -> Int
energy [x,y,z] = abs x + abs y + abs z

potential :: Star -> Int
potential = energy . fst

kinetic :: Star -> Int
kinetic = energy . snd

totalEnergy :: Star -> Int
totalEnergy star = potential star * kinetic star

systemEnergy :: Stars -> Int
systemEnergy = sum . map totalEnergy

main = do
    contents <- readFile "Day12.txt"
    print $ systemEnergy $ steps 1000 $ starting $ parse contents