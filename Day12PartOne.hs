module Day12PartOne where

import Day12

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