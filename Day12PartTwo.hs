module Day12PartTwo where

import Day12

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