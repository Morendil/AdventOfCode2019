module Day03PartTwo where

import Day03    

timing s1 s2 = minimum $ timings s1 s2

timings s1 s2 = map timeSteps $ tail $ crossings wire1 wire2
    where timeSteps position = (timeStepsTo position wire1) + (timeStepsTo position wire2)
          wire1 = process s1
          wire2 = process s2
          process = segments . parse

timeStepsTo :: Position -> [Segment] -> Integer
timeStepsTo = go 0
    where go total pos@(cx, cy) ((H (y, x1, x2)) : segments) =
            if y == cy && within cx x1 x2
                then total + (abs (cx-x1))
                else go (total + (abs (x2-x1))) pos segments
          go total pos@(cx, cy) ((V (x, y1, y2)) : segments) =
            if x == cx && within cy y1 y2
                then total + (abs (cy-y1))
                else go (total + (abs (y2-y1))) pos segments

main = do
    contents <- readFile "Day03.txt"
    let l = lines contents
    print $ timing (l !! 0) (l !! 1)