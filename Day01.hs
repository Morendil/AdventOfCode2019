module Day01 where

import Common

process :: (Integer -> Integer) -> IO ()  
process f = do
        contents <- readFile "Day01.txt"
        let masses = map readInt . words $ contents
          in print . sum . map f $ masses
