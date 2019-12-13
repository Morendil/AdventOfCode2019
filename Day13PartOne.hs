module Day13PartOne where

import Common
import IntCode
import qualified Data.HashMap.Strict as Map

type Position = (Int, Int)
type Tiles = Map.HashMap Position Int

tiles :: Program -> Tiles
tiles program = foldl addTile Map.empty $ chunks 3 $ outputSequence program []

addTile :: Tiles -> [Integer] -> Tiles
addTile tiles [x,y,id] = Map.insert (fromInteger x, fromInteger y) (fromInteger id) tiles

blocks :: Tiles -> Int
blocks = length . Map.filter (== 2)

main = do
    contents <- readFile "Day13.txt"
    print $ blocks $ tiles (parse contents)