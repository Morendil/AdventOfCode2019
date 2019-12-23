module Day23PartOne where

import Common
import Data.List
import Data.Maybe
import IntCode

data Bot = Bot { queue :: [Packet], state :: State }
type Packet = [Integer]
type Network = [Bot]

start :: Program -> Network
start program = map (\n -> Bot { queue = [], state = initialize program [n]}) [0..49]

main = do
    contents <- readFile "Day23.txt"
    print $ parse contents
