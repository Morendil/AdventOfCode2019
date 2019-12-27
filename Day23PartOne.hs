module Main where

import Common
import Data.List
import Data.Maybe
import IntCode
import Data.List.Tools

import Debug.Trace

data Bot = Bot { address :: Integer, state :: State }
type Packet = [Integer]
type Network = [Bot]

start :: Program -> Network
start program = map (\n -> Bot { address = toInteger n, state = initialize program ([n]++repeat (-1))}) [0..49]

switch :: Network -> Network
switch network = if any for255 mail then network else map relieve $ map (dispatch mail) network
  where mail = mailbag network

mailbag :: Network -> [Packet]
mailbag = catMaybes . map collect

collect :: Bot -> Maybe Packet
collect bot = if (length $ (output.state) bot) >= 3 then Just $ take 3 $ (output.state) bot else Nothing

relieve :: Bot -> Bot
relieve bot = bot { state = (state bot) { output = output'} }
  where out = (output.state) bot
        output' = if length out >= 3 then drop 3 out else out

dispatch :: [Packet] -> Bot -> Bot
dispatch packets bot = bot { state = (state bot) { input = input'} }
  where inp = takeWhile (/= (-1)) $ (input.state) bot
        input' = inp ++ (concat $ map tail $ filter (\p -> head p == address bot) packets) ++ (repeat (-1))

stepAll :: Network -> Network
stepAll = switch . map stepOneState

stepOneState :: Bot -> Bot
stepOneState bot = bot { state = step $ state bot}

for255 p = head p == toInteger 255

main = do
    contents <- readFile "Day23.txt"
    let program = parse contents        
    print $ find for255 $ last $ takeUntil (any for255) $ map mailbag $ iterate stepAll (start program)