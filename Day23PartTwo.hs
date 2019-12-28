module Main where

import Common
import Data.List
import Data.Maybe
import IntCode
import Data.List.Tools

import Debug.Trace

data Bot = Bot { address :: Integer, state :: State}
type Packet = [Integer]
data Network = Network { bots :: [Bot], nat :: [Packet], delivered :: [Packet] }

start :: Program -> Network
start program = Network { bots=bots, nat = [], delivered = []}
  where bots = map (\n -> Bot { address = toInteger n, state = initialize program ([n]++repeat (-1))}) [0..49]

idle :: Network -> Bool
idle network = not (null $ nat network) && all (\bot -> (lastIn.state) bot == Just (-1)) (bots network)

switch :: Network -> Network
switch network = if any for255 mail then interceptIt else switchIt
  where interceptIt = switchIt { nat = (nat network)++[natPacket] }
        switchIt = network { bots = map relieve $ map (dispatch mail) (bots network) }
        natPacket = fromJust $ find for255 mail
        -- mail = if null mail' then mail' else traceShowId $ mail'
        mail = mailbag network

mailbag :: Network -> [Packet]
mailbag = catMaybes . map collect . bots

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
stepAll network = if idle network then rebootIt else switch $ network { bots = map stepOneState (bots network) }
  where rebootIt = network { bots = map (stepOneState.resetLast) dispatched, delivered = (delivered network) ++ [rebootPacket] }
        resetLast bot = bot {state = (state bot) { lastIn = Nothing}}
        dispatched = (dispatch [rebootPacket] $ head $ bots network):(tail $ bots network)
        rebootPacket = replace 0 0 $ last $ nat network

stepOneState :: Bot -> Bot
stepOneState bot = bot { state = step $ state bot}

for255 p = head p == toInteger 255

main = do
    contents <- readFile "Day23.txt"
    let program = parse contents
        repeats l = length l >=2 && ((l !! ((length l)-1)) == (l !! ((length l)-2)))
    print $ last $ last $ takeUntil repeats $ map (map (!!2)) $ map delivered $ iterate stepAll (start program)
