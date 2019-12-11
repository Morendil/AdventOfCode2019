module Day11PartOne where

import Common
import IntCode hiding (State)
import qualified Data.HashMap.Strict as Map

data Direction = U | D | L | R deriving (Eq, Show)
data State = State (Map.HashMap Position Bool) Direction Position deriving (Eq, Show)
type Position = (Int, Int)

painted :: Program -> State -> Int
painted program state = countPainted $ finalState program empty

countPainted :: State -> Int
countPainted (State panels _ _ ) = length $ Map.keys panels

finalState :: Program -> State -> State
finalState program initial = last $ states program initial

states :: Program -> State -> [State]
states program initial = allStates
  where allStates = scanl stepState initial outputPairs
        outputPairs = (chunks 2 $ outputSequence program inputs)
        inputs = map input (initial : (tail allStates))

stepState :: State -> [Integer] -> State
stepState (State panels dir pos) [paint, turn] = State panels' dir' pos'
  where dir' = doTurn dir turn
        pos' = move dir' pos
        panels' = Map.insert pos (paint == 1) panels

move :: Direction -> Position -> Position
move U (x, y) = (x, y+1)
move D (x, y) = (x, y-1)
move L (x, y) = (x-1, y)
move R (x, y) = (x+1, y)

doTurn :: Direction -> Integer -> Direction
doTurn U 1 = R
doTurn U 0 = L
doTurn D 1 = L
doTurn D 0 = R
doTurn L 1 = U
doTurn L 0 = D
doTurn R 1 = D
doTurn R 0 = U
 
empty :: State
empty = State Map.empty U (0,0)

input :: State -> Integer
input (State panels _ pos) = if Map.lookup pos panels == Just True then 1 else 0

main = do
    contents <- readFile "Day11.txt"
    print $ painted (parse contents) empty