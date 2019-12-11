module Day11PartTwo where

import Common
import IntCode hiding (State)
import qualified Data.HashMap.Strict as Map

data Direction = U | D | L | R deriving (Eq, Show)
data State = State (Map.HashMap Position Bool) Direction Position deriving (Eq, Show)
type Position = (Int, Int)

display :: Program -> State -> [String]
display program initial = [[panel x y | x <- [minx..maxx]] | y <- [miny..maxy]]
  where panel x y = if Map.lookup (x,y) panels == Just True then '#' else '.'
        minx = minimum $ xs
        maxx = maximum $ xs
        miny = minimum $ ys
        maxy = maximum $ ys
        xs = map fst positions
        ys = map snd positions
        (State panels _ _) = finalState program initial
        positions = Map.keys panels

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

input :: State -> Integer
input (State panels _ pos) = if Map.lookup pos panels == Just True then 1 else 0

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
empty = State (Map.insert (0,0) True Map.empty) U (0,0)

main = do
    contents <- readFile "Day11.txt"
    let (State panels _ _ ) = finalState (parse contents) empty
    putStrLn $ unlines $ reverse $ display (parse contents) empty