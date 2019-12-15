module Day15PartTwo where

import Common
import IntCode hiding (State)
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import System.Console.ANSI
import Control.Concurrent

data State = State {
    position :: Position,
    tiles :: Tiles,
    explored :: Marks,
    command :: Integer,
    times :: Times
    } deriving Show

type Position = (Int, Int)
type Tiles = Map.HashMap Position Char
type Marks = Map.HashMap Position [Integer]
type Times = Map.HashMap Position Integer

states :: Program -> State -> [State]
states program initial = allStates
  where allStates = scanl stepState initial outputs
        outputs = map fromInteger $ outputSequence program inputs
        inputs = map command allStates

center :: Tiles
center = Map.insert (0,0) 'x' Map.empty

initial :: State
initial = State {position=(0,0), tiles = center, explored = Map.empty, command = 1, times = Map.insert (0,0) 0 Map.empty}

commands = [1,4,3,2]

offset 1 = (0,-1)
offset 2 = (0,1)
offset 3 = (1,0)
offset 4 = (-1,0)

add (x1, y1) (x2, y2) = (x1+x2, y1+y2)

go :: Position -> Integer -> Position
go pos cmd = add pos (offset cmd)

stepState :: State -> Int -> State
stepState state out = case out of
    0 -> state' { tiles = Map.insert pos' '#' map, command = moveOn pos}
    _ -> state' { tiles = Map.insert pos' '.' map, position = pos', command = moveOn pos', times = times'}
  where pos = position state
        pos' = go pos cmd
        cmd = command state
        map = tiles state
        state' = state { explored = Map.insert pos (cmd : alreadyExplored state pos) $ explored state }
        alreadyExplored state pos = fromMaybe [] $ Map.lookup pos (explored state)
        possibles pos = filter (exploring pos) (breadcrumbs pos)
        breadcrumbs pos = commands \\ alreadyExplored state' pos
        backtrack pos = if null $ breadcrumbs pos then -1 else head $ breadcrumbs pos
        moveOn pos = if null $ possibles pos then backtrack pos else head $ possibles pos
        timeTo pos = fromMaybe 999 $ Map.lookup pos (times state)
        times' = if timeTo pos' > ((timeTo pos)+1) then Map.insert pos' ((timeTo pos)+1) (times state) else (times state)
        exploring pos cmd = isNothing $ Map.lookup (go pos cmd) (tiles state)

display :: State -> [String]
display state = [[tile x y | x <- [minx..maxx]] | y <- [miny..maxy]]
  where tile x y = if ((floor x y) == '.') && (time x y > 70) then last $ show (time x y) else floor x y
        floor x y = fromMaybe ' ' $ Map.lookup (x,y) (tiles state)
        time x y = fromJust $ Map.lookup (x,y) (times state)
        minx = minimum $ xs
        maxx = maximum $ xs
        miny = minimum $ ys
        maxy = maximum $ ys
        xs = map fst positions
        ys = map snd positions
        positions = Map.keys $ tiles state

main = do
    contents <- readFile "Day15.txt"
    let program = parse contents
    let output = states program initial
    hideCursor
    clearScreen
    let final = last output
    toAscii final
    showCursor
    print $ maximum $ Map.elems $ times $ last output

toAscii state = do
    setCursorPosition 0 0
    putStr $ unlines $ display state
    setCursorPosition 45 0
