module Day13PartTwo where

import Common
import Data.Maybe
import Data.Tuple
import IntCode hiding (State)
import System.Console.ANSI
import Control.Concurrent
import qualified Data.HashMap.Strict as Map

tile 1 = '#'
tile 2 = '*'
tile 3 = '_'
tile 4 = 'o'
tile _ = '.'

data State = State {
    output :: [Int],
    tiles :: Tiles,
    paddleX :: Int,
    joystick :: Integer
     } deriving Show

type Position = (Int, Int)
type Tiles = Map.HashMap Position Int

states :: Program -> State -> [State]
states program initial = allStates
  where allStates = scanl stepState initial outputChunks
        outputChunks = chunks 3 $ map fromInteger $ outputSequence program inputs
        inputs = map input $ filter relevant allStates

initial :: State
initial = State {output=[0,0,0], tiles = Map.empty, paddleX = 21, joystick = 0}

add (x1, y1) (x2, y2) = (x1+x2, y1+y2)

relevant state = (id == 4)
  where [x,y,id] = output state

stepState :: State -> [Int] -> State
stepState state out@[x,y,id] = case id of
    3 -> state' { paddleX = x}
    4 -> state' { joystick = if x > px then 1 else if x < px then -1 else 0}
    _ -> state'
  where state' = state {output = out, tiles = Map.insert (x,y) id $ tiles state}
        px = paddleX state

input :: State -> Integer
input state = joystick state

display :: Tiles -> [String]
display tiles = [[at x y | x <- [0..42]] | y <- [0..22]]
  where at x y = tile $ fromMaybe 0 $ Map.lookup (x,y) tiles

main = do
    contents <- readFile "Day13.txt"
    let program = parse contents
    let output = states program initial
    clearScreen
    hideCursor
    setCursorPosition 0 0
    putStr $ unlines $ display $ tiles $ last $ take ((23*43)+1) output
    mapM screenOutput $ drop ((23*43)+1) output
    setCursorPosition 26 0
    showCursor

screenOutput State {output = [-1, 0, score]} = do
    setCursorPosition 24 0
    putStr $ show score

screenOutput state @ State {output=[x, y, id]} = do
    setCursorPosition y x
    putChar $ tile id
    setCursorPosition 25 0
