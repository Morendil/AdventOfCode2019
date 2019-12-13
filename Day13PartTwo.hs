module Day13PartTwo where

import Common
import IntCode hiding (State)
import System.Console.ANSI
import Control.Concurrent

tile 1 = '#'
tile 2 = '*'
tile 3 = '_'
tile 4 = 'o'
tile _ = '.'

data State = State { output :: [Int], ballPos :: (Int, Int), paddleX :: Int, ballDir :: Int } deriving Show

states :: Program -> State -> [State]
states program initial = allStates
  where allStates = scanl stepState initial outputChunks
        outputChunks = chunks 3 $ map fromInteger $ outputSequence program inputs
        inputs = map input $ filter relevant allStates

initial :: State
initial = State {output=[0,0,0], ballPos=(0, 0), paddleX=21, ballDir=1}

relevant state = (id == 4)
  where [x,y,id] = output state

stepState :: State -> [Int] -> State
stepState state out@[x,y,id] = case id of
    3 -> state' {paddleX = x}
    4 -> state' {ballPos = (x,y), ballDir = if x > ballX then 1 else -1}
    _ -> state'
  where (ballX, _) = ballPos state
        state' = state {output = out}

input :: State -> Integer
input state = if targetX > paddleX state then 1 else if targetX < paddleX state then -1 else 0
  where targetX = ballX + ballDir state
        (ballX, _) = ballPos state

main = do
    contents <- readFile "Day13.txt"
    let program = parse contents
    let output = states program initial
    clearScreen
    hideCursor
    setCursorPosition 0 0
    mapM screenOutput $ take (23*43) output
    mapM (wait . screenOutput) $ drop (23*43) output
    setCursorPosition 26 0
    showCursor

screenOutput State {output = [-1, 0, score]} = do
    setCursorPosition 24 0
    putStrLn $ show score

screenOutput state @ State {output=[x, y, id]} = do
    setCursorPosition y x
    putChar $ tile id
    setCursorPosition 25 0
    putStr $ show state
    
wait x = do
    x
    threadDelay 10000
