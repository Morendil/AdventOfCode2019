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

type State = ([Int], (Int, Int), Int, Int)

states :: Program -> State -> [State]
states program initial = allStates
  where allStates = scanl stepState initial outputChunks
        outputChunks = chunks 3 $ map fromInteger $ outputSequence program inputs
        inputs = map input $ filter relevant allStates

initial :: State
initial = ([0,0,0], (0, 0), 21, 1)

relevant ([x,y,id],_,_,_) = (id == 4)

stepState :: State -> [Int] -> State
stepState (_, pos@(ballX, ballY), paddleX, ballDir) out@[x,y,id] = case id of
    3 -> (out, pos, x, ballDir)
    4 -> (out, (x,y), paddleX, if x > ballX then 1 else -1)
    _ -> (out, pos, paddleX, ballDir)

input :: State -> Integer
input (_, (ballX, ballY), paddleX, ballDir) = if targetX > paddleX then 1 else if targetX < paddleX then -1 else 0
  where targetX = ballX + ballDir

getOut :: State -> [Int]
getOut (out, _, _, _) = out

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

screenOutput ([-1, 0, score], _, _, _) = do
    setCursorPosition 24 0
    putStrLn $ show score

screenOutput state@([x, y, id], _, _, _) = do
    setCursorPosition y x
    putChar $ tile id
    setCursorPosition 25 0
    putStr $ show state

screenOutputSlow [-1, 0, score] = do
    setCursorPosition 24 0
    putStrLn $ show score
    
wait x = do
    x
    threadDelay 1000000
