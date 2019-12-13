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
    ballPos :: Position,
    ballDir :: Position,
    predicted :: Position,
    next :: Maybe Position,
    paddleX :: Int
     } deriving Show

type Position = (Int, Int)
type Tiles = Map.HashMap Position Int

states :: Program -> State -> [State]
states program initial = allStates
  where allStates = scanl stepState initial outputChunks
        outputChunks = chunks 3 $ map fromInteger $ outputSequence program inputs
        inputs = map input $ filter relevant allStates

-- the predicted ball position is computed manually to avoid having to inject tiles and predicting here
initial :: State
initial = State {output=[0,0,0], tiles = Map.empty, ballPos=(19, 18), paddleX=21, ballDir=(1,1), predicted = (21,20), next = Nothing}

add (x1, y1) (x2, y2) = (x1+x2, y1+y2)

-- we do the next/predicted shuffle to avoid pushing the joystick *exactly* as the ball bounces
predictNext :: State -> State
predictNext state = error $ show (ballPos state, ballDir state, predictPath (ballPos state) (ballDir state) (tiles state))
    -- state {next = Just $ last $ predictPath (ballPos state) (ballDir state) (tiles state)}

predictPath :: Position -> Position -> Tiles -> [Position]
predictPath pos@(px, py) dir@(vx, vy) tiles = if (hy == 20) && (vy == 1)
    then finalPath
    else finalPath ++ tail (predictPath hitPosition bounceDir tiles)
  where hitPosition@(hx,hy) = if null projectedPath then pos else add dir $ last $ projectedPath
        projectedPath = takeWhile (\pos -> pathClear pos dir tiles) $ iterate (add dir) pos
        finalPath = projectedPath ++ [hitPosition]
        bounceDir@(bx,by) = whichBounce pos dir tiles

pathClear (px, py) (vx, vy) tiles = clearLeft && clearFwd && clearRight
  where clearFwd = clear (vx, vy)
        clearLeft = clear $ left (vx, vy)
        clearRight = clear $ right (vx, vy)
        clear (dx, dy) = ((py+dy) <= 21) && ((tile == Just 0) || (tile == Just 4))
          where tile = Map.lookup (px+dx,py+dy) $ tiles

left (1,1) = (1,0)
left (-1,1) = (0,1)
left (-1,-1) = (-1,0)
left (1,-1) = (0,-1)
right = swap . left

whichBounce (px, py) (vx, vy) tiles = if not clearLeft then (-vy, vx) else if not clearFwd then (-vx,-vy) else (vy,-vx)
  where clearFwd = clear (vx, vy)
        clearLeft = clear $ left (vx, vy)
        clearRight = clear $ right (vx, vy)
        clear (dx, dy) = (tile == Just 0) || (tile == Just 4)
          where tile = Map.lookup (px+dx,py+dy) $ tiles

relevant state = (id == 4)
  where [x,y,id] = output state

stepState :: State -> [Int] -> State
stepState state out@[x,y,id] = case id of
    3 -> state' {paddleX = x}
    -- on observing ball hit paddle we predict all the bounces to the next paddle hit
    4 -> if y == 20 && vy == 1 then ballBounced else ballMoved
    _ -> state'
  where (px, py) = ballPos state
        (vx, vy) = ballDir state
        state' = state {output = out, tiles = Map.insert (x,y) id $ tiles state}
        deferred = state' {predicted = fromMaybe (predicted state) (next state), next = Nothing}
        ballBounced = predictNext $ state' {ballPos = (x+vx, 19), ballDir = (vx, -1)} 
        ballMoved = deferred {ballPos = (x,y), ballDir = (if x>px then 1 else -1, if y>py then 1 else -1)}

input :: State -> Integer
input state = if targetX > paddleX state then 1 else if targetX < paddleX state then -1 else 0
  where (targetX, targetY) = predicted state

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
    threadDelay 500000
    mapM (wait . screenOutput) $ drop ((23*43)+1) output
    setCursorPosition 26 0
    showCursor

screenOutput State {output = [-1, 0, score]} = do
    setCursorPosition 24 0
    putStrLn $ show score

screenOutput state @ State {output=[x, y, id]} = do
    setCursorPosition y x
    putChar $ tile id
    setCursorPosition 25 0
    putStr $ show $ state {tiles = Map.empty}
    
wait x = do
    x
    threadDelay 1000000
