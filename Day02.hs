module Day02 where
  
import Common
import Data.Maybe (fromMaybe)

type Program = [Integer]
type State = (Int, Program)

parse :: String -> Program
parse program = fromMaybe [] $ parseMaybe numberList program

run :: Program -> Program
run program = snd $ untilStable $ iterate step (0, program)

step :: State -> State
step (pos, program) = case op of
    1 -> (next, replace output (x+y) program)
    2 -> (next, replace output (x*y) program)
    99 -> (pos, program)
  where next = pos + 4
        op = program !! pos
        output = fromInteger $ program !! (pos+3)
        x = program !! fromInteger (program !! (pos+1))
        y = program !! fromInteger (program !! (pos+2))
