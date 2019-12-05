module IntCode where
  
import Common
import Data.Maybe (fromMaybe)

type Program = [Integer]
type Inputs = [Integer]
type Outputs = [Integer]
type State = (Int, Program, Inputs, Outputs)

execute :: Inputs -> String -> Outputs
execute inputs = run inputs . parse

finalProgram :: String -> Program
finalProgram program = get $ untilStable $ iterate step (0, parse program, [], [])
    where get (_, p, _, _) = p

parse :: String -> Program
parse program = fromMaybe [] $ parseMaybe numberList program

run :: Inputs -> Program -> Outputs
run inputs program = outputs $ untilStable $ iterate step (0, program, inputs, [])
    where outputs (_, _, _, v) = v

step :: State -> State
step state@(pos, program, inputs, outputs) = case op of
        1 -> (next 4, replace output (x+y) program, inputs, outputs)
        2 -> (next 4, replace output (x*y) program, inputs, outputs)
        3 -> (next 2, replace (addr 1) (head inputs) program, inputs, outputs)
        4 -> (next 2, program, inputs, outputs ++ [fetch 1])
        5 -> (if x /= 0 then fromInteger y else next 3, program, inputs, outputs)
        6 -> (if x == 0 then fromInteger y else next 3, program, inputs, outputs)
        7 -> (next 4, replace output (if x < y then 1 else 0) program, inputs, outputs)
        8 -> (next 4, replace output (if x == y then 1 else 0) program, inputs, outputs)
        99 -> state
    where next count = pos + count
          opcode = (program !! pos)
          op = opcode `mod` 100
          fetch n = if mode n == 1 then immediate n else value n
          mode n = (opcode `div` (10^(1+n))) `mod` 10
          immediate n = program !! (pos+n)
          addr n = fromInteger $ immediate n
          value n = program !! (addr n)
          -- shortcuts for opcodes 1, 2
          output = addr 3
          x = fetch 1
          y = fetch 2
