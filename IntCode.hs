module IntCode where
  
import Common
import Data.Maybe
import qualified Data.HashMap.Strict as Map

import Debug.Trace

type Program = [Integer]
type Inputs = [Integer]
type Outputs = [Integer]
type Memory = Map.HashMap Integer Integer
data State = State {
  pc :: Int,
  cycles :: Int,
  halt :: Bool,
  code :: Program,
  input :: Inputs,
  lastIn :: Maybe Integer,
  output :: Outputs,
  base :: Integer,
  memory :: Memory } deriving (Eq, Show)

initialize :: Program -> Inputs -> State
initialize program inputs = State { pc=0, halt = False, cycles=0, code=program, input=inputs, lastIn=Nothing, output=[], base=0, memory=Map.empty }

execute :: Inputs -> String -> Outputs
execute inputs = run inputs . parse

finalProgram :: String -> Program
finalProgram program = code $ untilStable $ iterate step $ initialize (parse program) []

parse :: String -> Program
parse program = fromMaybe [] $ parseMaybe numberList program

run :: Inputs -> Program -> Outputs
run inputs program = output $ untilStable $ iterate step $ initialize program inputs

out (_,_,v) = v

outputSequence :: Program -> [Integer] -> [Integer]
outputSequence program inputs = mapMaybe onChange $ oneAndNext $ map out $ execSequence program inputs
  where onChange (one, next) = if one == next then Nothing else Just $ last next

execSequence program inputs = map snd $ takeWhile notSame $ oneAndNext $ map dropInputs $ iterate step $ initialize program inputs
  where dropInputs state = (pc state, code state, output state)

write :: Integer -> Integer -> State -> State
write n value state = if (fromInteger n) < (length $ code state)
    then state { code = replace (fromInteger n) value (code state) }
    else state { memory = Map.insert n value (memory state)}

step :: State -> State
step state = case op of
        1 -> write result (x+y) $ state' { pc = next 4 }
        2 -> write result (x*y) $ state' { pc = next 4 }
        3 -> write (put 1) (head $ input state) $ state' { pc = next 2, input = tail $ input state, lastIn=Just (head $ input state) }
        4 -> state' { pc = next 2, output = (output state)++[x] }
        5 -> state' { pc = if x /= 0 then fromInteger y else next 3 }
        6 -> state' { pc = if x == 0 then fromInteger y else next 3 }
        7 -> write result (if x < y then 1 else 0) $ state' { pc = next 4 }
        8 -> write result (if x == y then 1 else 0) $ state' { pc = next 4 }
        9 -> state' { pc = next 2, base = (base state) + x }
        99 -> state { halt = True}
        _ -> error ("HCF(op="++show op++")"++show (pc state, code state))
    where state' = state { cycles = cycles state + 1}
          next count = (pc state) + count
          opcode = access (pc state)
          op = opcode `mod` 100
          fetch n = case mode n of
            0 -> value n
            1 -> immediate n
            2 -> relative n
            _ -> error ("HCF(mode="++show (mode n)++")"++show (pc state, code state))
          put n = case mode n of
            0 -> addr n
            2 -> fromInteger $ (base state) + (immediate n)
            _ -> error ("HCF(write mode="++show (mode n)++")"++show (pc state, code state))
          mode n = (opcode `div` (10^(1+n))) `mod` 10
          access n = if n < length (code state) then (code state) !! n else Map.lookupDefault 0 (toInteger n) (memory state)
          immediate n = access ((pc state)+n)
          addr n = fromInteger $ immediate n
          value n = access (addr n)
          relative n = access $ fromInteger $ (base state) + (immediate n)
          -- shortcuts for opcodes 1, 2
          result = put 3
          x = fetch 1
          y = fetch 2
