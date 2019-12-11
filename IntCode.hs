module IntCode where
  
import Common
import Data.Maybe
import qualified Data.HashMap.Strict as Map

type Program = [Integer]
type Inputs = [Integer]
type Outputs = [Integer]
type Memory = Map.HashMap Integer Integer
type State = (Int, Program, Inputs, Outputs, Integer, Memory)

initialize :: Program -> Inputs -> State
initialize program inputs = (0, program, inputs, [], 0, Map.empty)

execute :: Inputs -> String -> Outputs
execute inputs = run inputs . parse

finalProgram :: String -> Program
finalProgram program = get $ untilStable $ iterate step $ initialize (parse program) []
    where get (_, p, _, _, _, _) = p

parse :: String -> Program
parse program = fromMaybe [] $ parseMaybe numberList program

run :: Inputs -> Program -> Outputs
run inputs program = outputs $ untilStable $ iterate step $ initialize program inputs
    where outputs (_, _, _, v,_,_) = v

outputSequence :: Program -> [Integer] -> [Integer]
outputSequence program inputs = mapMaybe onChange $ oneAndNext $ map output $ execSequence program inputs
  where output (_, _, out) = out
        onChange (one, next) = if one == next then Nothing else Just $ last next

execSequence program inputs = map snd $ takeWhile notSame $ oneAndNext $ map dropInputs $ iterate step $ initialize program inputs
  where dropInputs (pos, program, inputs, outputs, base, memory) = (pos, program, outputs)

write :: Integer -> Integer -> State -> State
write n value (pos, program, inputs, outputs, base, memory) = if fromInteger n < length program
    then (pos, replace (fromInteger n) value program, inputs, outputs, base, memory)
    else (pos, program, inputs, outputs, base, Map.insert n value memory)

step :: State -> State
step state@(pos, program, inputs, outputs, base, memory) = case op of
        1 -> write result (x+y) (next 4, program, inputs, outputs, base, memory)
        2 -> write result (x*y) (next 4, program, inputs, outputs, base, memory)
        3 -> write (put 1) (head inputs) (next 2, program, tail inputs, outputs, base, memory)
        4 -> (next 2, program, inputs, outputs ++ [x], base, memory)
        5 -> (if x /= 0 then fromInteger y else next 3, program, inputs, outputs, base, memory)
        6 -> (if x == 0 then fromInteger y else next 3, program, inputs, outputs, base, memory)
        7 -> write result (if x < y then 1 else 0) (next 4, program, inputs, outputs, base, memory)
        8 -> write result (if x == y then 1 else 0) (next 4, program, inputs, outputs, base, memory)
        9 -> (next 2, program, inputs, outputs, base + x, memory)
        99 -> state
        otherwise -> error ("HCF(op="++show op++")"++show (pos, program))
    where next count = pos + count
          opcode = access pos
          op = opcode `mod` 100
          fetch n = case mode n of
            0 -> value n
            1 -> immediate n
            2 -> relative n
            _ -> error ("HCF(mode="++show (mode n)++")"++show (pos, program))
          put n = case mode n of
            0 -> addr n
            2 -> fromInteger $ base + (immediate n)
            _ -> error ("HCF(write mode="++show (mode n)++")"++show (pos, program))
          mode n = (opcode `div` (10^(1+n))) `mod` 10
          access n = if n < length program then program !! n else Map.lookupDefault 0 (toInteger n) memory
          immediate n = access (pos+n)
          addr n = fromInteger $ immediate n
          value n = access (addr n)
          relative n = access $ fromInteger $ base + (immediate n)
          -- shortcuts for opcodes 1, 2
          result = put 3
          x = fetch 1
          y = fetch 2
