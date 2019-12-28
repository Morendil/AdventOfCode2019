module IntCodeM where
  
import Common
import Data.Maybe
import Data.List
import Data.List.Tools
import qualified Data.IntMap as Map
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Unboxed as V

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.Loops

import Debug.Trace

type Program = [Integer]
type Inputs = [Integer]
type Outputs = [Integer]
type Memory m = VM.MVector (PrimState m) Int
data State m = State {
  pc :: Int,
  cycles :: Int,
  halt :: Bool,
  input :: Inputs,
  lastIn :: Maybe Integer,
  output :: Outputs,
  base :: Int,
  codeLength :: Int,
  maxM :: Int,
  memory :: Memory m }

initialize :: PrimMonad m => Program -> Inputs -> m (State m)
initialize program inputs = do
    loaded <- load program
    return State {
        pc=0,
        maxM=0,
        halt = False,
        cycles=0,
        input=inputs,
        lastIn=Nothing,
        output=[],
        base=0,
        codeLength=length program,
        memory=loaded }

load :: PrimMonad m => Program -> m (Memory m)
load program = do
    initial <- V.thaw $ V.fromList $ map fromInteger program
    grown <- VM.grow initial (10 * (VM.length initial))
    return grown

execute :: Inputs -> String -> Outputs
execute inputs = run inputs . parse

parse :: String -> Program
parse program = fromMaybe [] $ parseMaybe numberList program

run :: Inputs -> Program -> Outputs
run inputs program = runST $ do
    firstState <- initialize program inputs
    lastState <- iterateUntilM (halt) step firstState
    return $ output lastState

fetch:: PrimMonad m => State m -> Int -> Int -> m Int
fetch state mode n = case mode of
    0 -> join $ value n
    1 -> immediate n
    2 -> join $ relative n
    _ -> error ("HCF(mode="++show mode++")"++show (pc state))
  where access n = VM.read (memory state) n          
        immediate n = access ((pc state)+n)
        value n = do addr <- immediate n; return $ access addr
        relative n = do addr <- immediate n; return $ access $ (base state) + addr

write :: PrimMonad m => State m -> Int -> Int -> Int -> m ()
write state mode n value = case mode of
    0 -> go $ (pc state) + n
    2 -> do offset <- fetch state immediate ((pc state)+n); go $ (base state) + offset
    _ -> error ("HCF(write mode="++show mode++")"++show (pc state))
  where go n = VM.write (memory state) n value
        immediate = 1

step :: PrimMonad m => State m -> m (State m)
step state = do
    opcode <- fetch state immediate 0
    let mode n = (opcode `div` (10^(1+n))) `mod` 10
        op = opcode `mod` 100
    x <- fetch state (mode 1) 1
    y <- fetch state (mode 2) 2
    case op of
        1 -> do write state (mode 3) 3 (x+y) ; return $ state' { pc = next 4 }
        2 -> do write state (mode 3) 3 (x*y) ; return $ state' { pc = next 4 }
        3 -> do write state (mode 1) 1 (fromInteger $ head $ input state) ; return $ state' { pc = next 2, input = tail $ input state, lastIn=Just (head $ input state) }
        4 -> return state' { pc = next 2, output = (output state)++[toInteger x] }
        5 -> return state' { pc = if x /= 0 then y else next 3 }
        6 -> return state' { pc = if x == 0 then y else next 3 }
        7 -> do write state (mode 3) 3 (if x < y then 1 else 0) ; return state' { pc = next 4 }
        8 -> do write state (mode 3) 3 (if x == y then 1 else 0) ; return state' { pc = next 4 }
        9 -> return state' { pc = next 2, base = (base state) + x }
        99 -> return state' { halt = True}
        _ -> error ("HCF(op="++show op++")"++show (pc state))
    where state' = state { cycles = cycles state + 1}
          next count = (pc state) + count 
          immediate = 1                   
