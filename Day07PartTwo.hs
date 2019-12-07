module Day07PartTwo where

import Common
import Data.List
import Data.Maybe
import IntCode

maxSignal :: String -> Integer
maxSignal program = maximum $ map (loopedSignal $ parse program) $ permutations [5..9]

loopedSignal :: Program -> [Integer] -> Integer
loopedSignal program inputs = last $ last $ wireLoop program (seed $ transpose [inputs])
  where seed (x:xs) = ((x++[0]):xs)

wireLoop :: Program -> [[Integer]] -> [[Integer]]
wireLoop program [sa, sb, sc, sd, se] = [aOut, bOut, cOut, dOut, eOut]
    where aOut = outputSequence program (sa ++ eOut)
          bOut = outputSequence program (sb ++ aOut)
          cOut = outputSequence program (sc ++ bOut)
          dOut = outputSequence program (sd ++ cOut)
          eOut = outputSequence program (se ++ dOut)

outputSequence :: Program -> [Integer] -> [Integer]
outputSequence program inputs = mapMaybe onChange $ oneAndNext $ map output $ execSequence program inputs
  where output (_, _, out) = out
        onChange (one, next) = if one == next then Nothing else Just $ last next

execSequence program inputs = map snd $ takeWhile notSame $ oneAndNext $ map dropInputs $ iterate step (0, program, inputs, [])
  where dropInputs (pos, program, inputs, outputs) = (pos, program, outputs)

main = do
    contents <- readFile "Day07.txt"
    print $ maxSignal contents