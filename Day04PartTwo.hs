module Day04PartTwo where

import Day04
import Data.Char
import Data.Either
import Data.Maybe

accept :: Int -> Bool
accept n = fst $ fromRight initial $ foldl examinePair (Right initial) $ pairs n
  where initial :: HasDouble
        initial = (False, 0)

type Reject = ()
type HasDouble = (Bool, Int)
type Status = Either Reject HasDouble

examinePair :: Status -> (Int, Int) -> Status
examinePair (Left ()) _ = Left ()
examinePair _ (a, b) | a > b = Left ()
examinePair state@(Right (False, c)) (a, b) | a == b = if c /= a then Right (True, a) else state
examinePair state@(Right (True, c)) (a, b) | a == b = if c == a then Right (False, a) else state
examinePair state _ = state

countAccepted :: Int -> Int -> Int
countAccepted low high = length $ filter accept [low..high]

main = print $ countAccepted 152085 670283