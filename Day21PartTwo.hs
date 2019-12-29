module Main where

import Common
import IntCodeM
import Data.Char
import Data.Maybe
import Data.List
import Control.Monad

import Debug.Trace

fromAscii = map (chr.fromInteger)
toAscii = map (toInteger.ord)

decision' :: [Bool] -> Bool
decision' [a,b,c,d,e,f,g,h,i] = not ((a && b && (c || f)) || (a && e && (i || f)))

decision :: [Bool] -> Maybe Bool
decision [True,True,False,True,True,True,False,True,False] = Just True
decision (True:True:True:True:rest) = Just False
decision tiles = if bigGaps (take 8 continue) || deadEitherWay continue then Nothing
    else if tiles !! 0 && not (tiles !! 3) then (if isJust ifNotJump then Just False else Nothing)
    else if not (tiles !! 0) && tiles !! 3 then (if isJust ifJump then Just True else Nothing)
    else if isJust ifNotJump then Just False else if isJust ifJump then Just True else Nothing
  where ifNotJump = decision $ (tail continue)
        ifJump = decision $ (drop 4 continue)
        continue = tiles ++ (repeat True)

outcome :: Bool -> [Bool] -> Bool
outcome False (False:rest) = False
outcome True (_:_:_:False:rest) = False
outcome _ _ = True

bigGaps :: [Bool] -> Bool
bigGaps = any (\g -> (not.head) g && length g > 3) . group

deadEitherWay :: [Bool] -> Bool
deadEitherWay tiles = not ((tiles !! 0) || (tiles !! 3))

expand [a,b,c,d,e,f] = [a,b,c,True,d,e,True,True,f]

diff n = filter (\s -> (isJust (withTrue s) || isJust (withFalse s)) && withTrue s /= withFalse s) $ sequence $ replicate 9 [False, True]
  where withTrue s = decision $ replace n True s
        withFalse s = decision $ replace n False s

minterms :: [[Maybe Bool]]
minterms = map (map Just) $ filter (\x -> (decision x == Just True) || (decision x == Nothing)) $ sequence $ replicate 9 [False, True]

differByOne :: Eq a => [a] -> [a] -> (Bool, Int)
differByOne [] [] = (False, 0)
differByOne (a:as) (b:bs) = if a /= b
    then if as == bs then (True, 0) else (False, 0)
    else (result, index+1)
  where (result, index) = differByOne as bs

combine :: [Maybe Bool] -> [Maybe Bool] -> Maybe [Maybe Bool]
combine a b = if fst diff then Just $ replace (snd diff) Nothing a else Nothing
  where diff = differByOne a b

recombine :: [[Maybe Bool]] -> [[Maybe Bool]]
recombine = catMaybes . concat . crossWith combine

repBin True = '1'
repBin False = '0'

repTer (Just True) = '1'
repTer (Just False) = '0'
repTer (Nothing) = '-'

represent (input, output) = (map repBin input) ++ " " ++ [repTer output]

main = do
    contents <- readFile "Day21.txt"
    putStrLn $ fromAscii $ execute (toAscii "NOT A J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\nNOT H T\nNOT T T\nOR E T\nAND T J\nRUN\n") contents
