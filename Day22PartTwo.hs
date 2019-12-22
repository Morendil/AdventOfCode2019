module Day22PartTwo where

import Common
import Data.Maybe
import Data.Tuple
import Data.List
import Text.ParserCombinators.ReadP

import Debug.Trace

data Move = Cut Int | Deal Int | NewStack deriving (Eq, Show)

parseCut = do
    string "cut "
    result <- number
    return $ Cut $ fromInteger result

parseNewStack = do
    string "deal into new stack"
    return NewStack

parseDeal = do
    string "deal with increment "
    result <- number
    return $ Deal $ fromInteger result

parseShuffle = sepBy1 (choice [parseCut, parseDeal, parseNewStack]) (string "\n")

parse :: String -> [Move]
parse = fromJust . parseMaybe parseShuffle

result :: [Move] ->  [Int] -> [Int]
result moves deck = foldl (flip applyMove) deck moves

simplifyAll :: Int -> [Move] -> [Move]
simplifyAll len = untilStable . iterate (simplify len)

simplify :: Int -> [Move] -> [Move]
simplify len moves@(NewStack:rest) = if elem NewStack rest then (cancelNew $ simplifyAll len before)++after else (NewStack:simplify len rest)
  where before = takeWhile (/= NewStack) rest
        after = drop ((length before)+1) rest
        cancelNew [Deal n] = [Deal n, Cut (1-n)]
        cancelNew [Cut n] = [Cut (-n)]
        cancelNew [Deal m, Cut n] = [Deal m, Cut (1-m-n)]
        cancelNew x = x
simplify len (one:two:rest) = e1++(simplify len $ e2++rest)
  where e1 = take 1 s
        e2 = drop 1 s
        s = simplifyPair len one two
simplify len [x] = [x]
simplify len [] = []

simplifyPair :: Int -> Move -> Move -> [Move]
simplifyPair len (Deal n) (Deal m) = [Deal ((n*m) `mod` len)]
simplifyPair len (Cut n) (Cut m) = [Cut ((n+m) `mod` len)]
simplifyPair len (Cut n) (Deal m) = [Deal m, Cut ((n*m) `mod` len)]
simplifyPair len x y = [x,y]

isDeal (Deal n) = True
isDeal _ = False

applyMove :: Move -> [Int] -> [Int]
applyMove NewStack deck = reverse deck
applyMove (Cut pos) deck = if pos >= 0 then (drop m deck) ++ (take m deck) else (drop (len-m) deck) ++ (take (len-m) deck)
  where m = (abs pos) `mod` len
        len = length deck
applyMove (Deal pos) deck = map (\p -> deck !! snd p) $ sort $ map (\n -> ((n*pos) `mod` len, n)) [0..(len-1)]
  where len = length deck

modPow :: Integer -> Integer -> Integer -> Integer
modPow b e m | e >= 0 = powm b e m 1
modPow b e m | e < 0 = modInv (powm b (-e) m 1) m

powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r
  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

modInv a m = modPow a (m-2) m

cardAt deal cut len pos = (inv * (pos+cut)) `mod` len
  where inv = modInv deal len

-- cribbed from https://www.nayuki.io/page/fast-skipping-in-a-linear-congruential-generator
indexOfRepeat repeats deal cut len value = (fstTerm + sndTerm) `mod` len
  where a1 = deal - 1
        ma = a1 * len
        sndTerm = ((modPow deal repeats ma) - 1) `div` a1 * cut
        fstTerm = (modPow deal repeats len) * value

cardAtRepeat repeats deal cut len value = (fstTerm + sndTerm) `mod` len
  where a = modInv deal len
        b = (-a) * cut
        a1 = a - 1
        ma = a1 * len
        sndTerm = ((modPow a repeats ma) - 1) `div` a1 * b
        fstTerm = (modPow a repeats len) * value

main = do
    contents <- readFile "Day22.txt"
    print $ elemIndex 2019 $ result (simplifyAll 10007 $ parse contents) [0..10006] -- [Deal 1159,Cut 646]
    print $ simplifyAll 119315717514047 $ parse contents
    print $ cardAtRepeat 101741582076661 10563787764654 (-80207649341109) 119315717514047 2020
    -- [Deal 10563787764654,Cut 80207649341109]
