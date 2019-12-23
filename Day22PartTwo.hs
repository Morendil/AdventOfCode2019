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

result :: [Move] -> [Int] -> [Int]
result moves deck = foldl (flip applyMove) deck moves

simplifyAll :: Int -> [Move] -> [Move]
simplifyAll len = untilStable . iterate (simplify len)

simplify :: Int -> [Move] -> [Move]
simplify len moves@(NewStack:rest) = if elem NewStack rest then (cancelNew $ simplifyAll len before)++after else (NewStack:simplify len rest)
  where before = takeWhile (/= NewStack) rest
        after = drop ((length before)+1) rest
        cancelNew [Deal n] = [Deal n, Cut ((1-n) `mod` len)]
        cancelNew [Cut n] = [Cut ((-n) `mod` len)]
        cancelNew [Deal m, Cut n] = [Deal m, Cut ((1-m-n) `mod` len)]
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

toTuple :: Integer -> Move -> (Integer, Integer, Integer)
toTuple len (NewStack) = (-1, -1, len)
toTuple len (Deal n) = (toInteger n, 0, len)
toTuple len (Cut n) = (1, toInteger (-n), len)

modPow :: Integer -> Integer -> Integer -> Integer
modPow b e m | e >= 0 = powm b e m 1
modPow b e m | e < 0 = modInv (powm b (-e) m 1) m

powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r
  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

modInv a m = modPow a (m-2) m

cardAt deal cut len pos = apply (inverse (deal, cut, len)) pos
indexOf deal cut len value = apply (deal, cut, len) value

apply (deal, cut, len) value = ((deal*value) + cut) `mod` len

inverse (deal, cut, len) = (inv, ((-cut)*inv) `mod` len, len)
  where inv = modInv deal len

mul (d1, c1, l1) (d2, c2, l2) = ((d1*d2) `mod` l1, (((c1*d2)+c2) `mod` l1), l1)

pow base n = foldr1 mul $ map ((!!) (powers (len+1) base)) $ findIndices (==1) $ binary
  where binary = toBinary n
        len = length binary

toBinary = unfoldr toBinary'
  where toBinary' 0 = Nothing
        toBinary' n = Just (n `mod` 2, n `div` 2)

powers n m = take n $ iterate (\m -> mul m m) m

main = do
  contents <- readFile "Day22.txt"
  -- let simplified = simplifyAll 10007 $ parse contents
  --     repeated = (iterate (result simplified) [0..10006]) !! 5
  -- print $ elemIndex 2019 $ result simplified [0..10006] -- [Deal 1159,Cut 646]
  -- print $ indexOf 1159 (-646) 10007 2019
  -- print $ cardAt 1159 (-646) 10007 $ indexOf 1159 (-646) 10007 2019
  -- print $ elemIndex 2019 $ repeated -- [Deal 1159,Cut 646]
  -- print $ apply (pow (1159, -646, 10007) 5) 2019
  -- print $ apply (pow (inverse (1159, -646, 10007)) 5) $ apply (pow (1159, -646, 10007) 5) 2019
  -- print $ simplifyAll 119315717514047 $ parse contents
  -- print $ foldl1 mul $ map (toTuple 10007) $ parse contents
  -- print $ foldl1 mul $ map (toTuple 119315717514047) $ parse contents
  print $ apply (pow (inverse $ foldl1 mul $ map (toTuple 119315717514047) $ parse contents) 101741582076661) 2020
