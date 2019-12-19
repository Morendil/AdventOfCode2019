module Common where

import Data.Char
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP
import Data.Eq.HT

readInt :: String -> Integer
readInt = read

comma = satisfy (',' ==)

number = do
  sign <- option ' ' (char '-')
  magnitude <- many1 $ satisfy isDigit
  return $ readInt (sign : magnitude)

numberList = sepBy1 number comma

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

notSame :: Eq a => (a, a) -> Bool
notSame (a, b) = a /= b

oneAndNext :: [a] -> [(a,a)]
oneAndNext sequence = zip sequence (tail sequence)

untilStable :: Eq a => [a] -> a
untilStable seq = if null pairs then head seq else snd $ last pairs
  where pairs = takeWhile notSame $ oneAndNext seq

replace :: Int -> a -> [a] -> [a]
replace n x xs = (take n xs) ++ [x] ++ (drop (n+1) xs)

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)

sortOrder :: (a -> a -> Ordering) -> [a] -> [Int]
sortOrder f list = map fst $ sortBy (\x y -> f (snd x) (snd y)) $ indexed list

indexed :: [a] -> [(Int, a)]
indexed list = zip [0..(length list)] list

partitionOn f = (groupBy $ equating f) . (sortBy $ comparing f)

cross :: Eq a => [a] -> [[(a,a)]]
cross list = [[(a,b) | b<-list , a /= b] | a <- list]

crossWith :: Eq a => (a -> a -> b) -> [a] -> [[b]]
crossWith f list = [[f a b | b<-list , a /= b] | a <- list]