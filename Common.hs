module Common where

import Data.Char
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP

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
untilStable = snd . last . takeWhile notSame . oneAndNext

replace :: Int -> a -> [a] -> [a]
replace n x xs = (take n xs) ++ [x] ++ (drop (n+1) xs)

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)

sortOrder :: (a -> a -> Ordering) -> [a] -> [Int]
sortOrder f list = map fst $ sortBy (\x y -> f (snd x) (snd y)) $ indexed list

indexed :: [a] -> [(Int, a)]
indexed list = zip [0..(length list)] list