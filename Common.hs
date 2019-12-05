module Common where

import Data.Char
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

untilStable :: Eq a => [a] -> a
untilStable sequence = snd $ last $ takeWhile notSame $ oneAndNext
  where oneAndNext = zip sequence (tail sequence)
        notSame (a, b) = a /= b

replace :: Int -> a -> [a] -> [a]
replace n x xs = (take n xs) ++ [x] ++ (drop (n+1) xs)
