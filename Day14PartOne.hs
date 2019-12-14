module Day14PartOne where

import Common
import Data.Maybe
import Data.Char
import Data.List
import Data.Bifunctor
import Text.ParserCombinators.ReadP

type Element = (Integer, String)
type Reaction = (Element, [Element])

element = do
    quantity <- number
    string " "
    name <- many1 (satisfy isAlpha)
    return (quantity, name)

parseReaction = do
    inputs <- sepBy1 element (string ", ")
    string " => "
    output <- element
    return (output, inputs)

fulfill :: [Reaction] -> [Element] -> [Element]
fulfill reactions = untilStable . iterate (step reactions)

step :: [Reaction] -> [Element] -> [Element]
step reactions = aggregate . (fulfillNonExact reactions) . aggregate . (fulfillAllExact reactions)

fulfillAllExact :: [Reaction] -> [Element] -> [Element]
fulfillAllExact reactions = untilStable . iterate (fulfillExact reactions)

fulfillExact :: [Reaction] -> [Element] -> [Element]
fulfillExact reactions required = concatMap (inputs reactions) required

fulfillNonExact :: [Reaction] -> [Element] -> [Element]
fulfillNonExact reactions required = concatMap (largerInputs reactions) required

inputs :: [Reaction] -> Element -> [Element]
inputs reactions element = fromMaybe [element] $ lookup element reactions

producing :: [Reaction] -> Element -> Maybe Reaction
producing reactions element = find (produces element) reactions
  where produces (_, label) ((_, outl), _) = label == outl

enoughFor :: Element -> Reaction -> [Element]
enoughFor elem@(quantity, _) _ | quantity < 0 = [elem]
enoughFor (quantity, label) ((outq,_), inputs) = leftovers ++ map (bimap (* numberNeeded) id) inputs
  where numberNeeded = (quantity-1) `div` outq+1
        unconsumed = (numberNeeded * outq) - quantity
        leftovers = if unconsumed == 0 then [] else [(-unconsumed, label)]

largerInputs :: [Reaction] -> Element -> [Element]
largerInputs reactions element = fromMaybe [element] $ fmap (enoughFor element) $ producing reactions element

aggregate :: [Element] -> [Element]
aggregate = map coalesce . partitionOn snd
  where coalesce = bimap sum head . unzip

primitive :: Element -> Bool
primitive (_,"ORE") = True
primitive _ = False

parse :: String -> [Reaction]
parse = mapMaybe (parseMaybe parseReaction) . lines

requires :: [Reaction] -> Integer
requires reactions = sum $ filter (>0) $ map fst inputs
  where inputs = fulfill reactions [(1, "FUEL")]

main = do
    contents <- readFile "Day14.txt"
    print $ requires (parse contents)