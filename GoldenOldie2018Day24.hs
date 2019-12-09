module GoldenOldie2018Day24 where

import Common
import Data.Maybe
import Text.ParserCombinators.ReadP

bludgeoning = do string "bludgeoning"; return Bludgeoning
radiation = do string "radiation"; return Radiation
slashing = do string "slashing"; return Slashing
cold = do string "cold"; return Cold
fire = do string "fire"; return Fire
aType = choice [bludgeoning, radiation, slashing, cold, fire]

immunities_ = do
    string "immune to "
    types <- sepBy1 aType (string ", ")
    return (\group -> group {immunities = types})

weaknesses_ = do
    string "weak to "
    types <- sepBy1 aType (string ", ")
    return (\group -> group {weaknesses = types})

options = between (char '(') (string ") ") (sepBy1 (choice [immunities_, weaknesses_]) (string "; "))
        
group = do
    units <- number
    string " units each with "
    hitPoints <- number
    string " hit points "
    optionals <- option [] options
    string "with an attack that does "
    damage <- number
    char ' '
    itsType <- aType
    string " damage at initiative "
    init <- number    
    let attack = Attack damage itsType init
        result = Group "" units hitPoints attack [] []
    return $ foldr id result optionals

anArmy = do
    name <- many1 $ satisfy (/= ':')
    char ':'; char '\n'
    groups <- sepBy1 group (char '\n')
    char '\n'
    return $ map (\group -> group {army = name}) groups

armies = do
    army1 <- anArmy
    char '\n'
    army2 <- anArmy
    return $ army1 ++ army2

data AttackType = Bludgeoning | Radiation | Slashing | Cold | Fire deriving (Eq, Show)
data Attack = Attack {damage :: Integer, attackType :: AttackType, initiative :: Integer} deriving (Eq, Show)

data Group = Group {army :: String, units :: Integer, hitPoints :: Integer, attack :: Attack, weaknesses :: [AttackType], immunities :: [AttackType]} deriving (Eq, Show)

parse :: String -> [Group]
parse = fromJust . parseMaybe armies

main = do
    contents <- readFile "GoldenOldie2018Day24.txt"
    print $ parse contents