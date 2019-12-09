module GoldenOldie2018Day24PartOne where

import Common
import Data.Ord
import Data.Maybe
import Data.List
import Data.Tuple
import GoldenOldie2018Day24 hiding (main)

effectivePower :: Group -> Integer
effectivePower group = (units group) * (damage $ attack group)

effectiveHitpoints :: Group -> Integer
effectiveHitpoints group = (units group) * (hitPoints group)

dealDamage :: Group -> Group -> Integer
dealDamage atk def = if immune then 0 else if weak then 2 * dmg else dmg
  where dmg = effectivePower atk
        what = attackType $ attack atk
        immune = elem what $ immunities def
        weak = elem what $ weaknesses def

damageMatrix :: [Group] -> ([[Integer]],[[Integer]])
damageMatrix groups = (cross army1 army2, cross army2 army1)
  where (army1, army2) = partition (\group -> (army group) == (army $ head groups)) groups
        cross g1 g2 = [[dealDamage i1 i2 | i2 <- g2] | i1 <- g1]

attackGroup :: Group -> Group -> Group
attackGroup attacker defender = defender {units = max 0 $ units defender - (damageSuffered `div` hitPoints defender)}
  where damageSuffered = dealDamage attacker defender

war :: [Group] -> Integer
war = sum . map units . untilStable . iterate fight

fight :: [Group] -> [Group]
fight groups = foldl (doFight order) groups $ byInitiative
  where byInitiative = sortOn (Down . initiative . attack . snd) (indexed groups)
        order = targetSelection groups

doFight :: [(Int, Int)] -> [Group] -> (Int, Group) -> [Group]
doFight order groups (index, _) = replace defender result groups
  where defender = fromJust $ (lookup index order)
        result = attackGroup (groups !! index) (groups !! defender)

cleanup :: [Group] -> [Group]
cleanup = filter (\group -> units group > 0)

targetSelection :: [Group] -> [(Int, Int)]
targetSelection groups = foldl (pickTarget groups) [] $ sortBy selectionOrder (indexed groups)
  where selectionOrder = mappend (comparing (Down . effectivePower . snd)) (comparing (Down . initiative . attack . snd))

pickTarget :: [Group] -> [(Int, Int)] -> (Int, Group) -> [(Int, Int)]
pickTarget groups picks (atkIndex, attacker) = if null candidates then picks else picks ++ [choose $ head candidates]
  where indexedTargets = indexed groups
        targetOrder = mappend (comparing (Down . dealDamage attacker . snd)) (comparing (Down . effectivePower . snd))
        picked target = isJust $ find (\(_,index) -> index == target) picks
        candidates = filter (\(index, group) -> (not.picked) index && opposing group) (sortBy targetOrder indexedTargets)
        opposing target = army target /= army attacker
        choose (index, target) = (atkIndex, index)

main = do
    contents <- readFile "GoldenOldie2018Day24.txt"
    print $ war $ parse contents