module GoldenOldie2018Day24PartTwo where

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
  where (army1, army2) = toPair groups
        cross g1 g2 = [[dealDamage i1 i2 | i2 <- g2] | i1 <- g1]

toPair :: [Group] -> ([Group], [Group])
toPair groups = partition (\group -> (army group) == (army $ head groups)) groups

attackGroup :: Group -> Group -> Group
attackGroup attacker defender = defender {units = max 0 $ units defender - (damageSuffered `div` hitPoints defender)}
  where damageSuffered = dealDamage attacker defender

war :: [Group] -> Integer
war = sum . map units . untilStable . iterate fight

fight :: [Group] -> [Group]
fight groups = cleanup $ foldl (doFight order) groups $ byInitiative
  where byInitiative = sortOn (Down . initiative . attack . snd) (indexed groups)
        order = targetSelection groups

doFight :: [(Int, Int)] -> [Group] -> (Int, Group) -> [Group]
doFight order groups (index, _) = if isJust defender
    then replace (fromJust defender) result groups
    else groups
  where defender = lookup index order
        result = attackGroup (groups !! index) (groups !! fromJust defender)

cleanup :: [Group] -> [Group]
cleanup = filter (\group -> units group > 0)

targetSelection :: [Group] -> [(Int, Int)]
targetSelection groups = foldl (pickTarget groups) [] $ sortBy selectionOrder (indexed groups)
  where selectionOrder = mappend (comparing (Down . effectivePower . snd)) (comparing (Down . initiative . attack . snd))

pickTarget :: [Group] -> [(Int, Int)] -> (Int, Group) -> [(Int, Int)]
pickTarget groups picks (atkIndex, attacker) = if null candidates then picks else picks ++ [choose $ head candidates]
  where indexedTargets = indexed groups
        targetOrder = mconcat $ map comparing [(Down . dealDamage attacker . snd), (Down . effectivePower . snd), (Down . initiative . attack . snd)]
        picked target = isJust $ find (\(_,index) -> index == target) picks
        candidates = filter (\(index, group) -> (not.picked) index && opposing group && (dealDamage attacker group) > 0) (sortBy targetOrder indexedTargets)
        opposing target = army target /= army attacker
        choose (index, target) = (atkIndex, index)

display armies = unlines $ map show $ zip (map army armies) (map units armies)
summary armies = doBoth $ toPair armies
  where doBoth (a, b) = (doOne a, doOne b)
        doOne groups = if null groups then 0 else (sum $ map units $ groups)

delta :: ((Integer, Integer),(Integer, Integer)) -> (Integer, Integer)
delta ((a,b),(c, d)) = (c-a,d-b)

boost :: String -> Integer -> [Group] -> [Group]
boost name amount = map (\g -> if army g == name then g {attack = (attack g) {damage = (damage.attack) g + amount}} else g)

winner :: [Group] -> String
winner = army . head . untilStable . iterate fight

main = do
    contents <- readFile "GoldenOldie2018Day24.txt"
    -- putStrLn $ unlines $ map (show . summary) $ take 1600 $ iterate fight $ boost "Immune System" 30 $ parse contents
    print $ summary $ untilStable $ iterate fight $ boost "Immune System" 31 $ parse contents