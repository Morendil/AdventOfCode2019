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

targetCriteria :: Group -> Group -> Group -> Ordering
targetCriteria attacker = mappend (comparing (dealDamage attacker)) (comparing effectivePower)

targetOrder :: Army -> Army -> [Int]
targetOrder atk def = map last $ map (\attacker -> sortOrder (targetCriteria attacker) (groups atk)) (groups def)

dealDamage :: Group -> Group -> Integer
dealDamage atk def = if immune then 0 else if weak then 2 * dmg else dmg
  where dmg = effectivePower atk
        what = attackType $ attack atk
        immune = elem what $ immunities def
        weak = elem what $ weaknesses def

damageMatrix :: (Army, Army) -> ([[Integer]],[[Integer]])
damageMatrix (army1, army2) = (potential army1 army2, potential army2 army1)
  where potential a1 a2 = cross (groups a1) (groups a2)
        cross g1 g2 = [[dealDamage i1 i2 | i2 <- g2] | i1 <- g1]

attackGroup :: Group -> Group -> Group
attackGroup attacker defender = defender {units = max 0 $ units defender - (damageSuffered `div` hitPoints defender)}
  where damageSuffered = dealDamage attacker defender

type Selector = (Army, Army) -> (Army, Army)
fightOrder :: (Army, Army) -> [(Selector, Int, Int, Integer)]
fightOrder (a1, a2) = sortOn (Down . getRank) $ (go swap a1 a2) ++ (go id a2 a1)
  where go sel atk def = [(sel, index, target, rank index atk) | (index, target) <- indexed $ targetOrder def atk]
        rank n army = (initiative . attack) ((groups army) !! n)
        getRank (_,_,_,rank) = rank

fight :: (Army, Army) -> (Army, Army)
fight war = cleanup $ foldl doFight war (fightOrder war)

cleanup :: (Army, Army) -> (Army, Army)
cleanup (a, b) = (doCleanup a, doCleanup b)
   where doCleanup army = army { groups = filter (\group -> units group > 0) (groups army)}

doFight :: (Army, Army) -> (Selector, Int, Int, Integer) -> (Army, Army)
doFight pair (sel, index, target, _) = sel (defender { groups = replace target result (groups defender)}, attacker)
  where defender = fst $ sel pair
        attacker = snd $ sel pair
        result = attackGroup (groups attacker !! index) ((groups defender) !! target)

repr (s,x,y,z) = (x,y,z)
summary (a1,a2) = map units $ (groups a1 ++ groups a2)

main = do
    contents <- readFile "GoldenOldie2018Day24.txt"
    print $ parse contents