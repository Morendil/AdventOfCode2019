module Day03 where

import Common
import Text.ParserCombinators.ReadP
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (sortOn)

data Instruction = R Integer | U Integer | L Integer | D Integer
    deriving (Eq, Show)

type SegmentD = (Integer, Integer, Integer)
data Segment = V SegmentD | H SegmentD
    deriving (Eq, Show)

type Position = (Integer, Integer)
type State = (Position, [Segment])

instruction :: ReadP Instruction
instruction = choice $ map instructionFrom [('R',R),('U', U),('L',L),('D',D)]

instructionFrom :: (Char, Integer -> Instruction) -> ReadP Instruction
instructionFrom (c,f) = do
    char c
    f <$> number

instructions :: ReadP [Instruction]
instructions = sepBy1 instruction comma

parse :: String -> [Instruction]
parse = fromMaybe [] . parseMaybe instructions

segments :: [Instruction] -> [Segment]
segments = reverse . snd . foldl advance ((0,0),[])

advance :: State -> Instruction -> State
advance ((x,y), segments) (R r) = ((x+r, y), H (y, x, x+r) : segments)
advance ((x,y), segments) (L l) = ((x-l, y), H (y, x, x-l) : segments)
advance ((x,y), segments) (U u) = ((x, y+u), V (x, y, y+u) : segments)
advance ((x,y), segments) (D d) = ((x, y-d), V (x, y, y-d) : segments)

crossings :: [Segment] -> [Segment] -> [Position]
crossings l1 l2 = catMaybes [cross s1 s2 | s1 <- l1, s2 <- l2]

cross :: Segment -> Segment -> Maybe Position
cross (V _) (V _) = Nothing
cross (H _) (H _) = Nothing
cross (V (x,y1,y2)) (H (y,x1,x2)) = if (within x x1 x2) && (within y y1 y2) then Just (x,y) else Nothing
cross h v = cross v h

within :: Integer -> Integer -> Integer -> Bool
within target r1 r2 = low <= target && target <= high
    where low = min r1 r2
          high = max r1 r2

crossingsFrom s1 s2 = tail $ sortOn manhattan $ crossings (process s1) (process s2)
    where process = segments . parse
          manhattan (x,y) = (abs x)+(abs y)
