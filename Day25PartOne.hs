module Main where

import IntCode
import Data.Char
import Control.Monad.Extra
import System.IO
import Data.List.Tools

fromAscii = map (chr.fromInteger)
toAscii = map (toInteger.ord)

main = do
    contents <- readFile "Day25.txt"
    let state = initialize (parse contents) []
    (flip loopM) state $ (\seed -> do
        let state' = last $ takeWhile (\s -> lastIn s == Nothing && (not.halt) s) $ iterate step seed
        putStrLn (fromAscii $ output state')
        putStr $ (show $ maxM state')++"/"++(show $ codeLength state')++"> "
        hFlush stdout
        command <- getLine
        putStrLn ("ok ("++command++")...")
        let state1 = state' { input = (toAscii (command++"\n")), output=[]}
        let state2 = last $ takeUntil (\s -> lastIn s == Just 10) $ iterate step state1
        let state3 = state2 { input=[10], lastIn=Nothing }
        return $ Left state3)
