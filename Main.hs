module Main where

import System.Console.ANSI

-- Set colors and write some text in those colors.
main :: IO ()
main = do
    saveCursor
    setCursorPosition 0 0
    clearScreen
    setSGR [SetColor Foreground Vivid Red]
    setSGR [SetColor Background Vivid Blue]
    putStr "Red-On-Blue"
    setSGR [Reset]  -- Reset to default colour scheme
    putStrLn ""
    putStrLn "Default colors."
    restoreCursor