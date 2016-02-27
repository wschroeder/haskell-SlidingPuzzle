module Main where

import System.Console.ANSI
import Menu

main :: IO ()
main = do
    clearScreen
    setCursorPosition 0 0
    runMenu

