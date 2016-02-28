module Menu (
    runMenu
) where

import System.IO
import Data.Char
import qualified Game as G
import Screen (clearScreen)

runMenu :: IO ()
runMenu = do
    putStrLn "-----------------------------"
    putStrLn " S L I D I N G   P U Z Z L E "
    putStrLn "-----------------------------"
    putStrLn " (P)lay"
    putStrLn " (Q)uit"
    putStrLn ""
    putStr "Choice: "
    hFlush stdout
    response <- getLine
    processInput $ map toUpper response

processInput :: String -> IO ()
processInput "P" = do
    G.playGame
    runMenu
processInput "Q" = do
    respond "Thanks for playing!"
processInput _   = do
    respond "Invalid choice.  Choose P or Q."
    runMenu

respond message = do
    clearScreen
    putStrLn message
