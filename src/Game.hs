module Game (
    playGame
) where

import Board (Board, Dimensions(..), SlideError(..), newBoard, slideNumber, isStartingLayout)
import System.IO
import System.Console.ANSI
import Data.Char
import Data.List

-- | Main "game loop".
playGame :: IO ()
playGame = askAboutDimensions

-- First success state for playing a game: choose the width for the board size.
askAboutDimensions :: IO ()
askAboutDimensions = do
    putStrLn "Let's choose a board size."
    width  <- getPositiveNumber "How many spots wide should the board be (Q to abort)? "
    askAboutHeight width

-- Second success state: choose the height.
askAboutHeight :: Maybe Int -> IO ()
askAboutHeight Nothing = abortGame
askAboutHeight width   = do
    height <- getPositiveNumber "How many spots high should the board be (Q to abort)? "
    verifySaneBoard width height

-- Third success state is to validate width and height.
verifySaneBoard :: Maybe Int -> Maybe Int -> IO ()
verifySaneBoard Nothing _ = abortGame
verifySaneBoard _ Nothing = abortGame
verifySaneBoard (Just width) (Just height)
    | width * height >= 4 = constructBoard width height
    | otherwise           = do
        putStrLn "These dimensions make for an uninteresting game.  Try again."
        putStrLn ""
        askAboutDimensions

-- Fourth success state is to construct a board based on the given dimensions.
constructBoard :: Int -> Int -> IO ()
constructBoard width height = do
    putStrLn ""
    board <- newBoard $ Dimensions width height
    clearScreen
    setCursorPosition 0 0
    playBoard board

-- Easy state transition for aborting out of the game.
abortGame :: IO ()
abortGame = do
    putStrLn "Aborting game."
    putStrLn ""

-- Fifth success state is to show the board and prompt the player for a move.
playBoard :: Board -> IO ()
playBoard board = do
    putStrLn $ show board
    number <- getPositiveNumber "Enter the number to move (Q to quit): "
    trySlidingNumber number board

-- Sixth success state: validate the move.
trySlidingNumber :: Maybe Int -> Board -> IO ()
trySlidingNumber Nothing _           = abortGame
trySlidingNumber (Just number) board =
    case slideNumber number board of
        (Left InvalidNumber)            -> reportError board "That number is not on the board."
        (Left NumberNotAdjacentToBlank) -> reportError board "That number is not next to the blank spot."
        (Right board)                   -> checkVictory board

-- Report error messages at the top of a cleared screen.
reportError :: Board -> String -> IO ()
reportError board error = do
    clearScreen
    setCursorPosition 0 0
    putStrLn error
    putStrLn ""
    playBoard board

-- Have we won yet?
checkVictory :: Board -> IO ()
checkVictory board
    | isStartingLayout board = winGame board
    | otherwise              = do
        clearScreen
        setCursorPosition 0 0
        playBoard board

-- We won!  Throw confetti, and return to the main menu.
winGame :: Board -> IO ()
winGame board = do
    clearScreen
    setCursorPosition 0 0
    putStrLn $ show board
    putStrLn "You won the game!"
    putStrLn ""

-- Prompt for and accept a positive number or Q to quit.
getPositiveNumber :: String -> IO (Maybe Int)
getPositiveNumber prompt = do
    queryUser
    where parseNumber ""             = queryUser
          parseNumber "Q"            = return Nothing
          parseNumber response
              | all isDigit response = ensurePositive $ read response
              | otherwise            = reportError
          ensurePositive number
              | number > 0           = return $ Just number
              | otherwise            = reportError
          reportError = do
              putStrLn "You must enter a positive number."
              putStrLn ""
              queryUser
          queryUser = do
              putStr prompt
              hFlush stdout
              response <- getLine
              parseNumber $ map toUpper response

