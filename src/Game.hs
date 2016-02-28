module Game (
    playGame,
    resetScreen
) where

import Board (Board, Dimensions(..), SlideError(..), newBoard, slideNumber, isStartingLayout, Width, Height)
import System.IO
import System.Console.ANSI
import Data.Char
import Data.List
import qualified Text.Read

-- | Main "game loop".
playGame :: IO ()
playGame = do
    putStrLn "Let's choose a board size."
    width  <- getPositiveNumber "How many spots wide should the board be (Q to abort)? "
    case width of
      Nothing -> abortGame
      Just w -> do
        height <- getPositiveNumber "How many spots high should the board be (Q to abort)? "
        case height of
          Nothing -> abortGame
          Just h -> do
            let dimens = Dimensions w h
            if isSaneBoard dimens
              then do
                resetScreen
                board <- newBoard dimens
                playBoard board
              else do
                putStrLn "These dimensions make for an uninteresting game.  Try again.\n"
                playGame

isSaneBoard :: Dimensions -> Bool
isSaneBoard (Dimensions width height) = width * height >= 4

-- Easy state transition for aborting out of the game.
abortGame :: IO ()
abortGame = putStrLn "Aborting game.\n"

-- Fifth success state is to show the board and prompt the player for a move.
playBoard :: Board -> IO ()
playBoard board = do
    putStrLn (show board)
    number <- getPositiveNumber "Enter the number to move (Q to quit): "
    case number of
      Just n  -> trySlidingNumber n board
      Nothing -> abortGame

-- Sixth success state: validate the move.
trySlidingNumber :: Int -> Board -> IO ()
trySlidingNumber number board =
    case slideNumber number board of
        Left InvalidNumber            -> reportError board "That number is not on the board."
        Left NumberNotAdjacentToBlank -> reportError board "That number is not next to the blank spot."
        Right board                   -> checkVictory board

-- Report error messages at the top of a cleared screen.
reportError :: Board -> String -> IO ()
reportError board error = do
    resetScreen
    putStrLn error
    putStrLn ""
    playBoard board

-- Have we won yet?
checkVictory :: Board -> IO ()
checkVictory board
    | isStartingLayout board = winGame board
    | otherwise              = do
        resetScreen
        playBoard board

-- We won!  Throw confetti, and return to the main menu.
winGame :: Board -> IO ()
winGame board = do
    resetScreen
    putStrLn (show board)
    putStrLn "You won the game!"
    putStrLn ""

resetScreen :: IO ()
resetScreen = do
  clearScreen
  setCursorPosition 0 0

data TryRead a = Read a | TryAgain | Quit

-- Prompt for and accept a positive number or Q to quit.
getPositiveNumber :: String -> IO (Maybe Int)
getPositiveNumber prompt = queryUser

    where parseNumber :: String -> TryRead Int
          parseNumber "q"            = Quit
          parseNumber "Q"            = Quit
          parseNumber response =
            case Text.Read.readMaybe response of
              Just n | n > 0 -> Read n
              _              -> TryAgain

          queryUser = do
              putStr prompt
              hFlush stdout
              response <- getLine
              case parseNumber response of
                Read n   -> return (Just n)
                TryAgain -> do
                  putStrLn "You must enter a positive number.\n"
                  queryUser
                Quit     -> return Nothing
