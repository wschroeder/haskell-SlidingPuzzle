module Screen (
  clearScreen
) where

import qualified System.Console.ANSI as ANSI

-- | Clears the screen and places the cursor at the top-left corner.
clearScreen :: IO ()
clearScreen = do
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0

