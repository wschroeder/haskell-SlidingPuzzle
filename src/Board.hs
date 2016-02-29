module Board (
  newBoard,
  slideNumber,
  isStartingLayout,
  Dimensions(..),
  Board(..),
  SlideError(..),
  Width,
  Height
) where

import qualified Data.List as L
import qualified System.Random as R
import qualified Text.Printf as Text

type Width            = Int
type Height           = Int
data Dimensions       = Dimensions Width Height deriving (Show, Eq)
data Position         = Position Int Int deriving (Show, Eq)
type PositionValue    = Maybe Int
type BoardLayout      = [PositionValue]
data Board            = Board Dimensions BoardLayout deriving (Eq)
data NumberPosition a = NumberPosition a | NoNumberPosition
data SlideError       = InvalidNumber | NumberNotAdjacentToBlank deriving (Show, Eq)

instance Show Board where
  show b@(Board (Dimensions width height) _) = unlines (map showRow [0..height-1])
    where showRow y                                = concatMap (\x -> showPosition b (Position x y)) [0..width-1]
          showPosition b p                         = Text.printf "%2s " (translateValue (numberAt b p))
          translateValue (NumberPosition Nothing)  = "__"
          translateValue (NumberPosition (Just x)) = show x

-- Given a board and a position, what is there?  The position could be outside
-- the bounds of the board (NoNumberPosition), the position could hold
-- Nothing, and the position could Just hold a number.
numberAt :: Board -> Position -> NumberPosition PositionValue
numberAt (Board (Dimensions width height) layout) (Position x y)
  | x >= width  = NoNumberPosition
  | y >= height = NoNumberPosition
  | x < 0       = NoNumberPosition
  | y < 0       = NoNumberPosition
  | otherwise   = NumberPosition (layout !! (y * width + x))

-- Slide numbers around the board until the layout is pretty far removed from
-- the original board layout.
scrambleBoard :: (R.RandomGen g) => Board -> Board -> g -> Board
scrambleBoard board@(Board (Dimensions width height) _) startingBoard initialGen =
  scrambleBoardLoop board initialGen ((width * height)^2)
  where scrambleBoardLoop board gen 0          = board
        scrambleBoardLoop board gen iterations =
          let (moveIndex, newGen)   = R.randomR (0, 3) gen
              (moveX, moveY)        = [(-1, 0), (1, 0), (0, -1), (0, 1)] !! moveIndex
              (Just (Position x y)) = positionOf Nothing board
              newX                  = x + moveX
              newY                  = y + moveY
           in trySlideNumber board (numberAt board (Position newX newY)) newGen iterations
        trySlideNumber board NoNumberPosition gen iterations               =
          scrambleBoardLoop board gen iterations
        trySlideNumber board (NumberPosition (Just number)) gen iterations =
          let (Right newBoard) = slideNumber number board
           in verifyBoard newBoard gen iterations
        verifyBoard board gen iterations
          | board == startingBoard = scrambleBoardLoop board gen iterations
          | otherwise              = scrambleBoardLoop board gen (iterations - 1)

-- | Randomly generate a new board, given dimensions.
newBoard :: Dimensions -> IO Board
newBoard d = do
  newGen <- R.newStdGen
  return (scrambleBoard startingBoard startingBoard newGen)
  where startingBoard = calculateStartingBoard (Board d [])

-- Find where a number or Nothing is on the board.
positionOf :: PositionValue -> Board -> Maybe Position
positionOf item (Board (Dimensions width _) layout) =
  case L.elemIndex item layout of
      Just index -> Just (Position (index `rem` width) (index `div` width))
      Nothing    -> Nothing

-- | Exchange the blank position with the given number if it is adjacent.
slideNumber :: Int -> Board -> Either SlideError Board
slideNumber number board =
  validateAndSlideNumber maybeBlankPosition maybeNumberPosition
  where maybeBlankPosition                          = positionOf Nothing board
        maybeNumberPosition                         = positionOf (Just number) board
        validateAndSlideNumber _ Nothing            = Left InvalidNumber
        validateAndSlideNumber (Just blankPosition) (Just numberPosition)
          | nextTo blankPosition numberPosition = Right (swapNumberAndBlank number board)
          | otherwise                           = Left NumberNotAdjacentToBlank
        nextTo (Position bx by) (Position nx ny)    = above || below || leftOf || rightOf
          where above   = (nx == bx) && (ny == by - 1)
                below   = (nx == bx) && (ny == by + 1)
                leftOf  = (nx == bx - 1) && (ny == by)
                rightOf = (nx == bx + 1) && (ny == by)
        swapNumberAndBlank number (Board d layout)  = Board d (map swappy layout)
          where swappy (Just n) | n == number = Nothing
                                | otherwise   = (Just n)
                swappy Nothing                = (Just number)

-- | Is the given board in its unscrambled, initial layout?
isStartingLayout :: Board -> Bool
isStartingLayout board = board == calculateStartingBoard board

-- Return what a given board would look like in its unscrambled, initial
-- layout.
calculateStartingBoard :: Board -> Board
calculateStartingBoard (Board d _) = Board d (generateOrderedLayout d)
  where generateOrderedLayout (Dimensions width height) =
          let lastNumber = width * height - 1
           in (map Just [1 .. lastNumber]) ++ [Nothing]

