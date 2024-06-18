module Errors (
    ProblemClass(..),
    Problem(..),
    Cursor(..),
    updateCursor,
    initCursor
) where

import qualified Terminal

-- Position within the source code
data Cursor = Cursor {
    line :: Int,
    column :: Int
} deriving (Show, Eq)

-- Update position of the cursor as we iterate
updateCursor :: Cursor -> Char -> Cursor
updateCursor cursor c 
    | c == '\n' = cursor { line = line cursor + 1, column = 1}
    | otherwise = cursor { column = column cursor + 1}

initCursor :: Cursor
initCursor = Cursor { line = 1, column = 1}

-- How severe is the issue?
data ProblemClass = Error | Warning | Lint deriving (Show, Eq)

-- Terminal color coding of issues
problemColor :: ProblemClass -> Terminal.TermColor
problemColor Error = Terminal.Red
problemColor Warning = Terminal.Yellow
problemColor Lint = Terminal.Blue

-- A `Problem` should provide enough detail to the programmer to solve it
data Problem = Problem {
    cls :: ProblemClass,
    cursor :: Cursor,
    message :: String,
    hint :: Maybe String
} deriving (Show, Eq)