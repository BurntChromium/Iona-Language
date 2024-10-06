-- Source code transformations
module Source where

import Text.Megaparsec (SourcePos, sourceColumn, sourceLine, unPos)

-- | Position within the source code
data Cursor = Cursor
  { line :: Int,
    column :: Int
  }
  deriving (Show, Eq)

-- | Update position of the cursor as we iterate
updateCursor :: Cursor -> Char -> Cursor
updateCursor cursor c
  | c == '\n' = cursor {line = line cursor + 1, column = 1}
  | otherwise = cursor {column = column cursor + 1}

initCursor :: Cursor
initCursor = Cursor {line = 1, column = 1}

-- Convert Megaparsec SourcePos to custom Cursor
parsecLocToCursor :: SourcePos -> Cursor
parsecLocToCursor pos = Cursor (unPos (sourceLine pos)) (unPos (sourceColumn pos))
