module Errors
  ( ProblemClass (..),
    Problem (..),
    quickProblem,
    parserProblem,
  )
where

import Source qualified
import Text.Megaparsec (SourcePos)

-- | How severe is the issue?
data ProblemClass = Error | Warning | Lint deriving (Show, Eq)

-- | A `Problem` should provide enough detail to the programmer to solve it
data Problem = Problem
  { cls :: ProblemClass,
    cursor :: Source.Cursor,
    message :: String,
    hint :: Maybe String,
    ref :: Maybe Source.Cursor -- if we need to link to another point in the code
  }
  deriving (Show, Eq)

quickProblem :: ProblemClass -> Source.Cursor -> String -> Problem
quickProblem cls crs msg = Problem cls crs msg Nothing Nothing

-- Create a Problem from a parse error
parserProblem :: SourcePos -> String -> Problem
parserProblem pos = quickProblem Error (Source.parsecLocToCursor pos)
