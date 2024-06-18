module Terminal (
    TermColor(..), 
    printColor,
    printError
) where

import Data.List (sort, intercalate)
import Data.Set (fromList, toList)

import Errors (Problem(..), ProblemClass(..))
import Source (Cursor(..))

-- Supported terminal colors
data TermColor = Red | Blue | Green | Yellow

-- Emit ANSI code for each color
termColorToStr :: TermColor -> String
termColorToStr Red = "\ESC[31m"
termColorToStr Blue = "\ESC[34m"
termColorToStr Green = "\ESC[32m"
termColorToStr Yellow = "\ESC[33m"

-- Terminal color coding of issues
problemColor :: ProblemClass -> Terminal.TermColor
problemColor Error = Terminal.Red
problemColor Warning = Terminal.Yellow
problemColor Lint = Terminal.Blue

-- Wrap a given string in the ANSI codes (and reset afterwards)
printColor :: TermColor -> String -> String
printColor color str = termColorToStr color ++ str ++ "\ESC[0m"

deduplicate :: Ord a => [a] -> [a]
deduplicate = toList . fromList

-- Nicely format an error into a string (messy imperative style, oh well)
printError :: String -> Problem -> String
printError source err = do
    let color = problemColor (cls err)
    let allLines = lines source
    let l = line (cursor err) - 1 -- our cursor is 1 indexed, while allLines is 0 indexed
    let c = column (cursor err)
    let selectedLines = sort (deduplicate [max 0 (l - 1), l, min (l + 1) (length selectedLines)])
    let context = fmap (allLines !!) selectedLines
    -- Long string concatenation
    printColor color (show (cls err)) ++ show (message err) ++ "at line " ++ show l ++ ", column " ++ show c ++ intercalate "\n" context ++ "\nhint: " ++ show (hint err)

    