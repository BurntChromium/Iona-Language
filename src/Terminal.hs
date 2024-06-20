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
    let selectedLines = sort (deduplicate [max 0 (l - 1), l, min (l + 1) (length allLines - 1)])
    let context = fmap (\i -> (i + 1, allLines !! i)) selectedLines -- Add line numbers
    -- Inject and dynamically format line numbers into context (weird padding, IDK came from an LLM and HLS complained when I un-padded it)
    let formattedContext = fmap (\(n, lineStr) -> let lineNumStr = show n
                                                      padding = replicate (4 - length lineNumStr) ' '
                                                  in padding ++ lineNumStr ++ " | " ++ lineStr) context
    let hintText = maybe "" ("\nhint: " ++) (hint err)
    -- Long string concatenation
    printColor color (show (cls err)) ++ ": " ++ message err ++ " at line " ++ show (l + 1) ++ ", column " ++ show c ++ "\n" ++ intercalate "\n" formattedContext ++ hintText ++ "\n"
