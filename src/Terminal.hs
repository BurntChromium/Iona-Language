module Terminal (
    TermColor(..), 
    printColor,
) where

-- Supported terminal colors
data TermColor = Red | Blue | Green | Yellow

-- Emit ANSI code for each color
termColorToStr :: TermColor -> String
termColorToStr Red = "\ESC[31m"
termColorToStr Blue = "\ESC[34m"
termColorToStr Green = "\ESC[32m"
termColorToStr Yellow = "\ESC[33m"

-- Wrap a given string in the ANSI codes (and reset afterwards)
printColor :: TermColor -> String -> String
printColor color str = termColorToStr color ++ str ++ "\ESC[0m"
