module Lex where

import Control.Monad.State
import Data.Char

import Errors (Cursor(..), updateCursor, Problem(..), ProblemClass(..))

-- A symbol is the what most would consider a Token normally, but we "enrich" the token with additional data
data Symbol = Comment | Identifier | Import | Path | With deriving (Show, Eq)

-- A token has its original string, its symbol, and where it is in the text
data Token = Token {
    str :: String,
    sym :: Symbol,
    line :: Int,
    offset :: Int
}

-- We return both a list of tokens and of problems -> not all problems are fatal and we also want to recover from errors
type LexerResult = ([Token], [Problem])

-- A lexer passes around a stateful Cursor as it processes tokens
type Lexer = State Cursor LexerResult

-- Pattern match
lexer :: String -> Lexer
lexer [] = return ([], []) -- Empty string => empty token list
lexer (c:cs)
    | isSpace c = do -- skip whitespace
        updateState c
        lexer cs
    | otherwise = do -- handle unsupported characters
        cursor <- get
        let problem = Problem Error cursor ("unexpected character: " ++ [c]) (Just "character is probably recognized")
        updateState c
        (tokens, problems) <- lexer cs
        return (tokens, problem : problems)

-- Utility to update the state cursor
updateState :: Char -> State Cursor ()
updateState c = modify (`updateCursor` c)