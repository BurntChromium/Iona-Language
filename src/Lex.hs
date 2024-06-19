{-# LANGUAGE DisambiguateRecordFields #-}

module Lex where

import Data.Char

import Errors (Problem(..), ProblemClass(..), quickProblem)
import Source (Cursor(..), updateCursor, initCursor)

-- A symbol is the what most would consider a Token normally, but we "enrich" the token with additional data
data Symbol = Comment | NewLine | Identifier | Import | With | EndStmt deriving (Show, Eq)

-- A token has its original string, its symbol, and where it is in the text
data Token = Token {
    str :: String,
    sym :: Symbol,
    pos :: Cursor
} deriving (Show, Eq)

-- We return both a list of tokens and of problems -> not all problems are fatal and we also want to recover from errors
data LexerState = LexerState {
    csr :: Cursor,
    tokens :: [Token],
    errors :: [Problem]
} deriving (Show, Eq)

initLexerState :: LexerState
initLexerState = LexerState { csr = initCursor, tokens = [], errors = []}

-- Helper function to create debug info
debugInfo :: LexerState -> String -> String
debugInfo state remainingInput =
    "Current state: " ++ show state ++ ", Remaining input: " ++ take 10 remainingInput ++ "..."

-- Run lexing by pattern matching
lexer :: LexerState -> String -> LexerState
lexer state [] = state -- Empty string => empty token list
lexer state (c:cs) 
    | c == '#' = lexer (addTokenToLexer [c] Comment (csr state) state) cs
    | c == '\n' = lexer (addTokenToLexer [c] NewLine (csr state) state) cs
    | isSpace c = lexer (advanceCursor state c) cs
    | c == ';' = lexer (addTokenToLexer [c] EndStmt (csr state) state) cs
    | isAlphaNum c = let (item, rest) = span isAlphaNum (c : cs) in lexer (addTokenToLexer item Identifier (csr state) state) rest
    | otherwise = do 
        let newProblemList = errors state ++ [quickProblem Error (csr state) ("unrecognized symbol: " ++ [c])]
        lexer (state { errors = newProblemList}) cs

-- Utility to update the cursor in the lexer's state
advanceCursor :: LexerState -> Char -> LexerState
advanceCursor state c = state { csr = updateCursor (csr state) c }

matchKeywords :: String -> Symbol
matchKeywords "import" = Import
matchKeywords "with" = With
matchKeywords _ = Identifier

addTokenToLexer :: String -> Symbol -> Cursor -> LexerState -> LexerState
addTokenToLexer st sy cr old = do
    let newToken = if length st > 1 then Token { str = st, sym = matchKeywords st, pos = cr} else Token { str = st, sym = sy, pos = cr}
    -- Pull out data for easier addressing
    let oldTokens = tokens old
    -- Use advanceCursor to update lexer state
    let upState = foldl advanceCursor old st
    -- Append tokens
    upState { tokens = oldTokens ++ [newToken] }

-- Save for later debugging
-- Run lexing by pattern matching
-- lexer :: LexerState -> String -> LexerState
-- lexer state [] = state -- Empty string => empty token list
-- lexer state (c:cs)
--     | isSpace c =
--         let newState = advanceCursor state c
--         in trace (debugInfo newState cs) (lexer newState cs)
--     | c == ';' =
--         let newState = addTokenToLexer [c] EndStmt (csr state) state
--         in trace (debugInfo newState cs) (lexer newState cs)
--     | isAlphaNum c = do
--         let (item, rest) = span isAlphaNum (c : cs) -- take while we have alpha-numerics
--         let newState = addTokenToLexer item Identifier (csr state) state
--         trace (debugInfo newState cs) (lexer newState rest)
--     | otherwise =
--         let newProblemList = errors state ++ [quickProblem Error (csr state) ("unrecognized symbol: " ++ [c])]
--             newState = advanceCursor (state { errors = newProblemList }) '-'
--         in trace (debugInfo newState cs) (lexer newState cs)