{-# LANGUAGE DisambiguateRecordFields #-}

module Lex where

import Data.Char
import Debug.Trace (trace)

import Errors (Problem(..), ProblemClass(..), quickProblem)
import Source (Cursor(..), updateCursor, initCursor)

-- A symbol is the what most would consider a Token normally, but we "enrich" the token with additional data
data Symbol = Comment | Identifier | Import | Path | With | EndStmt deriving (Show, Eq)

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

-- Run lexing by pattern matching
lexer :: LexerState -> String -> LexerState
lexer state [] = state -- Empty string => empty token list
lexer state (c:cs) 
    | isSpace c = 
        let newState = advanceCursor c state
        in trace (debugInfo newState cs) (lexer newState cs)
    | c == ';' = 
        let newState = addTokenToLexer [c] EndStmt (csr state) state
        in trace (debugInfo newState cs) (lexer newState cs)
    | otherwise = 
        let newProblemList = quickProblem Error (csr state) ("unrecognized symbol: " ++ [c]) : errors state
            newState = state { errors = newProblemList }
        in trace (debugInfo newState cs) (lexer newState cs)

-- Helper function to create debug info
debugInfo :: LexerState -> String -> String
debugInfo state remainingInput = 
    "Current state: " ++ show state ++ ", Remaining input: " ++ take 10 remainingInput ++ "..."

-- Run lexing by pattern matching
-- lexer :: LexerState -> String -> LexerState
-- lexer state [] = state -- Empty string => empty token list
-- lexer state (c:cs) 
--     | isSpace c = lexer (advanceCursor c state) cs
--     | c == ';' = lexer (addTokenToLexer [c] EndStmt (csr state) state) cs
--     | otherwise = do 
--         let newProblemList = quickProblem Error (csr state) ("unrecognized symbol: " ++ [c]) : errors state
--         lexer (state { errors = newProblemList}) cs

-- Utility to update the cursor in the lexer's state
advanceCursor :: Char -> LexerState -> LexerState
advanceCursor c state = state { csr = updateCursor (csr state) c } 
    
addTokenToLexer :: String -> Symbol -> Cursor -> LexerState -> LexerState
addTokenToLexer st sy cr old = do 
    -- Construct new token
    let newToken = Token { str = st, sym = sy, pos = cr} 
    -- Pull out data for easier addressing
    let oldTokens = tokens old
    -- Use advanceCursor to update lexer state
    let upState = if length st == 1 then advanceCursor (head st) old else  advanceCursor '-' old
    -- Append tokens
    upState { tokens = oldTokens ++ [newToken] }