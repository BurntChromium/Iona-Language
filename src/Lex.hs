{-# LANGUAGE DisambiguateRecordFields #-}

-- Note to self, debug trace is helpful for diagnosing infinite loops

module Lex where

import Data.Char
import Debug.Trace

import Errors (Problem(..), ProblemClass(..), quickProblem)
import Source (Cursor(..), updateCursor, initCursor)

-- A symbol is the what most would consider a Token normally, but we "enrich" the token with additional data
data Symbol = Comment | NewLine | Identifier | Import | With | Let | Mut | FieldSep | Struct | Enum | Equals | Bar | Is | Derives | Alias | FnDeclare | FnObject | Partial | For | In | NumberLiteral | EndStmt | Colon deriving (Show, Eq)

-- Given a string keyword, return the correct symbol
matchKeywords :: String -> Symbol
matchKeywords "import" = Import
matchKeywords "with" = With
matchKeywords "let" = Let
matchKeywords "mut" = Mut
matchKeywords "struct" = Struct
matchKeywords "enum" = Enum
matchKeywords "is" = Is
matchKeywords "derives" = Derives
matchKeywords "alias" = Alias
matchKeywords "fn" = FnDeclare
matchKeywords "Fn" = FnObject
matchKeywords "for" = For
matchKeywords "in" = In
matchKeywords _ = Identifier

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

-- Alpha numerics + underscores
isIdentifierChar :: Char -> Bool
isIdentifierChar c 
    | isAlphaNum c = True
    | c == '_' = True
    | c == '.' = True
    | otherwise = False

-- Numbers and dots
isNumberLiteral :: Char -> Bool
isNumberLiteral c
    | isDigit c = True
    | c == '.' = True
    | otherwise = False

-- Run lexing by pattern matching on each character
lexer :: LexerState -> String -> LexerState
lexer state [] = do
    let _ = trace "should terminate" False
    state -- Empty string => empty token list
lexer state (c:cs) 
   -- Single character symbols
    | c == '\n' = lexer (addTokenToLexer [c] NewLine (csr state) state) cs
    | isSpace c = lexer (advanceCursor state c) cs
    | c == '#' = lexer (addTokenToLexer [c] Comment (csr state) state) cs
    | c == ';' = lexer (addTokenToLexer [c] EndStmt (csr state) state) cs
    | c == '=' = lexer (addTokenToLexer [c] Equals (csr state) state) cs
    -- If colon, check if it's "::" or just ":"
    | c == ':' = if head cs == ':' then
        lexer (addTokenToLexer [c, head cs] FieldSep (csr state) state) (tail cs)
    else
        lexer (addTokenToLexer [c] Colon (csr state) state) cs
    -- Get number literals
    | isNumberLiteral c = let (item, rest) = span isNumberLiteral (c : cs) in lexer (addTokenToLexer item Identifier (csr state) state) rest
    -- Map alphanumerics -> keywords
    | isIdentifierChar c = let (item, rest) = span isIdentifierChar (c : cs) in lexer (addTokenToLexer item NumberLiteral (csr state) state) rest
    | otherwise = do 
        let newProblemList = errors state ++ [quickProblem Error (csr state) ("unrecognized symbol: " ++ [c])]
        lexer (state { errors = newProblemList}) cs

-- Utility to update the cursor in the lexer's state
advanceCursor :: LexerState -> Char -> LexerState
advanceCursor state c = state { csr = updateCursor (csr state) c }

-- Helper method to add a token to the lexer state
addTokenToLexer :: String -> Symbol -> Cursor -> LexerState -> LexerState
addTokenToLexer st sy cr old = do
    -- For full words overwrite the symbol, but for single characters take what was given to us
    let newToken = if length st > 1 then Token { str = st, sym = matchKeywords st, pos = cr} else Token { str = st, sym = sy, pos = cr}
    -- Pull out data for easier addressing
    let oldTokens = tokens old
    -- Use advanceCursor to update lexer state
    let upState = foldl advanceCursor old st
    -- Append tokens
    upState { tokens = oldTokens ++ [newToken] }