module Lex (Symbol, Token) where

-- A symbol is the what most would consider a Token normally, but we "enrich" the token with additional data
data Symbol = Import | Path | With | ModuleItem deriving (Show)

-- A token has its original string, its symbol, and where it is in the text
data Token = Token {
    str :: String,
    sym :: Symbol,
    line :: Int,
    offset :: Int
}