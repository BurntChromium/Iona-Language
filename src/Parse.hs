{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Lexer

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many alphaNumChar

reservedWords :: [String]
reservedWords = ["import", "with", "struct", "enum", "alias", "let", "fn", "for", "in", "if", "elif", "else", "return"]

reserved :: String -> Parser ()
reserved word = lexeme $ string word *> notFollowedBy alphaNumChar

-- Basic Parsers

semicolon :: Parser String
semicolon = symbol ";"

comma :: Parser String
comma = symbol ","

doubleColon :: Parser String
doubleColon = symbol "::"

-- Imports

importParser :: Parser ()
importParser = do
  reserved "import"
  _ <- identifier
  reserved "with"
  _ <- sepBy1 identifier (symbol " ")
  void semicolon

-- Types

typeParser :: Parser String
typeParser = identifier

structParser :: Parser ()
structParser = do
  reserved "struct"
  _ <- identifier
  symbol "="
  _ <- sepBy1 (do
    fieldName <- identifier
    fieldType <- typeParser
    return (fieldName, fieldType)) doubleColon
  void semicolon

enumParser :: Parser ()
enumParser = do
  reserved "enum"
  _ <- identifier
  symbol "="
  _ <- sepBy1 (do
    variantName <- identifier
    variantType <- optional typeParser
    return (variantName, variantType)) (symbol "|")
  void semicolon

aliasParser :: Parser ()
aliasParser = do
  reserved "alias"
  _ <- identifier
  symbol "="
  _ <- typeParser
  void semicolon

-- Objects

objectParser :: Parser ()
objectParser = do
  reserved "let"
  _ <- identifier
  doubleColon
  _ <- typeParser
  _ <- optional (do
    doubleColon
    sepBy1 identifier (symbol " "))
  symbol "="
  _ <- many (noneOf ";")
  void semicolon

-- Containers

containerParser :: Parser ()
containerParser = do
  reserved "let"
  _ <- identifier
  doubleColon
  _ <- choice [
    try $ string "Tuple" *> between (symbol "[") (symbol "]") (sepBy1 (fmap return typeParser) comma),
    try $ string "List" *> between (symbol "[") (symbol "]") (fmap return typeParser),
    try $ string "Map" *> between (symbol "[") (symbol "]") (do
      keyType <- typeParser
      comma
      valueType <- typeParser
      return [keyType, valueType])
    ]
  _ <- optional (doubleColon *> identifier)
  symbol "="
  _ <- between (symbol "[") (symbol "]") (sepBy (many (noneOf ",]")) comma)
  void semicolon
  
-- Functions

functionParser :: Parser ()
functionParser = do
  reserved "fn"
  _ <- identifier
  symbol "="
  _ <- sepBy1 (do
    argName <- identifier
    argType <- typeParser
    return (argName, argType)) doubleColon
  _ <- typeParser -- return type
  void $ between (symbol "{") (symbol "}") (many (noneOf "}"))

-- Iteration

forLoopParser :: Parser ()
forLoopParser = do
  reserved "for"
  _ <- identifier
  reserved "in"
  _ <- (try $ reserved "range" *> many (noneOf "{")) <|> identifier
  void $ between (symbol "{") (symbol "}") (many (noneOf "}"))

-- Conditionals

conditionalParser :: Parser ()
conditionalParser = do
  reserved "if"
  _ <- many (noneOf "{")
  _ <- between (symbol "{") (symbol "}") (many (noneOf "}"))
  _ <- optional (do
    reserved "elif"
    _ <- many (noneOf "{")
    void $ between (symbol "{") (symbol "}") (many (noneOf "}")))
  _ <- optional (do
    reserved "else"
    void $ between (symbol "{") (symbol "}") (many (noneOf "}")))
  return ()
  
-- Main Parser

programParser :: Parser ()
programParser = do
  _ <- many (choice [
    try importParser,
    try structParser,
    try enumParser,
    try aliasParser,
    try objectParser,
    try containerParser,
    try functionParser,
    try forLoopParser,
    try conditionalParser
    ])
  eof

-- Parse function

parseProgram :: String -> Either (ParseErrorBundle String Void) ()
parseProgram = parse programParser ""