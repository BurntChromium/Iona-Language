{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

-- AST for Iona Lang
data IonaDecl
  = ImportDecl Text [Text]
  | StructDecl Text [(Text, Text)] [Text]
  | EnumDecl Text [(Text, Maybe Text)] [Text]
  | LetDecl Text Text [Text] Text
  | FuncDecl Text [(Text, Text)] Text [Text] [Text] [Text] [IonaStmt]
  deriving (Show)

data IonaStmt
  = IfStmt IonaExpr [IonaStmt] (Maybe [IonaStmt])
  | ForStmt Text IonaExpr [IonaStmt]
  | ReturnStmt IonaExpr
  | ExprStmt IonaExpr
  | Contract Text IonaExpr
  | Annotation Text [Text]
  deriving (Show)

data IonaExpr
  = Var Text
  | IntLit Int
  | StrLit Text
  | FuncCall Text [IonaExpr]
  deriving (Show)

-- Lexer
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Identifiers are names for variables, fns, etc
identifier :: Parser Text
identifier = lexeme $ T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

-- Types are just identifiers for now
pType :: Parser Text
pType = identifier

-- Parsing a struct field: fieldName followed by its type, separated by a space
pField :: Parser (Text, Text)
pField = do
  name <- identifier
  typ <- lexeme identifier  -- Expect a space between field name and type
  return (name, typ)

-- Parsing imports
pImport :: Parser IonaDecl
pImport = do
  _ <- symbol "import"
  file <- identifier
  _ <- symbol "with"
  imports <- many identifier
  _ <- symbol ";"
  return $ ImportDecl file imports

-- Parsing structs
pStruct :: Parser IonaDecl
pStruct = do
  _ <- symbol "struct"
  name <- identifier
  _ <- symbol "="
  fields <- pField `sepBy` symbol "::"
  properties <- many identifier
  _ <- symbol ";"
  return $ StructDecl name fields properties

-- Parsing enums
pEnum :: Parser IonaDecl
pEnum = do
  _ <- symbol "enum"
  name <- identifier
  _ <- symbol "="
  variants <- variant `sepBy` symbol "|"
  properties <- many identifier
  _ <- symbol ";"
  return $ EnumDecl name variants properties
  where
    variant = do
      varName <- identifier
      typ <- optional pType
      return (varName, typ)

-- Parsing let bindings
pLet :: Parser IonaDecl
pLet = do
  _ <- symbol "let"
  name <- identifier
  _ <- symbol "::"
  typ <- pType
  properties <- many (symbol "::" *> identifier)
  _ <- symbol "="
  value <- identifier
  _ <- symbol ";"
  return $ LetDecl name typ properties value

-- Parsing function definitions
pFunc :: Parser IonaDecl
pFunc = do
  _ <- symbol "fn"
  name <- identifier
  _ <- symbol "="
  args <- pField `sepBy` symbol "::"
  _ <- symbol "->"
  retType <- lexeme pType
  stmts <- pBlock
  return $ FuncDecl name args retType [] [] [] stmts

-- pMetadata :: Parser IonaStmt
-- pMetadata = 

pBlock :: Parser [IonaStmt]
pBlock = between (symbol "{") (symbol "}") (many pStmt)

pStmt :: Parser IonaStmt
pStmt = choice
  [ pIfStmt
  , pForStmt
  , pReturnStmt
  , pContract
  , pAnnotation
  , ExprStmt <$> pExpr
  ]

-- If statement
pIfStmt :: Parser IonaStmt
pIfStmt = do
  _ <- symbol "if"
  cond <- pExpr
  stmts <- pBlock
  elseStmts <- optional (symbol "else" *> pBlock)
  return $ IfStmt cond stmts elseStmts

-- For loop
pForStmt :: Parser IonaStmt
pForStmt = do
  _ <- symbol "for"
  var <- identifier
  _ <- symbol "in"
  iter <- pExpr
  stmts <- pBlock
  return $ ForStmt var iter stmts

-- Return statement
pReturnStmt :: Parser IonaStmt
pReturnStmt = do
  _ <- symbol "return"
  expr <- pExpr
  _ <- symbol ";"
  return $ ReturnStmt expr

-- Parse contracts (In, Out, Invariant)
pContract :: Parser IonaStmt
pContract = do
  keyword <- choice [symbol "In", symbol "Out", symbol "Invariant"]
  _ <- symbol ":"
  expr <- pExpr
  return $ Contract keyword expr

-- Parse annotations (Props, Uses)
pAnnotation :: Parser IonaStmt
pAnnotation = do
  keyword <- choice [symbol "Props", symbol "Uses"]
  _ <- symbol ":"
  values <- identifier `sepBy` space
  return $ Annotation keyword values

-- Expression parsing
pExpr :: Parser IonaExpr
pExpr = choice
  [ Var <$> identifier
  , IntLit <$> lexeme L.decimal
  , StrLit <$> lexeme (T.pack <$> (char '"' *> manyTill L.charLiteral (char '"')))
  , pFuncCall
  ]

pFuncCall :: Parser IonaExpr
pFuncCall = do
  name <- identifier
  args <- between (symbol "(") (symbol ")") (pExpr `sepBy` symbol ",")
  return $ FuncCall name args

-- Entry point for the parser
pIona :: Parser [IonaDecl]
pIona = many (sc *> choice [pImport, pStruct, pEnum, pLet, pFunc] <* sc)

