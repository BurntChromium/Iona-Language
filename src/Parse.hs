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
data ASTNode
  = ImportDecl Text [Text]
  | StructDecl Text [(Text, Text)] [Text]
  | EnumDecl Text [(Text, Maybe Text)] [Text]
  | FuncDecl Text [(Text, Text)] Text [Text] [Text] [Text] [Statement]
  deriving (Show)

-- These can show up inside a block/scope
data Statement
  = IfStmt Expression [Statement] (Maybe [Statement])
  | ForStmt Text Expression [Statement]
  | ReturnStmt Expression
  | ExprStmt Expression
  | Contract Text Expression
  | Annotation Text [Text]
  | VarStmt Text Text [Text] Expression
  deriving (Show)

data Expression
  = Var Text
  | IntLit Int
  | StrLit Text
  | FuncCall Text [Expression]
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
pImport :: Parser ASTNode
pImport = do
  _ <- symbol "import"
  file <- identifier
  _ <- symbol "with"
  imports <- many identifier
  _ <- symbol ";"
  return $ ImportDecl file imports

-- Parsing structs
pStruct :: Parser ASTNode
pStruct = do
  _ <- symbol "struct"
  name <- identifier
  _ <- symbol "="
  fields <- pField `sepBy` symbol "::"
  properties <- many identifier
  _ <- symbol ";"
  return $ StructDecl name fields properties

-- Parsing enums
pEnum :: Parser ASTNode
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
pLet :: Parser Statement
pLet = do
  _ <- symbol "let"
  name <- identifier
  _ <- symbol "::"
  typ <- pType
  properties <- many (symbol "::" *> identifier)
  _ <- symbol "="
  value <- pExpr
  _ <- symbol ";"
  return $ VarStmt name typ properties value

-- Parsing function definitions
pFunc :: Parser ASTNode
pFunc = do
  _ <- symbol "fn"
  name <- identifier
  _ <- symbol "="
  args <- pField `sepBy` symbol "::"
  _ <- symbol "->"
  retType <- lexeme pType
  FuncDecl name args retType [] [] [] <$> pBlock

-- pMetadata :: Parser Statement
-- pMetadata = 

pBlock :: Parser [Statement]
pBlock = between (symbol "{") (symbol "}") (many pStmt)

pStmt :: Parser Statement
pStmt = choice
  [ pLet
  , pIfStmt
  , pForStmt
  , pReturnStmt
  , pContract
  , pAnnotation
  , ExprStmt <$> pExpr
  ]

-- If statement
pIfStmt :: Parser Statement
pIfStmt = do
  _ <- symbol "if"
  cond <- pExpr
  stmts <- pBlock
  elseStmts <- optional (symbol "else" *> pBlock)
  return $ IfStmt cond stmts elseStmts

-- For loop
pForStmt :: Parser Statement
pForStmt = do
  _ <- symbol "for"
  var <- identifier
  _ <- symbol "in"
  iter <- pExpr
  ForStmt var iter <$> pBlock

-- Return statement
pReturnStmt :: Parser Statement
pReturnStmt = do
  _ <- symbol "return"
  expr <- pExpr
  _ <- symbol ";"
  return $ ReturnStmt expr

-- Parse contracts (In, Out, Invariant)
pContract :: Parser Statement
pContract = do
  keyword <- choice [symbol "In", symbol "Out", symbol "Invariant"]
  _ <- symbol ":"
  Contract keyword <$> pExpr

-- Parse annotations (Props, Uses)
pAnnotation :: Parser Statement
pAnnotation = do
  keyword <- choice [symbol "Props", symbol "Uses"]
  _ <- symbol ":"
  values <- identifier `sepBy` space
  return $ Annotation keyword values

-- Expression parsing
pExpr :: Parser Expression
pExpr = choice
  [ Var <$> identifier
  , IntLit <$> lexeme L.decimal
  , StrLit <$> lexeme (T.pack <$> (char '"' *> manyTill L.charLiteral (char '"')))
  , pFuncCall
  ]

pFuncCall :: Parser Expression
pFuncCall = do
  name <- identifier
  args <- between (symbol "(") (symbol ")") (pExpr `sepBy` symbol ",")
  return $ FuncCall name args

-- Entry point for the parser
pIona :: Parser [ASTNode]
pIona = many (sc *> choice [pImport, pStruct, pEnum, pFunc] <* sc)

