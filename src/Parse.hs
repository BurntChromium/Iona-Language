{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug

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
  | FloatLit Double
  | StrLit Text
  | FuncCall Text [Expression]
  | TupleLit [Expression]
  | ListLit [Expression]
  | FieldAccess Expression Text
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
  typ <- lexeme identifier -- Expect a space between field name and type
  return (name, typ)

-- Helper for stuff between parenthesis
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Helper for stuff between square brackets
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

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
pLet = dbg "pLet" $ do
  _ <- symbol "let"
  name <- identifier
  _ <- symbol "::"
  typ <- pType
  properties <- many (symbol "::" *> identifier)
  _ <- symbol "="
  value <- pExpr
  _ <- symbol ";"
  return $ VarStmt name typ properties value

-- Parsing function definitions (NOT function calls - those are below)
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
pStmt =
  choice
    [ pLet,
      pIfStmt,
      pForStmt,
      pReturnStmt,
      pContract,
      pAnnotation,
      ExprStmt <$> pExpr
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
  expr <- pExpr
  _ <- symbol ";"
  return $ Contract keyword expr

-- Parse annotations (Props, Uses)
pAnnotation :: Parser Statement
pAnnotation = do
  keyword <- choice [symbol "Props", symbol "Uses"]
  _ <- symbol ":"
  values <- identifier `sepBy` space
  _ <- symbol ";"
  return $ Annotation keyword values

-- Main expression parser
pExpr :: Parser Expression
pExpr = do
  terms <- some pTerm
  case terms of
    [] -> fail "Empty expression"
    [singleTerm] -> return singleTerm
    (Var func : args) -> return $ FuncCall func args
    _ -> return $ foldl1 (\f x -> FuncCall (extractFuncName f) [f, x]) terms

-- Helper function to extract function name from an expression
extractFuncName :: Expression -> Text
extractFuncName (Var name) = name
extractFuncName _ = "anonymous_func" -- Default name for non-variable expressions used as functions

-- Parse a term (the base of an expression)
pTerm :: Parser Expression
pTerm =
  choice
    [ try (FloatLit <$> lexeme L.float),
      IntLit <$> lexeme L.decimal,
      StrLit <$> lexeme (T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))),
      try pFieldAccess, -- Handle field access
      pListLiteral,
      pTupleLiteral,
      Var <$> identifier,
      between (symbol "(") (symbol ")") pExpr
    ]

-- Parser for field access (struct fields)
pFieldAccess :: Parser Expression
pFieldAccess = do
  base <- choice [Var <$> identifier, between (symbol "(") (symbol ")") pExpr]
  fields <- many (symbol "." *> identifier)
  return $ foldl FieldAccess base fields

-- Parse function calls with parentheses
pFuncCallParen :: Parser Expression
pFuncCallParen = do
  fnName <- identifier
  args <- parens (pExpr `sepBy` symbol ",")
  return $ FuncCall fnName args

-- Parse function calls without parentheses
pFuncCallNoParen :: Parser Expression
pFuncCallNoParen = do
  fnName <- identifier
  args <- many pExpr
  return $ FuncCall fnName args

-- Parse list literals
pListLiteral :: Parser Expression
pListLiteral = do
  elements <- brackets (pExpr `sepBy` symbol ",")
  return $ ListLit elements

-- Parse tuple literals
pTupleLiteral :: Parser Expression
pTupleLiteral = do
  elements <- parens (pExpr `sepBy` symbol ",")
  return $ TupleLit elements

-- Entry point for the parser
pIona :: Parser [ASTNode]
pIona = many (sc *> choice [pImport, pStruct, pEnum, pFunc] <* sc)
