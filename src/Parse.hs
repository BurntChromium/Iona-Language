{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Parse where

import Text.Megaparsec
import Data.Void
import Control.Monad.Writer

import qualified SharedTypes as Core
import Errors(Problem(..), ProblemClass(..), parserError)

-- ==================== Data Schema ====================

-- | Built-in data types
data PrimitiveDataType =
    PrimitiveInt Int |
    PrimitiveFloat Float |
    PrimitiveStr String |
    PrimitiveBool Bool
    deriving (Show, Eq)

-- | Valid properties for a variable
data VariableProperty = Mutable | ThreadSafe deriving (Show, Eq)

-- | Valid properties for a function
data FunctionProperty = Pure | Public deriving (Show, Eq)

-- | What behaviors ("side effects") can a function perform?
data Permission = ReadFile | WriteFile | Custom String deriving (Show, Eq)

-- | Core data for a variable
data Variable = Variable {
    name :: String,
    vType :: PrimitiveDataType, -- `type` is a reserved word in Haskell
    vProps :: [VariableProperty]
} deriving (Show, Eq)

type Scope = String

-- | AST nodes: only some are true (recursive) trees because statements like import or property lists are flat
data AST =
    Import ImportStmt |
    DeclareFn Function |
    DeclareVar Variable |
    Expression ExpressionStmt |
    FnProperty [FunctionProperty] |
    FnPermission [Permission]
    deriving (Show, Eq)

data ImportStmt = NodeImport {
    file :: String,
    items :: [String]
} deriving (Show, Eq)

-- | Core data for a function
data Function = Function {
    name :: String,
    arguments :: [Variable],
    returnType :: PrimitiveDataType,
    fProps :: [FunctionProperty],
    permissions :: [Permission]
} deriving (Show, Eq)

data ExpressionStmt = Literal PrimitiveDataType | Method String [ExpressionStmt] deriving (Show, Eq)

-- ==================== Parsers ====================

-- Define Parser type for tokens
type Parser = ParsecT Void [Core.Token] (Writer [Problem])


symbol :: Core.Symbol -> Parser Core.Token
symbol sym = token testToken Nothing
  where
    testToken t = if sym == Core.sym t then Just t else Nothing 

-- Define the variable declaration parser
parseVarDecl :: Parser AST
parseVarDecl = do
    -- Expect the 'let' keyword
    _ <- symbol Core.Let
    
    -- Expect an identifier for the variable name
    varToken <- symbol Core.Identifier
    let varName = Core.str varToken
    
    -- Expect the '::' field separator
    _ <- symbol Core.FieldSep
    
    -- Expect an identifier for the variable type
    typeToken <- symbol Core.Identifier
    let varType = Core.str typeToken
    
    -- Optionally parse additional properties as identifiers
    properties <- many (Core.str <$> symbol Core.Identifier)
    
    -- Expect the '=' sign (we just match it but don't consume the rest here)
    _ <- symbol Core.Equals

    let output = Variable (varName varType properties)
    
    -- Construct and return the variable declaration
    return $ DeclareVar output


-- ==================== Utilities ====================

-- TODO: replace list with sequence so I can push to the back more efficiently
-- | A modification of `span` that terminates on a *specific* criteria instead of anything outside the predicate
spanUntil :: ([a], [a]) -> (a -> Bool) -> (a -> Bool) -> Either a ([a], [a])
spanUntil stream predicate terminal
    | null (fst stream) = Right stream
    | terminal (head (fst stream)) = Right (tail (fst stream), snd stream ++ [head (fst stream)])
    | predicate (head (fst stream)) = spanUntil (tail (fst stream), snd stream ++ [head (fst stream)]) predicate terminal
    | otherwise = Left (head (fst stream))