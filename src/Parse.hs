{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Parse where

import Control.Applicative
import Control.Monad (void)
import Data.Maybe (maybeToList)
import Text.Read(readEither)

import SharedTypes (Token(..), Symbol(..))
import Errors(Problem(..), ProblemClass(..), quickProblem)

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
    Import Scope ImportStmt |
    DeclareFn Scope Function [AST] |
    DeclareVar Scope Variable [AST] |
    Expression Scope ExpressionStmt |
    FnProperty Scope [FunctionProperty] |
    FnPermission Scope [Permission]
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

-- | A parser can return multiple types, a Variable, an AST, etc.
type Parser a = [Token] -> Either Problem (a, [Token])

-- Parse a token against a predicate
tokenSatisfying :: (Token -> Bool) -> Parser Token
tokenSatisfying pred = Parser $ \case
    t:ts | pred t -> Right (t, ts)
    t:ts | not (pred t) -> Left quickProblem Error (pos t) ("unexpected token: " ++ str t)

-- | Flipped version of the <$> (fmap) operator 
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

-- Parse identifier token and return the string
parseIdentifier :: Parser String
parseIdentifier = tokenSatisfying (\t -> sym t == Identifier) <&> str

-- Parse a symbol, but just checking instead of returning
parseSymbol :: Symbol -> Parser ()
parseSymbol s = tokenSatisfying (\t -> sym t == s)

option :: a -> Parser a -> Parser a
option fallback parser = parser <|> pure fallback

parseSome :: Parser a -> Parser [a]
parseSome parser = (:) <$> parser <*> many parser

parseMany :: Parser a -> Parser [a]
parseMany parser = some parser <|> pure []

-- Handle number literals (caller is assumed to only invoke on tokens of the right symbol type)
-- We treat anything with a "." in it as a float, otherwise it's an integer
-- TODO: we probably want `Parser = Either [Problem] AST` and this should return Parser
parseNumberLiteral :: Token -> Either Problem PrimitiveDataType
parseNumberLiteral t
    | '.' `elem` str t =
        case readEither (str t) :: Either String Float of
            Right f -> Right (PrimitiveFloat f)
            Left err -> Left (quickProblem Error (pos t) ("Invalid float literal: " ++ err))
    | otherwise =
        case readEither (str t) :: Either String Int of
            Right i -> Right (PrimitiveInt i)
            Left err  -> Left (quickProblem Error (pos t) ("Invalid integer literal: " ++ err))

parseVariableDeclaration :: Parser Variable
parseVariableDeclaration = do
    void $ symbol VarDeclare
    name <- identifier
    void $ symbol FieldSep
    vType <- dataType
    vProps <- option [] (parseSymbol FieldSep *> many identifier)
    void $ parseSymbol Equals
    return $ Variable name vType (map parseVariableProperty vProps)

-- ==================== Utilities ====================

-- TODO: replace list with sequence so I can push to the back more efficiently
-- | A modification of `span` that terminates on a *specific* criteria instead of anything outside the predicate
spanUntil :: ([a], [a]) -> (a -> Bool) -> (a -> Bool) -> Either a ([a], [a])
spanUntil stream predicate terminal
    | null (fst stream) = Right stream
    | terminal (head (fst stream)) = Right (tail (fst stream), snd stream ++ [head (fst stream)])
    | predicate (head (fst stream)) = spanUntil (tail (fst stream), snd stream ++ [head (fst stream)]) predicate terminal
    | otherwise = Left (head (fst stream))