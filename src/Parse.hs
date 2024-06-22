module Parse where

import Text.Read(readEither)

import Lex (Token(..), Symbol(..))
import Errors(Problem(..), ProblemClass(..), quickProblem)

-- | Built-in data types
data PrimitiveDataType = PrimitiveInt Int | PrimitiveFloat Float | PrimitiveStr String | PrimitiveBool Bool deriving (Show, Eq)

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

-- Iterate over a list of tokens until an element is found
-- This is sort of like `span` but doesn't require writing a custom predicate for matching generic patterns
-- Example: `let stmt = takeUntil tokens [Identifier, NumberLiteral] EndStmt`
-- `[Token]` is the input stream of tokens
-- `[Symbol]` is a list of valid "middle values" (if empty, accept ALL inputs)
-- `Symbol` is the terminal value
takeUntil :: [Token] -> [Symbol] -> Symbol -> Either Problem [Token]
takeUntil [] _ _ = Right []  -- If no tokens, return an empty list
takeUntil (t:ts) acceptable terminal
  | sym t == terminal = Right []  -- Stop if we hit the terminal symbol
    -- if all acceptable, or if current token is acceptable, then
  | null acceptable || sym t `elem` acceptable = do
      -- Include the token and continue
      rest <- takeUntil ts acceptable terminal
      return (t : rest)
  | otherwise =
      -- Create a problem for the unexpected token
        let problem = Problem {
            cls = Error,
            cursor = pos t,
            message = "Unexpected token: " ++ str t,
            hint = Just $ "Expected one of: " ++ show acceptable ++ " or " ++ show terminal,
            ref = Nothing
        }
        in Left problem