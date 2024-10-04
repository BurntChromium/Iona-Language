module SharedTypes where

import Source (Cursor (..))

-- | A symbol is the what most would consider a Token normally, but we "enrich" the token with additional data
data Symbol = Comment | NewLine | Identifier | Import | With | Let | Mut | Return | FieldSep | Struct | Enum | Equals | Bar | Is | Derives | Alias | FnDeclare | FnObject | Partial | For | IterableIn | NumberLiteral | EndStmt | Colon | OpenParen | CloseParen | OpenScope | CloseScope | Minus | Properties | Permissions | ContractIn | ContractOut | ContractInvariant deriving (Show, Eq)

-- | A token has its original string, its symbol, and where it is in the text
data Token = Token
  { str :: String,
    sym :: Symbol,
    pos :: Cursor
  }
  deriving (Show, Eq)
