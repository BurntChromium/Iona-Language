{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Refine "raw" AST to something more structured
-- Stage 1 of post-processing
module ASTRefinery where

import Data.Text (Text)
import Data.Text qualified as T
import Errors (Problem)
import Errors qualified
import Parse (ASTNode (StructDecl), ASTNodeWithPos (ASTNodeWithPos))
import Text.Megaparsec (SourcePos)

-------------------- Types --------------------

-- | Supported types
data Types = Int | Float | Str | List [Types] | Tuple [Types] | UserDefined Text

-- | Match text to get a type
recordType :: Text -> Types
recordType typeText = case T.stripPrefix "[" (T.strip typeText) of
  Just rest -> parseCompound (T.takeWhile (/= ']') rest)
  Nothing -> case typeText of
    "int" -> Int
    "float" -> Float
    "str" -> Str
    _ -> UserDefined typeText

parseCompound :: Text -> Types
parseCompound t
  | "List" `T.isPrefixOf` t = List [recordType (T.drop 4 t)]
  | "Tuple" `T.isPrefixOf` t = Tuple (map recordType $ T.splitOn "," (T.drop 5 t))
  | otherwise = UserDefined t

-------------------- Structures --------------------

-- | Creating a new struct
data StructDeclaration = StructDeclaration
  { name :: Text,
    fields :: [(Text, Types)],
    props :: [StructProperties],
    impl :: [StructDerives]
  }

-- | Valid struct properties for the compiler
data StructProperties = PublicStruct | ThreadSafeStruct

-- | Convert text to a struct property
checkStructProp :: SourcePos -> Text -> Either Problem StructProperties
checkStructProp pos propText = case propText of
  "Public" -> Right PublicStruct
  "ThreadSafe" -> Right ThreadSafeStruct
  _ -> Left $ Errors.parserProblem pos ("Unknown property: " <> T.unpack propText)

-- | Methods the struct can derive / implement automatically
data StructDerives = Log | UserDefinedDerives Text

-- | Convert text to struct derives
recordDerives :: Text -> StructDerives
recordDerives dText = case dText of
  "Log" -> Log
  _ -> UserDefinedDerives dText

-- Generate and print error from AST refinement
refineStruct :: ASTNodeWithPos -> Either Problem StructDeclaration
refineStruct (ASTNodeWithPos (StructDecl name fields props derives) pos) =
  case mapM (checkStructProp pos) props of
    Left invalidProp -> Left invalidProp
    Right validProps -> Right $ StructDeclaration name [(field_name, recordType typ) | (field_name, typ) <- fields] validProps (map recordDerives derives)
refineStruct (ASTNodeWithPos _ pos) =
  Left $ Errors.parserProblem pos "Tried to refine something that's not a struct into a Struct"

-------------------- Functions --------------------

-- | Creating a new function
data FunctionDeclaration = FunctionDeclaration
  { name :: Text,
    args :: [(Text, Types)],
    return :: Types,
    props :: FunctionProperties,
    uses :: FunctionPermissions
  }

-- | Check if function properties are recognized
data FunctionProperties = PureFunction | PublicFunction

data FunctionPermissions
  = ReadFile
  | WriteFile
  | UserDefinedFPermission Text
