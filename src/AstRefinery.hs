{-# LANGUAGE OverloadedStrings #-}

-- | Refine "raw" AST to something more structured
-- Stage 1 of post-processing
module ASTRefinery where

import Data.Text (Text)

-- | Check if function properties are recognized
data FunctionProperties
  = Pure
  | Public
