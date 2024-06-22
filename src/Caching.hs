module Caching where

import Data.Hashable(hash)

data CodeCache = CodeCache {
    filename :: String,
    hashCode :: Int,
    binary :: Maybe Int
}

-- | Given a (truncated) filename and its contents, emit a cache object for that code (TODO: emit binary representation of the data we want?)
hashFile :: String -> String -> CodeCache
hashFile fileName code = CodeCache { filename = fileName, hashCode = hash code, binary = Nothing }