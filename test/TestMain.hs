module Main where

import Test.HUnit
import TestParse

tests :: Test
tests = TestList
  [ TestLabel "parse a single comment" testParseComment
  , TestLabel "parse basic fn" testParseFn1
  ]

main :: IO()
main = do
  counts <- runTestTT tests
  print counts