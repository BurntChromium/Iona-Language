module Main where

import Test.HUnit
import TestParse

tests =
  TestList
    [ -- Parse
      TestLabel
        "parse a single comment"
        testParseComment
    ]

main = runTestTTAndExit tests
