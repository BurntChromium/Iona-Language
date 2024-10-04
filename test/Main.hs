module Main where

import Test.HUnit
import TestLex
import TestParse
import TestSource

tests =
  TestList
    [ -- Terminal
      TestLabel
        "update cursor on other char"
        testUpdateCursor1,
      TestLabel
        "update cursor on newline"
        testUpdateCursor2,
      -- Lexer
      TestLabel
        "update cursor in lexer on other char"
        testUpdateLexCursor1,
      TestLabel
        "update cursor in lexer on newline"
        testUpdateLexCursor2,
      TestLabel "add semicolon to lexer" testAddTokenToLexer,
      TestLabel "lex a comment statement" testLexCode1,
      TestLabel "lex an import statement" testLexCode2,
      TestLabel "lex an comment + import w/ newline" testLexCode3,
      TestLabel "span until on import" testSpanUntil1,
      TestLabel "span until on import with error" testSpanUntil2
    ]

main = runTestTTAndExit tests
