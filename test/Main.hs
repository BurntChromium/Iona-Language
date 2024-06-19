module Main where

import Test.HUnit

import TestSource
import TestLex

tests = TestList [
    TestLabel "update cursor on other char"
        testUpdateCursor1,
    TestLabel "update cursor on newline"
        testUpdateCursor2,
    TestLabel "update cursor in lexer on other char"
        testUpdateLexCursor1,
    TestLabel "update cursor in lexer on newline"
        testUpdateLexCursor2,
    TestLabel "add semicolon to lexer" testAddTokenToLexer
        ]

main = runTestTTAndExit tests
