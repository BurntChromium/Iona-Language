module Main where

import Test.HUnit

import TestSource

tests = TestList [
    TestLabel "test update cursor on other char"
        testUpdateCursor1,
    TestLabel "test update cursor on newline"
        testUpdateCursor2
        ]

main = runTestTTAndExit tests
