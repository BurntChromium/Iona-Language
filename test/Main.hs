module Main where

import Test.HUnit

import Source (initCursor, updateCursor, Cursor(..))

testUpdateCursor1 = TestCase (do 
    let c = initCursor
    let actual = updateCursor c 'p'
    assertEqual "update should not change line" (line actual) 1
    assertEqual "update should move column" (column actual) 2)

testUpdateCursor2 = TestCase (do 
    let c = initCursor
    let actual = updateCursor c '\n'
    assertEqual "update should change line" (line actual) 2
    assertEqual "update should reset column" (column actual) 1)

tests = TestList [
    TestLabel "test update cursor on other char"
        testUpdateCursor1,
    TestLabel "test update cursor on newline"
        testUpdateCursor2
        ]

main = runTestTTAndExit tests
