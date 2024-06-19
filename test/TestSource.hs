module TestSource where

import Test.HUnit

import Source (initCursor, updateCursor, Cursor(..))

testUpdateCursor1 = TestCase (do 
    let c = initCursor
    let actual = updateCursor c 'p'
    assertEqual "update should not change line" 1 (line actual)
    assertEqual "update should move column" 2 (column actual))

testUpdateCursor2 = TestCase (do 
    let c = initCursor
    let actual = updateCursor c '\n'
    assertEqual "update should change line" 2 (line actual)
    assertEqual "update should reset column" 1 (column actual))
