module TestLex where

import Test.HUnit

import Source (Cursor(..))
import Lex (LexerState(..), initLexerState, advanceCursor, addTokenToLexer, Symbol(..))

testUpdateLexCursor1 = TestCase (do 
    let s = initLexerState
    let actual = advanceCursor s 'p'
    assertEqual "update should not change line" 1 (line (csr actual))
    assertEqual "update should move column" 2 (column (csr actual)))

testUpdateLexCursor2 = TestCase (do 
    let s = initLexerState
    let actual = advanceCursor s '\n'
    assertEqual "update should change line" 2 (line (csr actual))
    assertEqual "update should reset column" 1 (column (csr actual)))

testAddTokenToLexer = TestCase (do 
    let s = initLexerState
    let actual = addTokenToLexer [';'] EndStmt (csr s) s  
    assertEqual "update should change line" 1 (line (csr actual))
    assertEqual "update should reset column" 2 (column (csr actual))
    assertEqual "tokens should be longer" 1 (length (tokens actual)))
