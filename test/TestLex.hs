module TestLex where

import Test.HUnit

import Source (Cursor(..))
import Lex (LexerState(..), initLexerState, advanceCursor, addTokenToLexer, Symbol(..), lexer, Token(..))

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

testLexCode1 = TestCase (do
    let code = "# here's a comment"
    let lexerOut = lexer initLexerState code
    let ts = tokens lexerOut
    assertEqual "first item should be a comment" (sym (head ts)) Comment
    assertEqual "comments span the whole length" (length ts) 1
    )

testLexCode2 = TestCase (do
    let code = "import math with pow sqrt;"
    let lexerOut = lexer initLexerState code
    let ts = tokens lexerOut
    let symbols = map sym ts
    let expectedSymbols = [Import, Identifier, With, Identifier, Identifier, EndStmt]
    assertEqual "we should have these specific symbols" symbols expectedSymbols
    )