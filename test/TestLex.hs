module TestLex where

import Lex (LexerState (..), Symbol (..), Token (..), addTokenToLexer, advanceCursor, initLexerState, lexer)
import Source (Cursor (..))
import Test.HUnit

testUpdateLexCursor1 =
  TestCase
    ( do
        let s = initLexerState
        let actual = advanceCursor s 'p'
        assertEqual "update should not change line" 1 (line (csr actual))
        assertEqual "update should move column" 2 (column (csr actual))
    )

testUpdateLexCursor2 =
  TestCase
    ( do
        let s = initLexerState
        let actual = advanceCursor s '\n'
        assertEqual "update should change line" 2 (line (csr actual))
        assertEqual "update should reset column" 1 (column (csr actual))
    )

testAddTokenToLexer =
  TestCase
    ( do
        let s = initLexerState
        let actual = addTokenToLexer [';'] EndStmt (csr s) s
        assertEqual "update should change line" 1 (line (csr actual))
        assertEqual "update should reset column" 2 (column (csr actual))
        assertEqual "tokens should be longer" 1 (length (tokens actual))
    )

-- Lex a comment
testLexCode1 =
  TestCase
    ( do
        let code = "# here's a comment"
        let lexerOut = lexer initLexerState code
        let ts = tokens lexerOut
        assertEqual "first item should be a comment" (sym (head ts)) Comment
        assertEqual "comments span the whole length" (length ts) 1
    )

-- Lex an import
testLexCode2 =
  TestCase
    ( do
        let code = "import math with pow sqrt;"
        let lexerOut = lexer initLexerState code
        let ts = tokens lexerOut
        let symbols = map sym ts
        let expectedSymbols = [Import, Identifier, With, Identifier, Identifier, EndStmt]
        assertEqual "we should have these specific symbols" symbols expectedSymbols
    )

-- Lex multiline: comment + import
testLexCode3 =
  TestCase
    ( do
        let code = "# here's a comment\nimport math with pow sqrt;"
        let lexerOut = lexer initLexerState code
        let ts = tokens lexerOut
        let symbols = map sym ts
        let expectedSymbols = [Comment, NewLine, Import, Identifier, With, Identifier, Identifier, EndStmt]
        assertEqual "we should have these specific symbols" symbols expectedSymbols
    )
