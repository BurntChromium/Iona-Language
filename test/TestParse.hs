module TestParse where

import Test.HUnit
import Lex (LexerState(..), initLexerState, advanceCursor, addTokenToLexer, Symbol(..), lexer, Token(..))
import Parse(spanUntil)

-- Test `takeUntil`
testSpanUntil1 = TestCase (do
    let code = "import math with pow sqrt;\nimport duck with quack;"
    let lexerOut = lexer initLexerState code
    let ts = tokens lexerOut
    let span1 = spanUntil (ts, []) (\x -> sym x `elem` [Import, Identifier, With]) (\x -> sym x == EndStmt)
    case span1 of 
        Left err -> assertFailure (show err)
        Right toks -> assertEqual "should have 6 tokens" (length (fst toks)) 6
    )

testSpanUntil2 = TestCase (do
    let code = "import math with return sqrt;\nimport duck with quack;"
    let lexerOut = lexer initLexerState code
    let ts = tokens lexerOut
    let span1 = spanUntil (ts, []) (\x -> sym x `elem` [Import, Identifier, With]) (\x -> sym x == EndStmt)
    case span1 of 
        Left tok -> assertEqual "should error on 'return'" (sym tok) Return
        Right _ -> assertFailure "should be an error"
    )