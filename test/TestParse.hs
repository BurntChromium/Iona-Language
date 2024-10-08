module TestParse where

import Data.Text qualified as T
import Parse qualified
import Test.HUnit
import Text.Megaparsec (errorBundlePretty, parse)

testParseComment :: Test
testParseComment = TestCase $ do
  let code = "# This is a comment"
  let input = T.pack code
  case parse Parse.pIona "" input of
    Left err -> assertFailure $ "Parse failed: " ++ errorBundlePretty err
    Right result ->
      assertEqual
        "Should parse empty file (only comments) correctly"
        []
        result

testParseFn1 :: Test
testParseFn1 = TestCase $ do
  let code = "fn sqrt = x int -> float { \
  \  Props: Pure Public; \
  \  return pow x 0.5; \
\}"
  let input = T.pack code
  let expected = Parse.FuncDecl (T.pack "notright") [(T.pack "x", T.pack "int")] (T.pack "float") [Parse.Annotation (T.pack "Props") [T.pack "Pure", T.pack "Public"], Parse.ReturnStmt (Parse.FuncCall (T.pack "pow") [Parse.Var (T.pack "x") , Parse.FloatLit 0.5])]
  case parse Parse.pIona "" input of
    Left err -> assertFailure $ "Parse failed: " ++ errorBundlePretty err
    Right result ->
      assertEqual
        "Should parse empty file (only comments) correctly"
        expected
        (Parse.node (head result))
