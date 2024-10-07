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
