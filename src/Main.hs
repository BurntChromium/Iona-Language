module Main where

import ASTRefinery qualified
import Data.Text (pack)
import Data.Time
import Parse
import System.Environment
import System.FilePath
import Terminal qualified
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main :: IO () = do
  globalStartTime <- getCurrentTime
  args <- getArgs
  -- Need at least a file to compile
  if null args
    then
      error (Terminal.printColor Terminal.Red "error: not enough arguments (expected a file path)")
    else
      putStrLn "> Iona compiler version 0.1"
  -- Extract arguments
  let filepath = head args
  -- Assert that we're compiling an Iona language file
  if snd (splitExtension filepath) /= ".iona"
    then
      error (Terminal.printColor Terminal.Red "error: received an incompatible file type (expected *.iona, received " ++ filepath)
    else
      putStrLn ("> compiling " ++ filepath)
  entryFile <- readFile filepath
  putStrLn "compiling this code: "
  putStrLn (entryFile ++ "\n")
  -- Run the parser
  let parseResult = parse pIona filepath (pack entryFile)
  case parseResult of
    Left err -> putStrLn ("Parse error: " ++ errorBundlePretty err)
    Right ast -> do
      putStrLn "Parsing successful!"
      print ast
      -- Refine the AST
      let (problems, refinedAST) = ASTRefinery.refineAST ast
      print refinedAST
      let messages = map (Terminal.printError entryFile) problems
      mapM_ putStrLn messages
  -- Get overall program runtime
  globalStopTime <- getCurrentTime
  putStr "compilation finished in "
  print $ diffUTCTime globalStopTime globalStartTime
