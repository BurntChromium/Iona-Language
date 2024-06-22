module Main where

import Data.Time
import System.Environment
import System.FilePath

import qualified Terminal
import Lex

main :: IO ()
main :: IO() = do
    globalStartTime <- getCurrentTime
    args <- getArgs
    -- Need at least a file to compile
    if null args then
        error (Terminal.printColor Terminal.Red "error: not enough arguments (expected a file path)")
    else
        putStrLn "> Iona compiler version 0.1"
    -- Extract arguments
    let filepath = head args
    -- Assert that we're compiling an Iona language file
    if snd (splitExtension filepath) /= ".iona" then
        error (Terminal.printColor Terminal.Red "error: received an incompatible file type (expected *.iona, received " ++ filepath)
    else
        putStrLn ("> compiling " ++ filepath)
    entryFile <- readFile filepath
    putStrLn "compiling this code: "
    putStrLn (entryFile ++ "\n")
    -- Run the lexer
    let lexerOutput = Lex.lexer Lex.initLexerState entryFile
    mapM_ print (Lex.tokens lexerOutput)
    mapM_ (putStrLn . Terminal.printError entryFile) (Lex.errors lexerOutput)
    -- Get overall program runtime
    globalStopTime <- getCurrentTime
    putStr "compilation finished in "
    print $ diffUTCTime globalStopTime globalStartTime
