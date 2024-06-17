module Main where

import System.Environment
import System.FilePath

import qualified Terminal

main :: IO ()
main :: IO() = do
    args <- getArgs
    -- Need at least a file to compile
    if null args then 
        error (Terminal.printColor Terminal.Red "error: not enough arguments (expected a file path)")
    else
        putStrLn "> Iona compiler version 0.1"
    let filepath = head args
    -- Assert that we're compiling an Iona language file
    if snd (splitExtension filepath) /= ".iona" then 
        error (Terminal.printColor Terminal.Red "error: received an incompatible file type (expected *.iona, received " ++ filepath)
    else
        putStrLn ("> compiling " ++ filepath)
    entryFile <- readFile filepath
    putStrLn entryFile