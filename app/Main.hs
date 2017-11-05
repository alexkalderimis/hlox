module Main where

import System.Environment

import Lox

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> runPrompt
    ["-h"] -> usage
    [file] -> runFile file
    _      -> usage
  where usage = putStrLn "Usage: hlox [script]"

