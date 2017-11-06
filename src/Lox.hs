{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lox
    ( runFile
    , runPrompt
    ) where

import Prelude hiding (readFile, getLine, putStr, putStrLn)
import System.Console.Readline
import Data.Text hiding (null)
import Data.Maybe
import System.IO (hFlush, stderr, stdout)
import System.Exit
import Data.Monoid
import Data.Text.IO (hPutStrLn, readFile, getLine, putStrLn, putStr)
import Control.Monad
import Control.Monad.State.Strict
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Lox.Language

data Error = Error Int Text Text
    deriving (Show)

data ReplOpts = ReplOpts
    { showTokens :: !Bool
    , showAST :: !Bool
    , showState :: !Bool
    , showResult :: !Bool
    } deriving (Show, Eq)

replOpts :: ReplOpts
replOpts = ReplOpts False False False False

runFile :: FilePath -> IO ()
runFile fileName = do
    code <- readFile fileName
    let (ts, errs) = tokens code
    when (not $ null errs) $ do
        mapM_ print errs
        exitFailure
    let parsed = runParser program (tokenStream fileName ts)
    case fst parsed of
      Left es -> do forM_ es $ \e -> putStr "[SYNTAX ERROR] " >> putStrLn (pack e)
                    exitFailure
      Right p -> do env <- builtins
                    res <- evalLoxT (runProgram p) (interpreter env)
                    case res of
                      Left e -> do putStr "[RUNTIME ERROR] "
                                   putStrLn (pack $ show e)
                                   exitFailure
                      Right _ -> return ()

runPrompt :: IO ()
runPrompt = welcome >> builtins >>= go replOpts
    where
        exit = putStrLn "Goodbye!"
        
        go opts env = do
            ml <- readline "Lox > "
            case ml of
              Nothing -> exit
              Just ":q" -> exit
              Just ":?" -> help >> go opts env
              Just ":t" -> go (opts{ showTokens = not (showTokens opts) }) env
              Just ":a" -> go (opts{ showAST = not (showAST opts) }) env
              Just ":s" -> go (opts{ showState = not (showState opts) }) env
              Just ":r" -> go (opts{ showResult = not (showResult opts) }) env
              Just ":env" -> do readEnv env >>= printBindings
                                go opts env
              Just code -> do env' <- run' opts env (pack code)
                              addHistory code
                              go opts env'

welcome :: IO ()
welcome = mapM_ putStrLn $
    "Lox REPL":
    ":? Show help message":
    []

help :: IO ()
help = mapM_ putStrLn $
    "Lox REPL":
    "Enter to evaluate":
    ":q Quit":
    ":? Show this help message":
    ":t Toggle the display of tokens":
    ":a Toggle the display of the AST":
    ":s Toggle the display of the interpreter state":
    ":r Toggle the display of the result":
    ":env Print the current environment":
    []

run' :: ReplOpts -> Env -> Text -> IO Env
run' ReplOpts{..} env code = do
    let (ts, errs) = tokens code
    when showTokens  $ do
        putStrLn "==== TOKENS"
        mapM_ print ts
        mapM_ print errs
    let parsed = runParser program (tokenStream "<repl>" ts)
    when showAST $ do
        putStrLn "==== PARSE"
        print parsed

    case fst parsed of
      Left es -> do forM_ es $ \e -> putStr "[SYNTAX ERROR] " >> putStrLn (pack e)
                    return env
      Right p -> do let lox = runProgram p
                    when showResult $ putStrLn "=== OUTPUT"
                    res <- evalLoxT (runProgram p)
                                    (interpreter $ enterScope env)
                    case res of
                      Left e -> do putStr "[RUNTIME ERROR] "
                                   putStrLn (pack $ show e)
                                   return env
                      Right (v, s) -> do
                          when (not showResult && v /= LoxNil) $ do
                              printLox v
                          when showResult $ do
                              putStrLn "==== RESULT"
                              print v
                          when showState $ do
                              putStrLn "==== STATE"
                              readEnv (bindings s) >>= printBindings
                          when (not $ HS.null $ warnings s) $ do
                              mapM_ (putStrLn . ("[WARNING] " <>)) (warnings s)
                          return (bindings s)

printBindings :: [HashMap Text Atom] -> IO ()
printBindings [] = return ()
printBindings (m:ms) = do
    putStrLn "-------------"
    forM (HM.toList m) $ \(k, v) -> do
        putStr k
        putStr " = "
        printLox v
    printBindings ms
printError :: Maybe Error -> IO ()
printError (Just (Error lineno loc msg)) = hPutStrLn stderr msg'
    where msg' = mconcat ["[line " ,pack (show lineno)
                         ,"] Error"
                         ,loc ,": " ,msg
                         ]
printError _ = return ()
