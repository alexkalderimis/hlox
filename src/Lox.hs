{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lox
    ( runFile
    , runPrompt
    ) where

import Prelude hiding (readFile, getLine, putStr, putStrLn, unwords)
import System.Console.Readline
import Data.Text hiding (foldr, null, filter)
import Data.Maybe
import System.IO (hFlush, stderr, stdout)
import System.Exit
import Data.Monoid
import Data.Text.IO (hPutStrLn, readFile, getLine, putStrLn, putStr)
import Control.Monad
import Control.Applicative
import Control.Monad.State.Strict
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Lox.Language
import Lox.Builtins (builtins)

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
      Left e  -> do parseError e
                    exitFailure
      Right p -> do env <- builtins
                    s <- interpreter env
                    res <- evalLoxT (runProgram p) s
                    case res of
                      Left e -> do putStr "[RUNTIME ERROR] "
                                   putStrLn (pack $ show e)
                                   exitFailure
                      Right _ -> return ()

runPrompt :: IO ()
runPrompt = welcome >> builtins >>= interpreter >>= go replOpts
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
              Just ":env" -> do readEnv (bindings env) >>= printBindings
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

run' :: ReplOpts -> Interpreter -> Text -> IO Interpreter
run' ReplOpts{..} intS code = do
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
      Left e  -> do parseError e
                    return intS
      Right p -> do let lox = runProgram p
                    when showResult $ putStrLn "=== OUTPUT"
                    res <- evalLoxT (runProgram p)
                                    (intS { bindings = enterScope (bindings intS) })
                    case res of
                      Left e -> do runtimeError e
                                   return intS
                      Right (v, s) -> do
                          when (not showResult && not (nil v)) $ do
                              printLox v
                          when showResult $ do
                              putStrLn "==== RESULT"
                              print v
                          when showState $ do
                              putStrLn "==== STATE"
                              readEnv (bindings s) >>= printBindings
                          when (not $ HS.null $ warnings s) $ do
                              mapM_ (putStrLn . ("[WARNING] " <>)) (warnings s)
                          recordIt v s

printBindings :: [HashMap Text Atom] -> IO ()
printBindings [] = return ()
printBindings (m:ms) = do
    putStrLn "-------------"
    forM (HM.toList m) $ \(k, v) -> do
        putStr k
        putStr " = "
        printLox v
    printBindings ms

parseError :: ParseError -> IO ()
parseError e = case e of
    NoParse       -> putStrLn "[SYNTAX ERROR] could not parse value"
    Fatal loc msg -> putE loc msg
    Recoverable loc msg -> putE loc msg
    where
        putE loc msg = putStrLn $ mconcat ["[SYNTAX ERROR] " ,niceLoc loc ," ", msg ]
        point t l c = unwords [t, ":", pack (show l), "..", pack (show c)]
        niceLoc loc = let (l@(a, b, c), r@(d, e, f)) = range loc
                       in if l == r then point a b c
                                    else point a b c <> " - " <> point d e f

runtimeError :: LoxException -> IO ()
runtimeError LoxBreak{} = return ()
runtimeError LoxContinue{} = return ()
runtimeError LoxReturn{} = return ()
runtimeError e = do putStr "[RUNTIME ERROR] "
                    putStrLn (pack $ show e)

removeEmptyScopes :: Env -> Env
removeEmptyScopes (Environment ms) = Environment (filter (not . HM.null) ms)

renameIts :: Env -> Env
renameIts (Environment ms) = Environment (go ms 0)
    where go [] _ = []
          go (m:ms) 0 = m : go ms (if HM.member "it" m then 1 else 0)
          go (m:ms) n = let k = "it" <> pack (show n)
                            oldKs = ["it", "it" <> pack (show $ pred n)]
                            mx = getFirst $ foldMap (\k -> First $ HM.lookup k m) oldKs
                            m' = maybe m (\x -> HM.insert k x (foldr HM.delete m oldKs)) mx
                            n' = maybe n (const (succ n)) mx
                        in m' : go ms n'

recordIt :: Atom -> Interpreter -> IO Interpreter
recordIt LoxNil s = return $ s { bindings = removeEmptyScopes (bindings s) }
recordIt v s = do env <- declare "it" (enterScope $ bindings s)
                  assign "it" v env
                  return s { bindings = renameIts (removeEmptyScopes env) }
