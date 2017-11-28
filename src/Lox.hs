{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lox
    ( runFile
    , runPrompt
    ) where

import Prelude hiding (readFile, getLine, putStr, putStrLn, unwords)
import System.Console.Haskeline
import Data.Text hiding (foldr, null, filter)
import Data.Maybe
import System.IO (hFlush, stderr, stdout)
import System.Exit
import Data.IORef
import Data.Monoid
import Data.Text.IO (hPutStrLn, readFile, getLine, putStrLn, putStr)
import Control.Monad
import Control.Applicative
import Control.Monad.State.Strict
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Lox.Language
import Lox.Builtins (initInterpreter)

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
      Right p -> do s <- initInterpreter
                    let prog = fromParsed p
                    res <- evalLoxT (runProgram prog) s
                    case res of
                      Left e -> do putStr "[RUNTIME ERROR] "
                                   putStrLn (pack $ show e)
                                   exitFailure
                      Right _ -> return ()

runPrompt :: IO ()
runPrompt = do
    names <- newIORef ([] :: [Text])
    welcome
    s <- initInterpreter
    let settings = setComplete (completeNames names)
                 $ defaultSettings
    runInputT settings (go 0 names replOpts s)
    where
        exit = outputStrLn "Goodbye!"

        completeNames ref = completeWord Nothing [' ', '\t'] $ \s -> do
            let t = pack s
            currentNames <- liftIO (readIORef ref)
            return [simpleCompletion (unpack n)
                        | n <- currentNames
                        , t `isPrefixOf` n
                        ]
        
        go :: Int -> IORef [Text] -> ReplOpts -> Interpreter -> InputT IO ()
        go i names opts env = do
            let loop = go (i+1) names
            ml <- getInputLine ("repl:" <> show i <> " > ")
            case ml of
              Nothing -> exit
              Just ":q" -> exit
              Just ":?" -> liftIO help >> loop opts env
              Just ":t" -> loop (opts{ showTokens = not (showTokens opts) }) env
              Just ":a" -> loop (opts{ showAST = not (showAST opts) }) env
              Just ":s" -> loop (opts{ showState = not (showState opts) }) env
              Just ":r" -> loop (opts{ showResult = not (showResult opts) }) env
              Just ":env" -> do liftIO (readEnv (bindings env) >>= printBindings.return)
                                loop opts env
              Just code -> do env' <- liftIO (run' i opts env (pack code))
                              let ns = HS.toList $ boundNames (bindings env')
                              liftIO (writeIORef names ns)
                              loop opts env'

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

run' :: Int -> ReplOpts -> Interpreter -> Text -> IO Interpreter
run' i ReplOpts{..} intS code = do
    let (ts, errs) = tokensFrom i code
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
      Right p -> do let prog = fromParsed p
                        lox = runProgram prog
                    when showAST $ do
                        putStrLn "==== PROG"
                        print prog
                    when showResult $ putStrLn "=== OUTPUT"
                    res <- evalLoxT lox
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
                              readEnv (bindings s) >>= printBindings.return
                          when (not $ HS.null $ warnings s) $ do
                              mapM_ (putStrLn . ("[WARNING] " <>)) (warnings s)
                          recordIt v s

printBindings :: [HashMap Text LoxVal] -> IO ()
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
        point t l c = mconcat [t, ": (", pack (show l), ",", pack (show c), ")"]
        niceLoc loc = let (l@(a, b, c), r@(d, e, f)) = range loc
                       in if l == r then point a b c
                                    else point a b c <> " - " <> point d e f

runtimeError :: RuntimeError -> IO ()
runtimeError (RuntimeError ts e) = printError e >> printTrace ts
    where
        printError (LoxError msg) = putStrLn $ "[INTERNAL ERROR] " <> msg
        printError (FieldNotFound k) = putStrLn $ "[FIELD NOT FOUND] " <> pack (show k)
        printError (UserError val) = putStrLn $ "[ERROR] " <> pack (show val)
        printError (TypeError ty val) = do
            putStr "[TYPE ERROR]"
            putStr $ " expected " <> ty
            putStr $ " but got " <> pack (show val) <> " :: " <> typeOf val
            putStrLn ""
        printError (ArgumentError n types args) = do
            putStr "[ARGUMENT ERROR]"
            putStr $ " in " <> n <> " expected (" <> intercalate ", " (fmap pack types) <> ")"
            putStrLn $ " but got (" <> intercalate ", " (fmap typeOf args) <> ")"
        printError (CaughtEx ex) = do
            putStr "[INTERNAL ERROR] "
            putStr . pack $ show ex
            putStrLn ""

        printTrace [] = return ()
        printTrace ((was,wo):frames) = do
            putStr "  in "
            putStr was
            putStr " at "
            putStr $ niceLoc wo
            putStrLn ""
            printTrace frames

niceLoc :: Loc -> Text
niceLoc loc = case range loc of
    (l, r) | l == r -> place l
    (l, r) -> place l <> " - " <> place r
    where
    place (t, l, c) = t <> " (" <> pack (show l) <> "," <> pack (show c) <> ")"

recordIt :: LoxVal -> Interpreter -> IO Interpreter
recordIt LoxNil s = return s
recordIt v s = do env <- declare "it" (enterScope $ bindings s)
                  assign "it" v env
                  return s { bindings = env }
