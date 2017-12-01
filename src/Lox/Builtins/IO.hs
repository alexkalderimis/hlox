{-# LANGUAGE OverloadedStrings #-}
module Lox.Builtins.IO where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM
import System.IO
import Control.Monad.IO.Class

import Lox.Syntax (Atom(Str))
import Lox.Interpreter.Types
import Lox.Interpreter (stringify)

object :: IO Object
object = Object emptyClass <$> newTVarIO (HM.fromList flds)
    where
        flds = [(Str (fnName f), LoxFn (qualifyName "IO." f)) | f <- fns]
        fns = [callable "readFile" readFile'
              ,callable "gets" readLn'
              ,callable "put" T.putStr
              ,callable "warn" warn
              ]

warn :: LoxVal -> LoxM ()
warn val = stringify val >>= liftIO . T.hPutStrLn stderr

readFile' :: Text -> IO Text
readFile' fn = T.readFile (T.unpack fn)

readLn' :: IO Text
readLn' =  T.getLine
