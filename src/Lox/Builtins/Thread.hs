{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins.Thread where

import Data.Monoid
import System.Random
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.STM
import Control.Concurrent

import Lox.Syntax hiding (arity)
import Lox.Interpreter (apply)
import Lox.Interpreter.Types (LoxT, runLoxT, interpreter)
import qualified Lox.Builtins.Object as O

object :: IO Object
object = Object emptyClass <$> newTVarIO (HM.fromList flds)
    where
        flds = [fn "run" (== 1) runThread
               ]
        fn name arity f = (Str name, LoxFn $ BuiltIn ("Random." <> name) arity f)

runThread :: NativeFn
runThread [LoxFn fn] = do
    forkIO (run (apply NativeCode fn []) >> return ())
    return (Right LoxNil)

run :: LoxT LoxVal -> LoxResult LoxVal
run lox = interpreter [] mempty >>= runLoxT lox
