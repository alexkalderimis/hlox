{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins.Thread where

import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (void)
import Control.Monad.State.Strict (get, liftIO)

import Lox.Syntax (Atom(Str))
import Lox.Interpreter (apply)
import Lox.Interpreter.Types (
    LoxM, LoxVal(LoxFn), Object(..), emptyClass, runLox, Callable(..),
    callable)

object :: IO Object
object = Object emptyClass <$> newTVarIO (HM.fromList flds)
    where
        flds = [(Str "run", LoxFn $ callable "Thread.run" runThread)
               ,(Str "sleep", LoxFn $ callable "Thread.sleep" sleep)
               ]

runThread :: Callable -> LoxM ()
runThread fn = get >>= fork . runLox (apply fn [])
    where
        fork = liftIO . void . forkIO . void

sleep :: Double -> IO ()
sleep n = threadDelay (floor $ n * 1e6)
