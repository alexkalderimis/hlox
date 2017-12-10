{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins.Thread where

import qualified Data.HashMap.Strict as HM
import Data.Typeable (Typeable, cast)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Error.Class
import Control.Monad.State.Strict (get, liftIO)
import qualified Control.Concurrent.Async as Async

import Lox.Interpreter (apply)
import Lox.Interpreter.Types

newtype RunningThread = T { unThread :: Async.Async (Either RuntimeError LoxVal) }
    deriving Typeable

object :: IO Object
object = Object emptyClass <$> newTVarIO (HM.fromList flds)
    where
        flds = [("run", LoxFn $ callable "Thread.run" runThread)
               ,("sleep", LoxFn $ callable "Thread.sleep" sleep)
               ,("await", LoxFn $ callable "Thread.await" waitFor)
               ]

threadCls :: Class
threadCls = emptyClass
    { className = "Thread"
    , classId = unsafeSingleton ()
    }

runThread :: Callable -> LoxM LoxVal
runThread fn = do
    s <- get
    a <- liftIO . Async.async $ runLox (apply fn []) s
    return . NativeObj $ HSObj threadCls (fmap ($! T a) . cast)

waitFor :: LoxVal -> LoxM LoxVal
waitFor x@(NativeObj (HSObj _ call)) =
    case call id of
      Nothing -> throwLox (TypeError "Thread" x)
      Just a -> liftIO (Async.wait (unThread a)) >>= either throwError return
waitFor x = throwLox $ TypeError "Thread" x

sleep :: Double -> IO ()
sleep n = threadDelay (floor $ n * 1e6)
