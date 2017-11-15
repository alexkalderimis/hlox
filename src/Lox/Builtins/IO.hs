{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lox.Builtins.IO where

import Control.Exception
import Data.Monoid
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Control.Monad.IO.Class (MonadIO, liftIO)

import Lox.Syntax hiding (arity)

object :: IO Object
object = Object emptyClass <$> newIORef (HM.fromList flds)
    where
        flds = [fn "readFile" (== 1) readFile'
               ,fn "gets" (== 0) readLn'
               ]
        fn name arity f = (name, LoxFn $ BuiltIn ("IO." <> name) arity f)

readFile' :: NativeFn
readFile' [LoxString fn] = do
    content <- callIO (T.readFile (T.unpack fn))
    return (fmap LoxString content)

readLn' :: NativeFn
readLn' [] = do
    content <- callIO T.getLine
    return (fmap LoxString content)

callIO :: IO a -> LoxResult a
callIO io = do
    r <- liftIO $ (Right <$> io) `catch` \ (ex :: IOException) -> return (Left ex)
    case r of
      Right r -> return (Right r)
      Left e -> return (Left $ LoxError NativeCode (show e))
