{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lox.Builtins.IO where

import Data.Bifunctor
import Control.Exception
import Data.Monoid
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)

import Lox.Syntax hiding (arity)

object :: IO Object
object = Object emptyClass <$> newTVarIO (HM.fromList flds)
    where
        flds = [fn "readFile" (== 1) readFile'
               ,fn "gets" (== 0) readLn'
               ]
        fn name arity f = (Str name, LoxFn $ BuiltIn ("IO." <> name) arity f)

readFile' :: NativeFn
readFile' [Txt fn] = do
    content <- callIO (T.readFile (T.unpack fn))
    return (fmap Txt content)

readLn' :: NativeFn
readLn' [] = do
    content <- callIO T.getLine
    return (fmap Txt content)

callIO :: IO a -> LoxResult a
callIO io = first asLoxException <$> try io
    where asLoxException :: IOException -> LoxException
          asLoxException = LoxError NativeCode . show
