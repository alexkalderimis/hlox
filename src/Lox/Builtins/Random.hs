{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins.Random where

import Control.Monad.IO.Class
import System.Random
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM

import Lox.Syntax
import Lox.Interpreter.Types

random :: IO Object
random = Object emptyClass <$> newTVarIO (HM.fromList fields)
    where
        fields = [(Str (fnName f), LoxFn (qualifyName "Random." f)) | f <- fns]
        fns = [callable "int" (randomIO :: IO Int)
              ,callable "float" (randomIO :: IO Double)
              ,callable "char" (T.singleton <$> randomIO)
              ,callable "bool" (randomIO :: IO Bool)
              ,BuiltIn "range" (== 2) randomRange
              ]

randomRange :: NativeFn
randomRange [LoxInt a, LoxInt b] = LoxInt <$> liftIO (randomRIO (a, b))
randomRange [LoxNum a, LoxNum b] = LoxDbl <$> liftIO (randomRIO (a, b))
randomRange [Txt a,    Txt b] = do
    let a' = T.unpack a
        b' = T.unpack b
    case (a', b') of
      ([from], [to]) -> Txt . T.singleton <$> liftIO (randomRIO (from, to))
      _ -> throwLox $ LoxError "Arguments must be single characters"
randomRange args = argumentError ["Number|String", "Number|String"] args
