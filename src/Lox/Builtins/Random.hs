{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins.Random where

import Data.Monoid
import System.Random
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.STM

import Lox.Syntax hiding (arity)
import qualified Lox.Builtins.Object as O

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
randomRange [LoxInt a, LoxInt b] = Right . LoxInt <$> randomRIO (a, b)
randomRange [LoxNum a, LoxNum b] = Right . LoxDbl <$> randomRIO (a, b)
randomRange [Txt a,    Txt b] = do
    let a' = T.unpack a
        b' = T.unpack b
    case (a', b') of
      ([from], [to]) -> Right . LoxString . T.singleton <$> randomRIO (from, to)
      _ -> return . Left $ LoxError NativeCode "Arguments must be single characters"
