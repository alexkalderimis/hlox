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
random = Object emptyClass <$> newTVarIO (HM.fromList flds)
    where
        flds = [fn "int" (== 0) randomInt
               ,fn "float" (== 0) randomFloat
               ,fn "char" (== 0) randomChar
               ,fn "range" (== 2) randomRange
               ]
        fn name arity f = (name, LoxFn $ BuiltIn ("Random." <> name) arity f)

randomInt :: NativeFn
randomInt _ = Right . LoxInt <$> randomIO

randomFloat :: NativeFn
randomFloat _ = Right . LoxDbl <$> randomIO

randomChar :: NativeFn
randomChar _ = Right . LoxString . T.singleton <$> randomIO

randomRange :: NativeFn
randomRange [LoxInt a, LoxInt b] = Right . LoxInt <$> randomRIO (a, b)
randomRange [LoxDbl a, LoxDbl b] = Right . LoxDbl <$> randomRIO (a, b)
randomRange [LoxInt a, LoxDbl b] = Right . LoxDbl <$> randomRIO (fromIntegral a, b)
randomRange [LoxDbl a, LoxInt b] = Right . LoxDbl <$> randomRIO (a, fromIntegral b)
randomRange [LoxString a, LoxString b] = do
    let a' = T.unpack a
        b' = T.unpack b
    case (a', b') of
      ([from], [to]) -> Right . LoxString . T.singleton <$> randomRIO (from, to)
      _ -> return . Left $ LoxError NativeCode "Arguments must be single characters"
