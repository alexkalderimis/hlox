{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins.Random where

import System.Random
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import Lox.Syntax hiding (arity)
import qualified Lox.Builtins.Object as O

random :: Class
random = emptyClass
    { className = "Random"
    , classId = unsafeSingleton ()
    , superClass = Just O.baseClass
    , staticMethods = HM.fromList
                      [("int", BuiltIn "Random.int" arity  randomInt)
                      ,("float", BuiltIn "Random.float" arity randomFloat)
                      ,("char", BuiltIn "Random.char" arity randomChar)
                      ]
    }

arity :: Int -> Bool
arity = (`elem` [0, 2])

randomInt :: NativeFn

randomInt [] = do
    i <- randomIO
    return . Right . LoxNum $ realToFrac (i :: Int)

randomInt [LoxNum a, LoxNum b] = do
    i <- randomRIO (floor a, floor b)
    return . Right . LoxNum $ realToFrac (i :: Int)

randomFloat :: NativeFn

randomFloat [] = do
    i <- randomIO
    return . Right . LoxNum $ realToFrac (i :: Double)

randomFloat [LoxNum a, LoxNum b] = do
    i <- randomRIO (realToFrac a, realToFrac b)
    return . Right . LoxNum $ realToFrac (i :: Double)

randomChar :: NativeFn

randomChar [] = do
    c <- randomIO
    return . Right . LoxString $ T.singleton c

randomChar [LoxString a, LoxString b] = do
    let a' = T.unpack a
        b' = T.unpack b
    case (a', b') of
      ([from], [to]) -> Right . LoxString . T.singleton <$> randomRIO (from, to)
      _ -> return . Left $ LoxError NativeCode "Arguments must be single characters"

