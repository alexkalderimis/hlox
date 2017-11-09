{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins where

import Data.IORef
import qualified Data.HashMap.Strict as HM
import System.IO.Unsafe (unsafePerformIO)
import System.Clock

import Lox.Syntax
import Lox.Environment (enterScopeWith)
import qualified Lox.Builtins.Array as A
import qualified Lox.Builtins.Object as O
import qualified Lox.Builtins.Random as R

builtins :: IO Env
builtins = enterScopeWith vals mempty
    where vals = classes [A.array, O.baseClass, R.random]
                 ++
                 [("clock", LoxFn (BuiltIn (== 0) clock))
                 ,("Math", LoxObj maths)
                 ]

classes :: [Class] -> [(VarName, Atom)]
classes cs = [(className c, LoxClass c) | c <- cs]

clock :: NativeFn
clock _ = fmap (Right . LoxNum . (/ 1e9) . realToFrac . toNanoSecs)
        $ getTime Realtime

maths :: Object
maths = Object O.baseClass (unsafePerformIO $ newIORef (HM.fromList flds))
    where
       flds = 
            [("pi",  LoxNum $ realToFrac pi)
            ,("sin", LoxFn (BuiltIn (== 1) (mathsFn sin)))
            ,("cos", LoxFn (BuiltIn (== 1) (mathsFn cos)))
            ,("tan", LoxFn (BuiltIn (== 1) (mathsFn tan)))
            ]
       mathsFn f [LoxNum n] = return (Right . LoxNum $ call f n)
       mathsFn _ args = return (Left $ ArgumentError NativeCode ["Number"] args)
       call f = realToFrac . f . realToFrac

