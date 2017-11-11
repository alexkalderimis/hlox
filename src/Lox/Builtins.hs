{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lox.Builtins (builtins) where

import Data.IORef
import qualified Data.HashMap.Strict as HM
import System.IO.Unsafe (unsafePerformIO)
import System.Clock
import qualified Data.Vector as V

import Lox.Syntax
import Lox.Environment (enterScopeWith)
import Lox.Interpreter (LoxT, apply, runLoxT, interpreter)
import qualified Lox.Builtins.Array as A
import qualified Lox.Builtins.Object as O
import qualified Lox.Builtins.Random as R

builtins :: IO Env
builtins = enterScopeWith vals mempty
    where vals = classes [A.array, O.baseClass, R.random, errorCls]
                 ++
                 [("clock", LoxFn (BuiltIn "clock" (== 0) clock))
                 ,("apply", LoxFn (BuiltIn "apply" (== 2) applyFun))
                 ,("Math", LoxObj maths)
                 ]

classes :: [Class] -> [(VarName, LoxVal)]
classes cs = [(className c, LoxClass c) | c <- cs]

errorCls :: Class
errorCls = emptyClass
    { className = "Error"
    , classId = unsafeSingleton ()
    , superClass = Just O.baseClass 
    , initializer = Just (BuiltIn "Error.init" (== 2) setErrorMsg)
    }

setErrorMsg :: NativeFn
setErrorMsg [LoxObj Object{..}, msg] = do
    modifyIORef objectFields $ HM.insert "message" msg
    return (Right LoxNil)

clock :: NativeFn
clock _ = fmap (Right . LoxDbl . (/ 1e9) . fromInteger . toNanoSecs)
        $ getTime Realtime

applyFun [LoxFn fn, LoxArray as] = run $ do
    vs <- readArray as
    apply Unlocated fn (V.toList vs)
applyFun args = argumentError ["Function", "Array"] args

maths :: Object
maths = Object O.baseClass (unsafePerformIO $ newIORef (HM.fromList flds))
    where
       flds = 
           [("pi",  LoxDbl pi)
           ,("sin", LoxFn (BuiltIn "Math.sin" (== 1) (mathsFn sin)))
           ,("cos", LoxFn (BuiltIn "Math.cos" (== 1) (mathsFn cos)))
           ,("tan", LoxFn (BuiltIn "Math.tan" (== 1) (mathsFn tan)))
           ]
       mathsFn f [LoxDbl n] = return (Right . LoxDbl $ f n)
       mathsFn f [LoxInt n] = mathsFn f [LoxDbl $ fromIntegral n]
       mathsFn _ args = return (Left $ ArgumentError NativeCode "" ["Number"] args)

run :: LoxT LoxVal -> LoxResult LoxVal
run lox = interpreter mempty >>= runLoxT lox

