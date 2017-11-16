{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Lox.Builtins (initInterpreter) where

import Control.Concurrent (threadDelay)
import Data.IORef
import qualified Data.HashMap.Strict as HM
import System.IO.Unsafe (unsafePerformIO)
import System.Clock
import qualified Data.Vector as V
import qualified Data.Text as T

import Lox.Syntax
import Lox.SeqEnv (enterScopeWith)
import Lox.Interpreter (LoxT, Interpreter, apply, runLoxT, interpreter)
import qualified Lox.Builtins.Array as A
import qualified Lox.Builtins.Object as O
import qualified Lox.Builtins.Random as R
import qualified Lox.Builtins.IO as LoxIO

initInterpreter :: IO Interpreter
initInterpreter = do
    mods <- builtinModules
    env <- builtins
    interpreter mods env

builtinModules :: IO [(ModuleIdentifier, Object)]
builtinModules = sequenceA $ fmap sequenceA
    [(ModuleIdentifier ["math"], maths)
    ,(ModuleIdentifier ["random"], R.random)
    ,(ModuleIdentifier ["io"], LoxIO.object)
    ]

builtins :: IO Env
builtins = enterScopeWith vals mempty
    where vals = classes [A.array, O.baseClass, errorCls]
                 ++
                 [("clock", LoxFn (BuiltIn "clock" (== 0) clock))
                 ,("apply", LoxFn (BuiltIn "apply" (== 2) applyFun))
                 ,("typeof", LoxFn (BuiltIn "typeof" (== 1) typeofFn))
                 ,("sleep", LoxFn (BuiltIn "sleep" (== 1) sleep))
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

typeofFn :: NativeFn
typeofFn [x] = return . Right . LoxString . T.pack $ typeOf x

clock :: NativeFn
clock _ = fmap (Right . LoxDbl . (/ 1e9) . fromInteger . toNanoSecs)
        $ getTime Realtime

sleep :: NativeFn
sleep [LoxInt i] = Right LoxNil <$ threadDelay (i * 1000000)
sleep [LoxDbl i] = Right LoxNil <$ threadDelay (floor $ i * 1e6)

applyFun [LoxFn fn, LoxArray as] = run $ do
    vs <- readArray as
    apply Unlocated fn (V.toList vs)
applyFun args = argumentError ["Function", "Array"] args

maths :: IO Object
maths = Object O.baseClass <$> newIORef (HM.fromList flds)
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
run lox = interpreter [] mempty >>= runLoxT lox

