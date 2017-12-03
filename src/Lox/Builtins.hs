{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lox.Builtins (initInterpreter) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.State.Class
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM
import System.Clock
import Data.Monoid ((<>))
import qualified Data.Text.Read as Read
import qualified Data.Vector as V
import qualified Data.Text as T

import Lox.Syntax
import Lox.Environment (enterScopeWith)
import Lox.Interpreter (apply, stringify)
import Lox.Interpreter.Types
import qualified Lox.Builtins.Array as A
import qualified Lox.Builtins.Object as O
import qualified Lox.Builtins.Random as R
import qualified Lox.Builtins.String as S
import qualified Lox.Builtins.IO as LoxIO
import qualified Lox.Builtins.Thread as Thread
import qualified Lox.Builtins.Regex as RE
import qualified Lox.Builtins.Map as M

initInterpreter :: IO Interpreter
initInterpreter = do
    mods <- builtinModules
    env <- builtins
    interpreter mods env

builtinModules :: IO [(ModuleIdentifier, Object)]
builtinModules = traverse sequenceA
    [(ModuleIdentifier ["math"], maths)
    ,(ModuleIdentifier ["random"], R.random)
    ,(ModuleIdentifier ["io"], LoxIO.object)
    ,(ModuleIdentifier ["thread"], Thread.object)
    ,(ModuleIdentifier ["re"], RE.object)
    ]

builtins :: IO Env
builtins = enterScopeWith vals mempty
    where vals = classes [A.array, O.baseClass, errorCls, S.string, M.map]
                 ++
                 [("clock", LoxFn (callable "clock" clock))
                 ,("apply", LoxFn (callable "apply" applyFun))
                 ,("typeof", LoxFn (callable "typeof" typeofFn))
                 ,("number", LoxFn (callable "number" numberFn))
                 ,("str", LoxFn (callable "str" stringify))
                 ]

classes :: [Class] -> [(VarName, LoxVal)]
classes cs = [(className c, LoxClass c) | c <- cs]

errorCls :: Class
errorCls = emptyClass
    { className = "Error"
    , classId = unsafeSingleton ()
    , superClass = Just O.baseClass 
    , initializer = Just (callable "Error.init" errorInit)
    }

errorInit :: Object -> LoxVal -> LoxM ()
errorInit Object{..} msg = do
    frame <- gets stack
    trc <- errorTrace frame
    let props = [(Str "message", msg), (Str "stackTrace", trc)]
    liftIO $ atomically $ modifyTVar' objectFields (<> HM.fromList props)

numberFn :: LoxVal -> LoxM LoxVal
numberFn x = maybe (loxError "Not a number") return $ case x of
    LoxInt _ -> return x
    LoxDbl _ -> return  x
    Txt t -> (LoxInt <$> (fromRead $ Read.decimal t))
              <|>
             (LoxDbl <$> (fromRead $ Read.double t))
    _     -> Nothing
    where
        fromRead (Right (x,"")) = Just x
        fromRead _ = Nothing

typeofFn :: LoxVal -> LoxM T.Text
typeofFn x = return $! typeOf x

clock :: IO Double
clock = fromTime <$> getTime Realtime
    where fromTime = (/ 1e9) . fromInteger . toNanoSecs

applyFun :: Callable -> AtomArray -> LoxM LoxVal
applyFun fn as = do
    vs <- readArray as
    apply fn (V.toList vs)

maths :: IO Object
maths = Object O.baseClass <$> newTVarIO (HM.fromList flds)
    where
       flds = 
           [(Str "pi",  LoxDbl pi)
           ,(Str "sin", LoxFn (natively "Math.sin" (sin :: Double -> Double)))
           ,(Str "cos", LoxFn (natively "Math.cos" (cos :: Double -> Double)))
           ,(Str "tan", LoxFn (natively "Math.tan" (tan :: Double -> Double)))
           ]
