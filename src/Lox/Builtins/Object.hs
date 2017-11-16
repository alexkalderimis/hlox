{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins.Object (baseClass) where

import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import qualified Data.HashMap.Strict as HM

import Lox.Syntax
import Lox.Interpreter (bindThis)
import Lox.Interpreter.Types (LoxT, runLoxT, interpreter)
import qualified Lox.Core.Array as A

-- the Gettable protocol is handled by the interpreter directly, in order
-- to also resolve methods.
baseClass :: Class
baseClass = emptyClass
    { className = "Object"
    , classId = (unsafeSingleton ())
    , methods = HM.fromList
                [("keys", (BuiltIn "Object::keys" (== 1) objectKeys))
                ,("entries", (BuiltIn "Object::entries" (== 1) objectEntries))
                ]
    , protocols = HM.fromList
                  [ ( Settable, BuiltIn "[] =" (== 3) setField )
                  , ( Gettable, BuiltIn "[]" (== 2) getField )
                  , ( Iterable, BuiltIn "__iter" (== 1) iterator )
                  ]
    }

objectKeys :: NativeFn

objectKeys [LoxObj o] = liftIO $ do
    hm <- atomically . readTVar $ objectFields o
    vs <- AtomArray <$> A.fromList (fmap LoxString $ HM.keys hm)
    return (Right $ LoxArray vs)

objectEntries :: NativeFn

objectEntries [LoxObj o] = do
    es <- HM.toList <$> fields o
    vs <- atomArray <$> (mapM pair es >>= A.fromList)
    return (Right vs)
    where
        atomArray = LoxArray . AtomArray
        pair (k, v) =  atomArray <$> A.fromList [LoxString k, v]

iterator :: NativeFn

iterator [LoxObj o] = do
    keys <- fmap LoxString . HM.keys <$> fields o
    let next []     = return $ Right (Nothing, [])
        next (k:ks) = return $ Right (Just k, ks)
        it = Stepper keys next
    return $ Right $ LoxIter it

getField :: NativeFn

getField [LoxObj inst@Object{..}, LoxString k] = do
    hm <- fields inst
    case HM.lookup k hm of
      Nothing -> getMethod objectClass inst k
      Just v -> return (Right v)

getMethod :: Class -> Object -> VarName -> LoxResult LoxVal
getMethod cls inst k = do
    case HM.lookup k (methods cls) of
      Nothing -> case superClass cls of
                   Nothing -> fieldNotFound
                   Just sup -> getMethod sup inst k
      Just fn -> run (LoxFn <$> bindThis (LoxObj inst) fn)
    where
        fieldNotFound = return . Left $ FieldNotFound Unlocated k

setField :: NativeFn

setField [LoxObj Object{..}, LoxString k, v] =
    Right v <$ (atomically $ modifyTVar' objectFields (HM.insert k v))

setField args = argumentError ["Object", "String", "Any"] args

run :: LoxT LoxVal -> LoxResult LoxVal
run lox = interpreter [] mempty >>= runLoxT lox

fields :: Object -> IO (HM.HashMap VarName LoxVal)
fields = atomically . readTVar . objectFields
