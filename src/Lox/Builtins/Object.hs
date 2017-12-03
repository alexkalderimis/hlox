{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins.Object (baseClass, getObjectMethod, objectEntries) where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HM

import Lox.Syntax
import Lox.Interpreter.Types (
    Class(..), AtomArray(..), Stepper(..), Object(..), LoxException(..), Protocol(..),
    LoxVal(..), LoxM,
    throwLox, unsafeSingleton, callable, emptyClass, objectMethod
    )
import qualified Lox.Core.Array as A

-- the Gettable protocol is handled by the interpreter directly, in order
-- to also resolve methods.
baseClass :: Class
baseClass = emptyClass
    { className = "Object"
    , classId = unsafeSingleton ()
    , methods = HM.fromList
                [("keys", callable "Object::keys" objectKeys)
                ,("entries", callable "Object::entries" objectEntries)
                ]
    , protocols = HM.fromList
                  [ ( Settable, callable "[] =" setField )
                  , ( Gettable, callable "[]" getField )
                  , ( Iterable, callable "__iter" iterator )
                  ]
    }

objectKeys :: Object -> IO AtomArray

objectKeys o = do
    hm <- atomically . readTVar $ objectFields o
    AtomArray <$> A.fromList (LoxLit <$> HM.keys hm)

objectEntries :: Object -> IO AtomArray
objectEntries o = do
    es <- HM.toList <$> fields o
    AtomArray <$> (mapM pair es >>= A.fromList)
    where
        pair (k, v) = LoxArray . AtomArray <$> A.fromList [LoxLit k, v]

iterator :: Object -> IO Stepper
iterator o = do
    keys <- fmap LoxLit . HM.keys <$> liftIO (fields o)
    let next []     = return (Nothing, [])
        next (k:ks) = return (Just k, ks)
    return $! Stepper keys next

getField :: Object -> Atom -> LoxM LoxVal
getField inst@Object{..} key = do
    hm <- liftIO $ fields inst
    case HM.lookup key hm of
         Nothing -> getObjectMethod inst key
         Just v -> return v

setField :: Object -> Atom -> LoxVal -> IO LoxVal
setField Object{..} k v = 
    v <$ atomically (modifyTVar' objectFields $ HM.insert k v)

fields :: Object -> IO (HM.HashMap Atom LoxVal)
fields = atomically . readTVar . objectFields

getObjectMethod :: Object -> Atom -> LoxM LoxVal
getObjectMethod o k = maybe (throwLox $ FieldNotFound k) (fmap LoxFn)
                            (objectMethod o k)

