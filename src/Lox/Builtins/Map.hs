module Lox.Builtins.Map where

import Data.Maybe
import Control.Arrow ((&))
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM

import Lox.Syntax
import Lox.Builtins.Object (baseClass)
import Control.Concurrent.STM

map :: Class
map = baseClass
      { className = "Map"
      , classId = unsafeSingleton ()
      , methods = methods baseClass & HM.insert "put" (callable "put" put)
                                    & HM.insert "has" (callable "has" has)
                                    & HM.insert "get" (callable "get" get)
                                    & HM.insert "delete" (callable "delete" del)
      , protocols = protocols baseClass & HM.insert Gettable (callable "getMethod" getMethod)
                                        & HM.delete Settable
      }

put :: Object -> Atom -> LoxVal -> IO ()
put o k v = atomically $ HM.insert k v <$> readTVar (objectFields o)

has :: Object -> Atom -> IO Bool
has o k = atomically . fmap (HM.member k) . readTVar $ objectFields o

get :: Object -> Atom -> IO LoxVal
get o k = atomically $ do
    hm <- readTVar (objectFields o)
    return . fromMaybe LoxNil $ HM.lookup k hm

del :: Object -> Atom -> IO ()
del o k = atomically (HM.delete k <$> readTVar (objectFields o))

getMethod :: Object -> Text -> IO LoxVal
getMethod inst@Object{..} k = go objectClass
    where
        go cls = case HM.lookup k (methods cls) of
                   Nothing -> case superClass cls of
                                Nothing -> throwIO fieldNotFound
                                Just sup -> go sup
                   Just fn -> run (LoxFn <$> bindThis (LoxObj inst) fn)
        fieldNotFound :: LoxException
        fieldNotFound = FieldNotFound def (Txt k)

run :: LoxT LoxVal -> IO LoxVal
run lox = interpreter [] mempty >>= runLoxT lox >>= either throwIO return
