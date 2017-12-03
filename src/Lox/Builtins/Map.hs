{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins.Map where

import Data.Maybe
import Data.Monoid
import Data.Function ((&))
import Data.Text (Text)
import Data.Foldable (toList)
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import qualified Data.HashMap.Strict as HM

import Lox.Syntax
import qualified Lox.Builtins.Object as O
import Control.Concurrent.STM
import Lox.Interpreter (stringify, iterable)
import Lox.Interpreter.Types

map :: Class
map = O.baseClass
      { className = "Map"
      , superClass = Just O.baseClass
      , classId = unsafeSingleton ()
      , methods = mapMethods
      , initializer = Just (BuiltIn "init" (<= 2) (fmap toLoxVal . initMap))
      , protocols = protocols O.baseClass
                  & HM.insert Gettable (callable "[]" O.getObjectMethod)
                  & HM.delete Settable
      }

mapMethods :: Methods
mapMethods = HM.fromList [ ("put",callable "put" put)
                         , ("has",callable "has" has)
                         , ("get",callable "get" get)
                         , ("putAll", callable "putAll" putAll)
                         , ("delete",callable "delete" del)
                         , ("toString", callable "toString" toString)
                         ]

toString :: Object -> LoxM Text
toString o = do
  es <- liftIO (O.objectEntries o) >>= stringify . LoxArray
  return (className (objectClass o) <> "(" <> es <> ")")

initMap :: [LoxVal] -> LoxM ()
initMap args = case args of
  [LoxObj _]     -> return ()
  [LoxObj o, xs] -> putAll o xs
  _ -> throwLox (ArgumentError "init" ["Map", "Iterable?"] args)

putAll :: Object -> LoxVal -> LoxM ()
putAll o xs = do
    let fields = objectFields o
    s <- liftIO . atomically $ readTVar fields
    Stepper a next <- iterable xs
    loop next a `catchError` \e -> liftIO (atomically $ writeTVar fields s)
                                   >> throwError e
    where
        loop next a = do
            (mv, a') <- next a
            case mv of
                Nothing -> return ()
                Just (LoxArray ar) -> do
                    vs <- toList <$> readArray ar
                    case vs of
                      [LoxLit k, v] -> liftIO (put o k v) >> loop next a'
                      [k, _] -> throwLox (TypeError "Atom" k)
                      _ -> throwLox (ArgumentError "putAll" ["Atom", "Any"] vs)
                Just x -> throwLox (TypeError "Array" x)

put :: Object -> Atom -> LoxVal -> IO ()
put o k v = atomically $ modifyTVar' (objectFields o) (HM.insert k v)

has :: Object -> Atom -> IO Bool
has o k   = atomically $ HM.member k <$> readTVar (objectFields o)

get :: Object -> Atom -> IO LoxVal
get o k = atomically $ do
    hm <- readTVar (objectFields o)
    return . fromMaybe LoxNil $ HM.lookup k hm

del :: Object -> Atom -> IO ()
del o k = atomically $ modifyTVar' (objectFields o) (HM.delete k)
