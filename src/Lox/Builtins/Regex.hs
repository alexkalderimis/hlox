{-# LANGUAGE OverloadedStrings #-}
module Lox.Builtins.Regex where

import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Data.Monoid
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Text.RE.PCRE.Text
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Lox.Syntax
import Lox.Interpreter.Types

-- we need this for dynamic dispatch
newtype TyMatches = TM { unTyMatches :: Matches Text } deriving Typeable

object :: IO Object
object = Object emptyClass <$> newTVarIO (HM.fromList flds)
    where
        flds = [(Str "Regex", LoxClass regexCls)]

regexCls :: Class
regexCls = emptyClass
  { className = "Regex"
  , classId = unsafeSingleton ()
  , methods = regexMethods
  , initializer = Just (callable "Regex::init" initRegex)
  , protocols = HM.singleton Gettable (callable "[]" $ getMethod regexMethods)
  }

matchesCls :: Class
matchesCls = emptyClass
  { className = "Matches"
  , classId = unsafeSingleton ()
  , methods = matchesMethods
  , protocols = HM.fromList
                [(Gettable, callable "[]" $ getMethod matchesMethods)
                ,(Iterable, callable "__iter__" iterMatches)
                ]
  }

regexMethods :: Methods
regexMethods = HM.fromList
  [("toString", callable "Regex::toString" reToString)
  ,("match", callable "Regex::match" reMatch)
  ]

matchesMethods :: Methods
matchesMethods = HM.fromList
  [("length", callable "Matches::length" matchesLength)]

iterMatches :: LoxVal -> LoxM Stepper
iterMatches (NativeObj (HSObj _ call)) = do
  (TM ms) <- maybe (loxError "Not a match") return (call id)
  return (Stepper (matches ms) next)
    
  where next [] = return (Nothing, [])
        next (m:ms) = return (Just (Txt m), ms)
iterMatches x = throwLox (TypeError "Matches" x)

matchesLength :: LoxVal -> LoxM Int
matchesLength (NativeObj (HSObj _ call)) =
  case call (countMatches . unTyMatches) of
    Just n -> return n
    Nothing -> loxError "Not a Matches"
matchesLength x = throwLox (TypeError "Matches" x)

reMatch :: Object -> Text -> LoxM LoxVal
reMatch o t = do
    pat <- getRE o
    let ms = t *=~ pat
    return $ case countMatches ms of
               0 -> LoxNil
               _ -> NativeObj $ HSObj matchesCls (fmap ($! TM ms) . cast)

reToString :: Object -> LoxM Text
reToString o = do
    re <- getRE o
    return $ "/" <> (T.pack $ reSource re) <> "/"

getRE :: Object -> LoxM RE
getRE o = do
    (NativeObj (HSObj _ call)) <- (HM.! "_re") <$> liftIO (atomically . readTVar $ objectFields o)
    case call id of
      Just re -> return re
      Nothing -> loxError "Not a regular expression"

initRegex :: Object -> Text -> LoxM ()
initRegex o t = do
  re <- compileRegex (T.unpack t)
  liftIO $ atomically $ modifyTVar' (objectFields o)
    $ HM.insert (Str "_re") $ NativeObj (HSObj emptyClass (fmap ($! re) . cast))
