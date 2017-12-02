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

regexMethods :: Methods
regexMethods = HM.fromList
  []

initRegex :: Object -> Text -> LoxM ()
initRegex o t = do
  re <- compileRegex (T.unpack t)
  liftIO $ atomically $ modifyTVar' (objectFields o)
    $ HM.insert (Str "_re") $ NativeObj (HSObj emptyClass (fmap ($! re) . cast))
