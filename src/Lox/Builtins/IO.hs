{-# LANGUAGE OverloadedStrings #-}
module Lox.Builtins.IO where

import Data.Typeable (Typeable, cast)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM
import System.IO
import Control.Monad.IO.Class

import Lox.Syntax (Atom(Str))
import Lox.Interpreter.Types
import Lox.Interpreter (stringify)

handle :: Class
handle = Class { className = "Handle"
               , classId = unsafeSingleton ()
               , superClass = Nothing
               , initializer = Just (callable "Handle::init" initHandle)
               , staticMethods = mempty
               , methods = handleMethods
               , protocols = HM.fromList
                             [(Gettable, callable "[]" (getMethod handleMethods))
                             ,(Iterable, callable "__iter__" iterFile)
                             ]
               }

initHandle :: LoxM ()
initHandle = loxError "Cannot construct Handle"

iterFile :: LoxVal -> LoxM Stepper
iterFile (NativeObj (HSObj _ call)) =
    case call id of
      Just h -> return (Stepper (Just h) nextLine)
      Nothing -> loxError "Not a handle"
    where
        nextLine :: Maybe Handle -> LoxM (Maybe LoxVal, Maybe Handle)
        nextLine Nothing = return (Nothing, Nothing)
        nextLine (Just h) = do
            eof <- liftIO (hIsEOF h)
            if eof
               then liftIO (hClose h) >> return (Nothing, Nothing)
               else do line <- liftIO (T.hGetLine h)
                       return (Just (Txt line), Just h)

iterFile x = throwLox (TypeError "Handle" x)

object :: IO Object
object = Object emptyClass <$> newTVarIO (HM.fromList flds)
    where
        flds = (Str "stdin", NativeObj (HSObj handle (applyHandleFn stdin)))
             : [(Str (fnName f), LoxFn (qualifyName "IO." f)) | f <- fns]
        fns = [callable "readFile" readFile'
              ,callable "gets" readLn'
              ,callable "put" T.putStr
              ,callable "warn" warn
              ,callable "openFile" lopenFile
              ]

lopenFile :: Text -> LoxM LoxVal
lopenFile fn = do
    h <- liftIO $ openFile (T.unpack fn) ReadMode
    return $ NativeObj $ HSObj handle (applyHandleFn h)

applyHandleFn :: (Typeable a, Typeable b) => Handle -> (a -> b) -> Maybe b
applyHandleFn h f = case cast f of
    Just g -> Just (g $! h)
    _      -> Nothing

handleMethods :: Methods
handleMethods = HM.fromList
  [ ("readLine", callable "Handle::readLine" readLineH)
  , ("close", callable "Handle::close" closeH)
  ]

readLineH :: LoxVal -> LoxM Text
readLineH (NativeObj (HSObj _ call)) = do
    case call T.hGetLine of
      Just act -> liftIO act
      Nothing -> loxError "Not a handle"
readLineH x = throwLox (TypeError "Handle" x)

closeH :: LoxVal -> LoxM ()
closeH (NativeObj (HSObj _ call)) = do
    case call hClose of
      Just io -> liftIO io
      Nothing -> loxError "Not a handle"
closeH x = throwLox (TypeError "Handle" x)

warn :: LoxVal -> LoxM ()
warn val = stringify val >>= liftIO . T.hPutStrLn stderr

readFile' :: Text -> IO Text
readFile' fn = T.readFile (T.unpack fn)

readLn' :: IO Text
readLn' =  T.getLine
