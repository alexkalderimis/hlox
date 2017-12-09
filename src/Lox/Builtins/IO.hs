{-# LANGUAGE OverloadedStrings #-}
module Lox.Builtins.IO where

import Data.Typeable (Typeable, cast)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
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
                             [(Gettable, callable "[]" getHandleMethod)
                             ,(Iterable, callable "__iter__" iterFile)
                             ]
               }

writer :: Class
writer = Class { className = "Writer"
               , classId = unsafeSingleton ()
               , superClass = Nothing
               , initializer = Just (callable "Writer::init" initHandle)
               , staticMethods = mempty
               , methods = writerMethods
               , protocols = HM.singleton Gettable $ callable "[]" (getMethod writerMethods)
               }

bihandle :: Class
bihandle = Class { className = "ReadWrite"
               , classId = unsafeSingleton ()
               , superClass = Just handle
               , initializer = Just (callable "Writer::init" initHandle)
               , staticMethods = mempty
               , methods = writerMethods
               , protocols = protocols handle
               }

getHandleMethod :: LoxVal -> Atom -> LoxM LoxVal
getHandleMethod this@(NativeObj (HSObj cls _)) name
  = maybe (throwLox $ FieldNotFound name) (fmap LoxFn)
          (objectMethod cls this name)
getHandleMethod x _ = throwLox $ TypeError "Handle" x

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
        flds = (Str "stdin",  NativeObj (HSObj handle (applyHandleFn stdin)))
             : (Str "stdout", NativeObj (HSObj writer (applyHandleFn stdout)))
             : (Str "stderr", NativeObj (HSObj writer (applyHandleFn stderr)))
             : [(Str (fnName f), LoxFn (qualifyName "IO." f)) | f <- fns]
        fns = [callable "readFile" readFile'
              ,callable "gets" readLn'
              ,callable "put" T.putStr
              ,callable "warn" warn
              ,BuiltIn "openFile" (\n -> n == 1 || n == 2) lopenFile
              ]

lopenFile :: [LoxVal] -> LoxM LoxVal
lopenFile [fn] = lopenFile [fn, LoxNil]
lopenFile [Txt fn, x] = do
    (iomode, cls) <- case x of
        Txt "r"  -> return (ReadMode, handle)
        Txt "w"  -> return (WriteMode, writer)
        Txt "a"  -> return (AppendMode, writer)
        Txt "rw" -> return (ReadWriteMode, bihandle)
        LoxNil   -> return (ReadMode, handle)
        Txt mode -> loxError ("Unknown file mode: " <> mode)
        _        -> throwLox (TypeError "String" x)
    h <- liftIO $ openFile (T.unpack fn) iomode
    return $ NativeObj $ HSObj cls (applyHandleFn h)
lopenFile args = throwLox (ArgumentError "openFile" ["String", "String?"] args)

applyHandleFn :: (Typeable a, Typeable b) => Handle -> (a -> b) -> Maybe b
applyHandleFn h f = case cast f of
    Just g -> Just (g $! h)
    _      -> Nothing

handleMethods :: Methods
handleMethods = HM.fromList
  [ ("readLine", callable "Handle::readLine" readLineH)
  , ("close", callable "Handle::close" closeH)
  ]

writerMethods :: Methods
writerMethods = HM.fromList
  [ ("write", callable "Writer::write" $ writeH T.hPutStr)
  , ("writeLn", callable "Writer::writeLn" $ writeH T.hPutStrLn)
  , ("close", callable "Handle::close" closeH)
  , ("unbuffer", callable "Handle::unbuffer" unbuffH)
  ]

unbuffH :: LoxVal -> LoxM ()
unbuffH (NativeObj (HSObj _ call)) = do
    case call (flip hSetBuffering NoBuffering) of
      Just act -> liftIO act
      Nothing -> loxError "Not a handle"
unbuffH x = throwLox (TypeError "Handle" x)

writeH :: (Handle -> Text -> IO ()) -> LoxVal -> Text -> LoxM ()
writeH write this txt = case this of
    NativeObj (HSObj _ call) -> maybe typeError liftIO $ call (flip write txt)
    _                        -> typeError
    where
      typeError = throwLox (TypeError "Writer" this)

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
