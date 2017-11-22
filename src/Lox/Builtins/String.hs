{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins.String (string) where

import Control.Exception (throwIO)
import Data.Monoid
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import Lox.Syntax
import qualified Lox.Builtins.Object as O
import Lox.Interpreter (truthy, apply, bindThis, (<=>))
import Lox.Interpreter.Types (LoxT, runLoxT, interpreter)
import qualified Lox.Core.Array as A

string :: Class
string = emptyClass { className = "String"
                   , classId = (unsafeSingleton ())
                   , methods = HM.fromList stringMethods
                   , staticMethods = HM.fromList statics
                   , protocols = HM.fromList
                                 [ (Gettable, BuiltIn "[]" (== 2) get)
                                 , (Iterable, BuiltIn "__iter" (== 1) (natively iterator))
                                 ]
                   }

statics :: [(VarName, Callable)]
statics = []

stringMethods :: [(VarName, Callable)]
stringMethods = [(n, BuiltIn ("String::" <> n) a f) | (BuiltIn n a f) <- fns]
    where
        fns = [ single "upper" (natively T.toUpper)
              , single "lower" (natively T.toLower)
              , triple "slice" (toNativeFn slice)
              , double "split" split
              ]
        triple n = BuiltIn n (== 3)
        double n = BuiltIn n (\n -> n == 2 || n == 1)
        single n = BuiltIn n (== 1)

get :: NativeFn

get [s, LoxDbl i] = get [s, LoxInt (round i)]

get [Txt t, LoxInt i] | 0 <= i && i < T.length t =
    return . Right . Txt . T.take 1 . T.drop i $ t

get [this, LoxString k] = do
    case HM.lookup k (methods string) of
      Nothing -> return . Left $ FieldNotFound Unlocated (Str k)
      Just fn -> run (LoxFn <$> bindThis this fn)

get [_, LoxInt i]
    = return . Left . LoxError NativeCode $ "character index out of bounds"

get args = argumentError ["String", "Number"] args

-- iterator takes a read-only snapshot of the array
-- to avoid issues due to modification.
iterator :: Text -> LoxVal
iterator t = LoxIter $ Stepper t iterStr

iterStr :: Text -> LoxResult (Maybe LoxVal, Text)
iterStr t = return . Right $ case T.uncons t of
              Nothing -> (Nothing, mempty)
              Just (c, t') -> (Just (Txt $ T.singleton c), t')

slice :: Text -> Int -> Int -> IO Text
slice t i j
  | 0 <= i && j < T.length t = return . T.take (j - i) $ T.drop i t
  | otherwise                = throwIO outofBounds
  where
      outofBounds :: LoxException
      outofBounds = LoxError NativeCode "Indices out of bounds"

split :: NativeFn
split [Txt t]         = split [Txt t, Txt ""]
split [Txt t, LoxNil] = split [Txt t, Txt ""]

split [Txt t, Txt ""] -- split into characters
  = Right . LoxArray . AtomArray <$> A.fromList (fmap (Txt . T.singleton) $ T.unpack t)

split [Txt t, Txt t'] -- split on separator
  = Right . LoxArray . AtomArray <$> A.fromList (fmap Txt $ T.splitOn t' t)

split [Txt t, LoxInt n] -- split into chunks of size n
  = Right . LoxArray . AtomArray <$> A.fromList (fmap Txt $ T.chunksOf n t)

run :: LoxT LoxVal -> LoxResult LoxVal
run lox = interpreter [] mempty >>= runLoxT lox

