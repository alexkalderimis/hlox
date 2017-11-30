{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins.String (string) where

import Control.Exception (throwIO)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import Lox.Syntax
import Lox.Interpreter (bindThis)
import Lox.Interpreter.Types (
    NativeFn, LoxException(..), Callable(..), Stepper(..), Class(..), Protocol(..),
    LoxVal(..), pattern LoxNil, pattern LoxInt, pattern LoxDbl, pattern Txt,
    callable, natively, atomArray, throwLox, unsafeSingleton, emptyClass, argumentError)

string :: Class
string = emptyClass { className = "String"
                   , classId = unsafeSingleton ()
                   , methods = HM.fromList stringMethods
                   , staticMethods = HM.fromList statics
                   , protocols = HM.fromList
                                 [ (Gettable, BuiltIn "[]" (== 2) get)
                                 , (Iterable, callable "__iter" iterator)
                                 ]
                   }

statics :: [(VarName, Callable)]
statics = []

stringMethods :: [(VarName, Callable)]
stringMethods = [(n, BuiltIn ("String::" <> n) a f) | (BuiltIn n a f) <- fns]
    where
        fns = [ natively "upper" T.toUpper
              , natively "lower" T.toLower
              , callable "slice" slice
              , BuiltIn "split" (\n -> 0 < n && n < 3) split
              ]

get :: NativeFn

get [s, LoxDbl i] = get [s, LoxInt (round i)]

get [Txt t, LoxInt i] | 0 <= i && i < T.length t =
    return . Txt . T.take 1 . T.drop i $ t

get [_, LoxInt _]
  = throwLox $ LoxError "character index out of bounds"

get [this, Txt k] =
    case lookup k stringMethods of
      Nothing -> throwLox $ FieldNotFound (Str k)
      Just fn -> LoxFn <$> bindThis this fn

get args = argumentError ["String", "Number"] args

-- iterator takes a read-only snapshot of the array
-- to avoid issues due to modification.
iterator :: Text -> LoxVal
iterator t = LoxIter $ Stepper t iterStr

iterStr :: Monad m => Text -> m (Maybe LoxVal, Text)
iterStr t = return $ case T.uncons t of
              Nothing -> (Nothing, mempty)
              Just (c, t') -> (Just (Txt $ T.singleton c), t')

slice :: Text -> Int -> Int -> IO Text
slice t i j
  | 0 <= i && j < T.length t = return . T.take (j - i) $ T.drop i t
  | otherwise                = throwIO outofBounds
  where
      outofBounds = LoxError "Indices out of bounds"

split :: NativeFn
split [Txt t]         = split [Txt t, Txt ""]
split [Txt t, LoxNil] = split [Txt t, Txt ""]

split [Txt t, Txt ""] -- split into characters
  = atomArray $ Txt . T.singleton <$> T.unpack t

split [Txt t, Txt t'] -- split on separator
  = atomArray $ Txt <$> T.splitOn t' t

split [Txt t, LoxInt n] -- split into chunks of size n
  = atomArray $ Txt <$> T.chunksOf n t

split args = argumentError ["String", "String"] args
