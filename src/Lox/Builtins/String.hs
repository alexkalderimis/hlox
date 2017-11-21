{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins.String (string) where

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

string :: Class
string = emptyClass { className = "String"
                   , classId = (unsafeSingleton ())
                   , methods = HM.fromList stringMethods
                   , staticMethods = HM.fromList statics
                   , protocols = HM.fromList
                                 [ (Gettable, BuiltIn "[]" (== 2) get)
                                 , (Iterable, BuiltIn "__iter" (== 1) iterator)
                                 ]
                   }

statics :: [(VarName, Callable)]
statics = []

stringMethods :: [(VarName, Callable)]
stringMethods = []
{-
    where
        fns = [ triple "fold" foldArray
              , double "filter" filterArray
              , double "map" mapArray
              , single "reverse" (inPlace A.reverse)
              , single "sort" inPlaceSort
              , single "sorted" sortArray
              , double "push" pushArray
              , single "pop" popArray
              , single "size" arraySize
              ]
        triple n = BuiltIn n (== 3)
        double n = BuiltIn n (== 2)
        single n = BuiltIn n (== 1)
-}

-- worth noting: indexing a Text object is not very efficient.
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
iterator :: NativeFn
iterator [Txt t] = return . Right . LoxIter $ Stepper t iterStr

iterStr :: Text -> LoxResult (Maybe LoxVal, Text)
iterStr t = return . Right $ case T.uncons t of
              Nothing -> (Nothing, mempty)
              Just (c, t') -> (Just (Txt $ T.singleton c), t')

run :: LoxT LoxVal -> LoxResult LoxVal
run lox = interpreter [] mempty >>= runLoxT lox

