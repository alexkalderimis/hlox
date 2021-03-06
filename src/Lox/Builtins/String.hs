{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins.String (string) where

import Control.Exception (throwIO)
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Text.RE.PCRE.Text (SearchReplace(..), (*=~), (*=~/), (?=~/))
import Text.RE.Replace (
  replaceMethods, REContext(TOP), matchedText, replaceAllCapturesM)

import Lox.Syntax
import qualified Lox.Builtins.Regex as Regex
import Lox.Interpreter.Types
import Lox.Interpreter (stringify, apply)

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
              , natively "length" T.length
              , callable "replace" replace
              , callable "replaceAll" replaceAll
              , BuiltIn "split" (\n -> 0 < n && n < 3) split
              ]

replace :: Text -> Object -> Text -> LoxM Text
replace str o str' = do
  re <- Regex.getRE o
  return (str ?=~/ SearchReplace re str')

replaceAll :: Text -> LoxVal -> LoxVal -> LoxM Text

-- replace using a text template (supporting capture references)
replaceAll str pat (Txt replacement) = do
  re <- Regex.asRegex pat
  return (str *=~/ SearchReplace re replacement)

-- replace using a match->replacement function
replaceAll str pat (LoxFn fn) = do
  re <- Regex.asRegex pat
  let f match _ _ = case matchedText match of
        Nothing -> return Nothing
        Just t -> apply fn [Txt t] >>= fmap return . stringify 
  replaceAllCapturesM replaceMethods TOP f (str *=~ re)

-- incompatible types
replaceAll _ x y = throwLox $ ArgumentError
                   "String::replaceAll" ["String|Regex", "String|Function"] [x, y]

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
  | i < 0                    = norm i >>= \i' -> slice t i' j
  | j < 0                    = norm j >>= slice t i
  | 0 <= i && j <= T.length t = return . T.take (j - i) $ T.drop i t
  | otherwise                = throwIO outofBounds
  where
      outofBounds = LoxError $ "Indices out of bounds " <> fromString (show (i,j))
      norm idx = let normed = T.length t + idx + 1
                  in if normed < 0 then throwIO outofBounds
                                   else return normed

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
