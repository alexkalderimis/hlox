{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins.Array (array) where

import Control.Monad.IO.Class
import Control.Exception (throwIO)
import Data.Monoid
import Data.Maybe
import Data.String
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Vector ((!?))

import Lox.Syntax
import qualified Lox.Core.Array as A
import Lox.Interpreter (apply, bindThis, (<=>))
import Lox.Interpreter.Types (
    NativeFn, Stepper(..), LoxException(..), LoxVal(..), Callable(..),
    Class(..), Protocol(..), unsafeSingleton, emptyClass, AtomArray(..), LoxM,
    pattern LoxInt, pattern LoxNum, pattern LoxDbl, pattern Txt, pattern LoxNil,
    truthy, callable, toNativeFn, readArray, throwLox)

array :: Class
array = emptyClass { className = "Array"
                   , classId = unsafeSingleton ()
                   , methods = HM.fromList arrayMethods
                   , staticMethods = HM.fromList statics
                   , protocols = HM.fromList
                                 [ (Gettable, callable "[]" getElement)
                                 , (Settable, callable "[] =" setElement)
                                 , (Iterable, callable "__iter" iterator)
                                 ]
                   }

statics :: [(VarName, Callable)]
statics = [("range", callable "Array.range" inclusiveRange)]

arrayMethods :: [(VarName, Callable)]
arrayMethods = [(n, BuiltIn ("Array::" <> n) a f) | (BuiltIn n a f) <- fns]
    where
        fns = [ callable "fold" foldArray
              , callable "filter" filterArray
              , callable "map" mapArray
              , callable "reverse" (A.reverse . unAtomArray)
              , callable "sort" (A.sort cmp . unAtomArray)
              , callable "sorted" sortArray
              , callable "push" pushArray
              , callable "pop" popArray
              , callable "size" arraySize
              ]

getElement :: AtomArray -> LoxVal -> LoxM LoxVal
getElement xs@(AtomArray arr) = go
    where
        go (LoxDbl d) = go (LoxInt $ round d)
        go (LoxInt i) | i < 0 = throwLox (LoxError "negative array index")
        go (LoxInt i) = liftIO $ do
            n <- A.size arr
            if i < n
               then A.get i arr
               else throwIO (FieldNotFound (AInt i))
        go (Txt k) =
            case lookup k arrayMethods of
              Nothing -> throwLox (FieldNotFound (Str k))
              Just fn -> LoxFn <$> bindThis (LoxArray xs) fn
        go x = throwLox (TypeError "Number|String" x)

setElement :: AtomArray -> Int -> LoxVal -> IO LoxVal
setElement (AtomArray arr) i e
  | i >= 0    = e <$ A.set i e arr
  | otherwise = throwIO (LoxError "negative array index")

-- iterator takes a read-only snapshot of the array
-- to avoid issues due to modification.
iterator :: AtomArray -> IO Stepper
iterator arr = do
    xs <- readArray arr
    let seed   = 0 :: Int
        next i = return (xs !? i, succ i)
    return $! Stepper seed next

inclusiveRange :: LoxVal -> LoxVal -> IO AtomArray
inclusiveRange (LoxInt from) (LoxInt to) | from <= to = do
    xs <- A.range from to >>= A.map (return . LoxInt)
    return (AtomArray xs)
inclusiveRange (LoxNum from) (LoxNum to)
  = inclusiveRange (LoxInt (ceiling from)) (LoxInt (floor to))
inclusiveRange (LoxInt from) (LoxInt to)
  = throwIO . LoxError
  $ "Expected (A, B) where B >= A, got " <> T.pack (show from) <> " and " <> T.pack (show to)

foldArray :: AtomArray -> LoxVal -> Callable -> LoxM LoxVal
foldArray (AtomArray arr) acc fn =
    let f m a = apply fn [m, a]
     in A.foldl f acc arr

filterArray :: AtomArray -> Callable -> LoxM AtomArray
filterArray (AtomArray arr) fn = AtomArray <$> A.filter (fmap truthy . apply fn . return) arr

mapArray :: AtomArray -> Callable -> LoxM AtomArray
mapArray (AtomArray arr) fn = AtomArray <$> A.map (apply fn . return) arr

sortArray :: AtomArray -> LoxM AtomArray
sortArray (AtomArray arr) = AtomArray <$> A.sorted cmp arr

pushArray :: AtomArray -> LoxVal -> IO ()
pushArray (AtomArray arr) x = A.push x arr

popArray :: AtomArray -> IO LoxVal
popArray (AtomArray arr) = fromMaybe LoxNil <$> A.pop arr

arraySize :: AtomArray -> IO Int
arraySize (AtomArray arr) = A.size arr

-- transform 4-valued Lox comparisons to 3-valued HS ones.
cmp :: LoxVal -> LoxVal -> LoxM Ordering
cmp a b = fmap (fromMaybe LT) (a <=> b)
