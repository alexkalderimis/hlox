{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins.Array (array) where

import Data.Monoid
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import           Data.Vector ((!?))

import Lox.Syntax
import           Lox.Core.Array (Array)
import qualified Lox.Core.Array as A
import qualified Lox.Builtins.Object as O
import Lox.Interpreter (LoxT, truthy, apply, bindThis, runLoxT, interpreter, (<=>))

array :: Class
array = emptyClass { className = "Array"
                   , classId = (unsafeSingleton ())
                   , methods = HM.fromList arrayMethods
                   , staticMethods = HM.fromList statics
                   , protocols = HM.fromList
                                 [ (Gettable, BuiltIn (== 2) getElement)
                                 , (Settable, BuiltIn (== 3) setElement)
                                 , (Iterable, BuiltIn (== 1) iterator)
                                 ]
                   }

statics :: [(VarName, Callable)]
statics = [("range", BuiltIn (== 2) inclusiveRange)]

arrayMethods :: [(VarName, Callable)]
arrayMethods =
          [("fold", triple foldArray)
          ,("filter", double filterArray)
          ,("map", double mapArray)
          ,("reverse", single (inPlace A.reverse))
          ,("sort", single inPlaceSort)
          ,("sorted", single sortArray)
          ,("push", double pushArray)
          ,("pop", single popArray)
          ,("size", single arraySize)
          ]
        where
            triple = BuiltIn (== 3)
            double = BuiltIn (== 2)
            single = BuiltIn (== 1)

getElement :: NativeFn
getElement [LoxArray (AtomArray arr), LoxNum i] | i >= 0 = do
    n <- A.size arr
    if i < realToFrac n
       then Right <$> A.get (floor i) arr
       else return . Left . LoxError NativeCode
            $ "index (" <> show (floor i) <> ") not in range"
getElement [this@LoxArray{}, LoxString k] = do
    case HM.lookup k (methods array) of
      Nothing -> return . Left $ FieldNotFound Unlocated k
      Just fn -> run (LoxFn <$> bindThis this fn)

getElement [LoxArray _, LoxNum i]
    = return . Left . LoxError NativeCode
      $ "negative array index"
getElement args = argumentError ["Array", "Number"] args

setElement :: NativeFn
setElement [LoxArray (AtomArray arr), LoxNum i, e]
  | i >= 0    = Right e <$ A.set (floor i) e arr
  | otherwise = return . Left . LoxError NativeCode
                $ "negative array index"
setElement args = argumentError ["Array", "Number", "Any"] args

-- iterator takes a read-only snapshot of the array
-- to avoid issues due to modification.
iterator :: NativeFn
iterator [LoxArray arr] = do
    xs <- readArray arr
    let seed   = (0 :: Int)
        next i = return $ Right (xs !? i, succ i)
        it = Stepper seed next
    return . Right . LoxIter $ it

inclusiveRange :: NativeFn

inclusiveRange [LoxNum from, LoxNum to] | from <= to = do
    xs <- A.range (floor from) (floor to)
          >>= A.map LoxNil (return . LoxNum . realToFrac)
    done xs

inclusiveRange [LoxNum from, LoxNum to]
    = return . Left . LoxError NativeCode
    $ "Expected (A, B) where B >= A, got " <> show from <> " and " <> show to

inclusiveRange args = argumentError ["Number", "Number"] args

foldArray :: NativeFn
foldArray [LoxArray (AtomArray arr), acc, LoxFn fn] = do
    let f m a = apply Unlocated fn [m, a]
    run $ A.foldl f acc arr
foldArray args = argumentError ["Array", "Any", "Function"] args

filterArray :: NativeFn
filterArray [LoxArray (AtomArray arr), LoxFn fn] = run $ do
    let f a = truthy <$> apply Unlocated fn [a]
    fromArray <$> A.filter f arr
filterArray args = argumentError ["Array", "Function"] args

mapArray :: NativeFn
mapArray [LoxArray (AtomArray arr), LoxFn fn] = run $ do
    let f a = apply Unlocated fn [a]
    fromArray <$> A.map LoxNil f arr
mapArray args = argumentError ["Array", "Function"] args

sortArray :: NativeFn
sortArray [LoxArray (AtomArray arr)] = run $ do
    fromArray <$> A.sorted (<=>) arr
sortArray args = argumentError ["Array"] args

pushArray :: NativeFn
pushArray [LoxArray (AtomArray arr), x] = Right LoxNil <$ A.push x arr
pushArray args = argumentError ["Array", "Any"] args

popArray :: NativeFn
popArray [LoxArray (AtomArray arr)] = do
    v <- fromMaybe LoxNil <$> A.pop arr
    return (Right v)

arraySize :: NativeFn
arraySize [LoxArray (AtomArray arr)] = do
    i <- A.size arr
    return . Right . LoxNum $ fromIntegral i

inPlaceSort :: NativeFn
inPlaceSort [LoxArray (AtomArray xs)] = do
    run (A.sort (<=>) xs >> return LoxNil)

run :: LoxT Atom -> LoxResult Atom
run lox = interpreter mempty >>= runLoxT lox

inPlace :: (Array Atom -> IO ()) -> NativeFn
inPlace f [LoxArray (AtomArray arr)] = Right LoxNil <$ f arr
inPlace _ args = argumentError ["Array"] args

-- boilerplate removal
done :: Array Atom -> LoxResult Atom
done = return . Right . fromArray

fromArray :: Array Atom -> Atom
fromArray = LoxArray . AtomArray
