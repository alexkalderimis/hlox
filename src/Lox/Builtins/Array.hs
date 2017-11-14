{-# LANGUAGE OverloadedStrings #-}

module Lox.Builtins.Array (array) where

import Data.Monoid
import Data.Maybe
import Data.String
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
                                 [ (Gettable, BuiltIn "[]" (== 2) getElement)
                                 , (Settable, BuiltIn "[] =" (== 3) setElement)
                                 , (Iterable, BuiltIn "__iter" (== 1) iterator)
                                 ]
                   }

statics :: [(VarName, Callable)]
statics = [("range", BuiltIn "Array.range" (== 2) inclusiveRange)]

arrayMethods :: [(VarName, Callable)]
arrayMethods = [(n, BuiltIn ("Array::" <> n) a f) | (BuiltIn n a f) <- fns]
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

getElement :: NativeFn
getElement [xs, LoxDbl i] = getElement [xs, LoxInt (round i)]
getElement [LoxArray (AtomArray arr), LoxInt i] | i >= 0 = do
    n <- A.size arr
    if i < n
       then Right <$> A.get i arr
       else return . Left $ FieldNotFound NativeCode (fromString $ show i)
getElement [this@LoxArray{}, LoxString k] = do
    case HM.lookup k (methods array) of
      Nothing -> return . Left $ FieldNotFound Unlocated k
      Just fn -> run (LoxFn <$> bindThis this fn)
getElement [LoxArray _, LoxInt i]
    = return . Left . LoxError NativeCode
      $ "negative array index"
getElement args = argumentError ["Array", "Number"] args

setElement :: NativeFn
setElement [xs, LoxDbl i, e] = setElement [xs, LoxInt (round i), e]
setElement [LoxArray (AtomArray arr), LoxInt i, e]
  | i >= 0    = Right e <$ A.set i e arr
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

inclusiveRange [LoxInt from, LoxInt to] | from <= to = do
    xs <- A.range from to >>= A.map (return . LoxInt)
    done xs
inclusiveRange [LoxDbl from, LoxDbl to] =
    inclusiveRange [LoxInt (round from), LoxInt (round to)]
inclusiveRange [from, LoxDbl to] =
    inclusiveRange [from, LoxInt (round to)]
inclusiveRange [LoxDbl from, to] =
    inclusiveRange [LoxInt (round from), to]
inclusiveRange [LoxInt from, LoxInt to]
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
    fromArray <$> A.map f arr
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
    return . Right . LoxInt $ fromIntegral i

inPlaceSort :: NativeFn
inPlaceSort [LoxArray (AtomArray xs)] = do
    run (A.sort (<=>) xs >> return LoxNil)

run :: LoxT LoxVal -> LoxResult LoxVal
run lox = interpreter mempty >>= runLoxT lox

inPlace :: (Array LoxVal -> IO ()) -> NativeFn
inPlace f [LoxArray (AtomArray arr)] = Right LoxNil <$ f arr
inPlace _ args = argumentError ["Array"] args

-- boilerplate removal
done :: Array LoxVal -> LoxResult LoxVal
done = return . Right . fromArray

fromArray :: Array LoxVal -> LoxVal
fromArray = LoxArray . AtomArray
