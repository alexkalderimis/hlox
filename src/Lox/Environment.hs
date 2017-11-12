{-# LANGUAGE ViewPatterns #-}
module Lox.Environment where

import Prelude hiding (lookup)

import Control.Arrow (second)
import Data.Traversable
import Data.IORef
import Data.Monoid
import Data.Foldable (foldMap)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

-- Environments are persistent linked mappings of mutable values
type Ref a = IORef (Maybe a)
type Scope k a = HashMap k (Ref a)
newtype Environment k a = Environment { scopes :: [Scope k a] }

readEnv :: (Eq k, Hashable k) => Environment k a -> IO (HashMap k a)
readEnv = fmap HM.unions . mapM unScope . scopes
    where unScope = fmap (HM.mapMaybe id) . sequence . fmap deref

instance Monoid (Environment k v) where
    mempty = Environment []
    mappend a b = Environment (scopes a <> scopes b)

writeRef :: Ref a -> a -> IO ()
writeRef ref a = writeIORef ref (Just a)

emptyRef :: IO (Ref a)
emptyRef = newIORef Nothing

deref :: Ref a -> IO (Maybe a)
deref = readIORef

-- Declare a variable in the current scope
-- Previous bindings are dropped by the insert
declare :: (Hashable k, Eq k) => k -> Environment k v -> IO (Environment k v)
declare k (Environment [])     = Environment . return        . HM.singleton k <$> emptyRef
declare k (Environment (m:ms)) = Environment . (:ms) . ($ m) . HM.insert k <$> emptyRef

-- define sets the value of an existing binding, returning
-- True iff the bounding was found.
assign :: (Eq k, Hashable k) => k -> v -> Environment k v -> IO Bool
assign k v env = case resolve k env of
    Nothing  -> return False
    Just ref -> True <$ writeRef ref v

resolve :: (Hashable k, Eq k) => k -> Environment k v -> Maybe (Ref v)
resolve k = resolve' . scopes
    where
        resolve' [] = Nothing
        resolve' (m:ms) = maybe (resolve' ms) return $ HM.lookup k m

-- start a new empty scope
enterScope :: Environment k v -> Environment k v
enterScope (scopes -> ms) = Environment (HM.empty : ms)

-- more efficient than mapM (declare >> assign) as this assigns
-- everthing to the same scope
enterScopeWith :: (Eq k, Hashable k) => [(k, v)] -> Environment k v -> IO (Environment k v)
enterScopeWith vals (scopes -> ms) = do
    vals' <- mapM (sequence . second (newIORef . Just)) vals
    return (Environment (HM.fromList vals' : ms))

inCurrentScope :: (Hashable k, Eq k) => k -> Environment k v -> Bool
inCurrentScope _ (Environment []) = False
inCurrentScope k (Environment (m:_)) = HM.member k m

boundNames :: (Hashable k, Eq k) => Environment k v -> HashSet k
boundNames = foldMap (HS.fromList . HM.keys) . scopes
