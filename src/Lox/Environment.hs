{-# LANGUAGE RecordWildCards #-}
module Lox.Environment where

import Control.Arrow (second)
import Data.Traversable
import Data.Monoid
import Data.Foldable (foldMap)
import Data.Sequence (Seq, (><), (|>))
import Data.Hashable
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HS
import Control.Concurrent.STM

-- Environments are persistent linked mappings of mutable values
type Ref a = TVar (Maybe a)

newtype Environment k a = Environment
    { envVars :: HM.HashMap k (Ref a)
    }

instance (Hashable k, Eq k) => Monoid (Environment k v) where
    mempty = Environment mempty
    (Environment vs) `mappend` (Environment vs') =
        Environment (HM.union vs' vs)

readEnv :: Environment k a -> IO (HashMap k a)
readEnv = atomically . fmap (HM.mapMaybe id) . HM.traverseWithKey f . envVars
    where f k = readTVar

writeRef :: Ref a -> a -> IO ()
writeRef ref a = atomically $ writeTVar ref (Just a)

emptyRef :: IO (Ref a)
emptyRef = newTVarIO Nothing

newRef :: a -> IO (Ref a)
newRef = newTVarIO . Just

deref :: Ref a -> IO (Maybe a)
deref = readTVarIO

-- Declare a variable in the current scope
-- Previous bindings are shadowed by the insert
declare :: (Hashable k, Eq k) => k -> Environment k v -> IO (Environment k v)
declare k Environment{..} = do
    ref <- emptyRef
    return Environment { envVars = HM.insert k ref envVars }

-- define sets the value of an existing binding, returning
-- True iff the bounding was found.
assign :: (Eq k, Hashable k) => k -> v -> Environment k v -> IO Bool
assign k v env = case resolve k env of
    Nothing  -> return False
    Just ref -> True <$ writeRef ref v

resolve :: (Hashable k, Eq k) => k -> Environment k v -> Maybe (Ref v)
resolve k = HM.lookup k . envVars

-- start a new empty scope (not required in this implementation)
enterScope :: Environment k v -> Environment k v
enterScope = id

enterScopeWith :: (Eq k, Hashable k) => [(k, v)] -> Environment k v -> IO (Environment k v)
enterScopeWith vals Environment{..} = do
    refs <- mapM (newRef . snd) vals
    let vars = foldr insertPair envVars (zip (map fst vals) refs)
    return $ Environment vars
    where 
        insertPair = uncurry HM.insert

--- no way of knowing; better to detect this via an analysis pass
inCurrentScope :: (Hashable k, Eq k) => k -> Environment k v -> Bool
inCurrentScope _ _ = False

boundNames :: (Hashable k, Eq k) => Environment k v -> HS.HashSet k
boundNames = HS.fromList . HM.keys . envVars

-- remove all names bound in the lhs
diffEnv :: (Hashable k, Eq k)
        => Environment k v -> Environment k v -> Environment k v
diffEnv lhs (Environment m) = Environment (foldr HM.delete m bound)
    where bound = boundNames lhs

