{-# LANGUAGE RecordWildCards #-}
module Lox.VectorEnv where

import Prelude hiding (lookup, (!))

import Control.Arrow (second)
import Data.Traversable
import Data.IORef
import Data.Monoid
import Data.Foldable (foldMap)
import Data.Sequence (Seq, (><), (|>))
import Data.Hashable
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Vector as V
import           Data.Vector (Vector, (!?), (!))

-- Environments are persistent linked mappings of mutable values
type Ref a = IORef (Maybe a)
data Environment k a = Environment
    { envVars :: !(Vector (Ref a))
    , envNames :: [(k, Int)] -- record the names.
    }

instance Monoid (Environment k v) where
    mempty = Environment mempty mempty
    (Environment vs ns) `mappend` (Environment vs' ns') =
        let len = V.length vs
         in Environment (vs <> vs') (map (second (+ len)) ns' <> ns)

readEnv :: (Hashable k, Eq k) => Environment k a -> IO (HashMap k a)
readEnv Environment{..}
    = fmap (HM.mapMaybe id) -- remove undefined vars
    . HM.traverseWithKey f
    . HM.map (envVars V.!)
    . HM.fromList
    $ envNames
    where f k r = deref r

writeRef :: Ref a -> a -> IO ()
writeRef ref a = writeIORef ref (Just a)

emptyRef :: IO (Ref a)
emptyRef = newIORef Nothing

newRef :: a -> IO (Ref a)
newRef = newIORef . Just

deref :: Ref a -> IO (Maybe a)
deref = readIORef

-- the structure of the code ensures that declarations are added to the right.
declare :: k -> Environment k v -> IO (Environment k v)
declare k Environment{..} = do
    ref <- emptyRef
    return $ Environment { envVars = envVars <> V.singleton ref
                         , envNames = (k, V.length envVars) : envNames
                         }

-- This implementation requires the calling code to know the index; this
-- requires precalculation.
assign :: (Eq k) => k -> v -> Environment k v -> IO Bool
assign k v env = case resolve k env of
    Nothing  -> return False
    Just ref -> True <$ writeRef ref v

assignAt :: Int -> v -> Environment k v -> IO ()
assignAt k v env = writeRef (envVars env ! k) v

-- look for a ref by name, via indirection table
resolve :: (Eq k) => k -> Environment k v -> Maybe (Ref v)
resolve k Environment{..} = L.lookup k envNames >>= (envVars !?)

-- just straight up fetch that ref
getRef :: Int -> Environment k v -> Ref v
getRef i Environment{..} = envVars ! i

-- start a new empty scope (not required in this implementation)
enterScope :: Environment k v -> Environment k v
enterScope = id

enterScopeWith :: [(k, v)] -> Environment k v -> IO (Environment k v)
enterScopeWith vals Environment{..} = do
    refs <- mapM newRef $ map snd vals
    let names = zip (map fst vals) [V.length envVars ..]
    return $ Environment { envVars = envVars <> V.fromList refs
                         , envNames = names <> envNames
                         }

--- no way of knowing; better to detect this via an analysis pass
inCurrentScope :: (Hashable k, Eq k) => k -> Environment k v -> Bool
inCurrentScope _ _ = False

boundNames :: (Hashable k, Eq k) => Environment k v -> HS.HashSet k
boundNames = HS.fromList . map fst . envNames
