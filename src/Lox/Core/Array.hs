{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- Lox has mutable arrays, supporting indexed reads and writes
-- 
-- Growth is biased to the right: Pushing is O(1), but unshifting is O(n).
module Lox.Core.Array where

import Prelude hiding (reverse, foldl)

import Data.Data (Typeable, Data)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.Monoid
import Data.Traversable
import Data.Default
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as Vector

-- each array carries its size, so we can resize it
-- efficiently.
-- The mutable vector is itself encapsulated in a ref so that
-- we may dynamically resize it.
data Array a = Array !(IORef Int) !(IORef (Vector.IOVector a))

empty :: Default a => IO (Array a)
empty = new 0

new :: Default a => Int -> IO (Array a)
new n = Array <$> newIORef n <*> (Vector.replicate n def >>= newIORef)

size :: Array a -> IO Int
size (Array n _) = readIORef n

readArray :: (Array a) -> IO (V.Vector a)
readArray (Array n' xs) = do
    n <- readIORef n'
    readIORef xs >>= V.freeze . Vector.slice 0 n

concat :: Array a -> Array a -> IO (Array a)
concat xs ys = do
    arr <- (<>) <$> readArray xs <*> readArray ys
    Array <$> newIORef (V.length arr)
          <*> (V.unsafeThaw arr >>= newIORef)

push :: Default a => a -> Array a -> IO ()
push a xs = do
    n <- size xs
    set n a xs

pop :: Array a -> IO (Maybe a)
pop (Array n' xs') = do
    n <- subtract 1 <$> readIORef n'
    if n > 0
       then writeIORef n' n >> (Just <$> (readIORef xs' >>= flip Vector.read n))
       else return Nothing

set :: Default a => Int -> a -> Array a -> IO ()
set i a (Array n' xs') = do
    n <- readIORef n'
    xs <- readIORef xs'
    case (i < n, i < Vector.length xs) of
      (_,    False) -> grow xs >> readIORef xs' >>= (\xs -> Vector.write xs i a)
      (True, True)  -> Vector.write xs i a
      (False, True) -> Vector.write xs i a >> writeIORef n' (i + 1)
    where
        -- double in size, set all all elements to default
        grow xs = do let n = Vector.length xs
                         m = max (if n < 1 then 16 else n) (2 * i - n)
                     bigger <- Vector.unsafeGrow xs m
                     writeIORef n' (i + 1)
                     forM [n .. n + m - 1] $ \i -> Vector.write bigger i def
                     writeIORef xs' bigger

get :: Int -> Array a -> IO a
get i (Array _ xs') = readIORef xs' >>= flip Vector.read i

range :: Int -> Int -> IO (Array Int)
range from to = do
    let n = (to - from) + 1
    a <- Vector.new n
    forM (zip [0 ..] [from .. to]) $ uncurry (Vector.write a)
    Array <$> newIORef n <*> newIORef a

-- unlike map and filter, this function is not thread-safe.
-- modifying the array during the fold could case errors when reading elements
foldl :: forall a b m . (Monad m, MonadIO m)
      => (b -> a -> m b) -> b -> (Array a) -> m b
foldl f acc (Array n' xs') = do
    n <- liftIO $ readIORef n'
    xs <- liftIO $ readIORef xs'
    let go :: b -> Int -> m b
        go b i = if i >= n
                   then return b
                   else do a  <- liftIO (Vector.read xs i)
                           b' <- f b a
                           go b' (succ i)

    go acc 0

map :: (Monad m, MonadIO m) => (a -> m b) -> Array a -> m (Array b)
map f = withVec (mapM f)

filter :: (Monad m, MonadIO m) => (a -> m Bool) -> Array a -> m (Array a)
filter f xs = withVec (V.filterM f) xs

-- create a sorted copy of an array
--
-- Rather than relying on Ord, we expect a custom monadic ordering function to
-- allow comparing Lox values, which can a) require dereferencing mutable values
-- and b) fail for incomparable items.
sorted :: forall a m. (Monad m, MonadIO m)
       => (a -> a -> m Ordering) -> (Array a) -> m (Array a)
sorted cmp xs = do
    xs' <- liftIO $ copy xs
    sort cmp xs'
    return xs'

copy :: Array a -> IO (Array a)
copy (Array n xs) = Array <$> (readIORef n >>= newIORef)
                          <*> (readIORef xs >>= Vector.clone >>= newIORef)

withVec :: forall a b m. (Monad m, MonadIO m)
        => (V.Vector a -> m (V.Vector b)) -> Array a -> m (Array b)
withVec f xs = do
    ys <- liftIO (readArray xs) >>= f
    Array <$> liftIO (newIORef $ V.length ys)
          <*> (liftIO (V.unsafeThaw ys >>= newIORef))

fromList :: Default a => [a] -> IO (Array a)
fromList xs = do
    let v = V.fromList xs
    Array <$> newIORef (V.length v) <*> (V.unsafeThaw v >>= newIORef)

-- sort the array in place
sort :: forall a m. (Monad m, MonadIO m) => (a -> a -> m Ordering) -> Array a -> m ()
sort cmp (Array n' xs') = do
    n  <- liftIO (readIORef n')
    xs <- liftIO (readIORef xs')
    quicksort cmp xs 0 (n - 1)

quicksort :: forall a m. (Monad m, MonadIO m) => (a -> a -> m Ordering) -> Vector.IOVector a -> Int -> Int -> m ()
quicksort _   _  lo hi | lo >= hi = return ()
quicksort cmp xs lo hi = do
    p <- partition lo hi
    recur lo      p
    recur (p + 1) hi

    where
        recur = quicksort cmp xs
        partition lo hi = do
            pivot <- liftIO (Vector.read xs lo)
            loop pivot (lo - 1) (hi + 1)
        loop pivot i j = do
            i' <- highest pivot i
            j' <- lowest pivot j
            if i' >= j'
               then return j'
               else liftIO (Vector.swap xs i' j') >> loop pivot i' j'
        lowest = search (subtract 1) LT
        highest = search (+1) GT
        search move continue p idx = do
            let i = move idx
            x <- liftIO (Vector.read xs i)
            ord <- p `cmp` x
            if ord == continue
               then search move continue p i
               else return i

-- reverse the array in place
reverse :: (Array a) -> IO ()
reverse (Array n' xs') = do
    n <- readIORef n'
    xs <- readIORef xs'
    go xs 0 (n - 1)

    where
        go xs i j | i >= j = return ()
        go xs i j = do
            Vector.swap xs i j
            go xs (i + 1) (j - 1)
