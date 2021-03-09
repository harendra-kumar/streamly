-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Top
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Top level IsStream module that can use all other lower level IsStream
-- modules.

module Streamly.Internal.Data.Stream.IsStream.Top
    (
    -- * Transformation
    -- ** Sampling
    -- | Value agnostic filtering.
      sampleFromThen
    , sampleIntervalStart
    , sampleIntervalEnd
    , sampleBurstStart
    , sampleBurstEnd

    -- ** Reordering
    , sortBy

    -- * Nesting
    -- ** Set like operations
    -- | These are not exactly set operations because streams are not
    -- necessarily sets, they may have duplicated elements.
    , intersectBy
    , mergeIntersectBy
    , differenceBy
    , mergeDifferenceBy
    , unionBy
    , mergeUnionBy

    -- ** Join operations
    , crossJoin
    , innerJoin
    , mergeInnerJoin
    , hashInnerJoin
    , leftJoin
    , mergeLeftJoin
    , hashLeftJoin
    , outerJoin
    , mergeOuterJoin
    , hashOuterJoin
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (get, put)
-- import Data.Hashable (Hashable)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Kind (Type)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Streamly.Internal.Data.SVar (MonadAsync)
import Streamly.Internal.Data.Stream.IsStream.Common (concatM)
import Streamly.Internal.Data.Stream.Prelude (foldl', fromList)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Stream.StreamK (IsStream)
import Streamly.Internal.Data.Time.Units (NanoSecond64(..), toRelTime64)

import qualified Data.List as List
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream.Lift as Stream
import qualified Streamly.Internal.Data.Stream.IsStream.Eliminate as Stream
import qualified Streamly.Internal.Data.Stream.IsStream.Generate as Stream
import qualified Streamly.Internal.Data.Stream.IsStream.Nesting as Stream
import qualified Streamly.Internal.Data.Stream.IsStream.Transform as Stream
import qualified Streamly.Internal.Data.Stream.StreamK as StreamK

import Prelude hiding (filter, zipWith, concatMap, concat)

-- $setup
-- >>> :m
-- >>> import Prelude hiding (filter, zipWith, concatMap, concat)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream

------------------------------------------------------------------------------
-- Sampling
------------------------------------------------------------------------------

-- XXX We can implement this using addition instead of "mod" to make it more
-- efficient.
--
-- | @sampleFromthen offset stride@ samples the element at @offset@ index and
-- then every element at strides of @stride@.
--
-- >>> Stream.toList $ Stream.sampleFromThen 2 3 $ Stream.enumerateFromTo 0 10
-- [2,5,8]
--
-- /Pre-release/
--
{-# INLINE sampleFromThen #-}
sampleFromThen :: (IsStream t, Monad m, Functor (t m)) =>
    Int -> Int -> t m a -> t m a
sampleFromThen offset stride =
    Stream.with Stream.indexed Stream.filter
        (\(i, _) -> i >= offset && (i - offset) `mod` stride == 0)

-- | Continuously evaluate the input stream and sample the last event in time
-- window of @n@ seconds.
--
-- This is also known as @throttle@ in some libraries.
--
-- @
-- sampleIntervalEnd n = Stream.catMaybes . Stream.intervalsOf n Fold.last
-- @
--
-- /Pre-release/
--
{-# INLINE sampleIntervalEnd #-}
sampleIntervalEnd :: (IsStream t, MonadAsync m, Functor (t m)) =>
    Double -> t m a -> t m a
sampleIntervalEnd n = Stream.catMaybes . Stream.intervalsOf n Fold.last

-- | Like 'sampleInterval' but samples at the beginning of the time window.
--
-- @
-- sampleIntervalStart n = Stream.catMaybes . Stream.intervalsOf n Fold.head
-- @
--
-- /Pre-release/
--
{-# INLINE sampleIntervalStart #-}
sampleIntervalStart :: (IsStream t, MonadAsync m, Functor (t m)) =>
    Double -> t m a -> t m a
sampleIntervalStart n = Stream.catMaybes . Stream.intervalsOf n Fold.head

-- | Sample one event at the end of each burst of events.  A burst is a group
-- of events close together in time, it ends when an event is spaced by more
-- than the specified time interval from the previous event.
--
-- This is known as @debounce@ in some libraries.
--
-- The clock granularity is 10 ms.
--
-- /Pre-release/
--
{-# INLINE sampleBurstEnd #-}
sampleBurstEnd :: (IsStream t, MonadAsync m, Functor (t m)) =>
    Double -> t m a -> t m a
sampleBurstEnd gap =
    let f (t1, _) (t2, _) =
            t2 - t1 >= toRelTime64 (NanoSecond64 (round (gap * 10^(9::Int))))
    in Stream.map snd
        . Stream.catMaybes
        . Stream.groupsByRolling f Fold.last
        . Stream.timeIndexed

-- | Like 'sampleBurstEnd' but samples the event at the beginning of the burst
-- instead of at the end of it.
--
-- /Pre-release/
--
{-# INLINE sampleBurstStart #-}
sampleBurstStart :: (IsStream t, MonadAsync m, Functor (t m)) =>
    Double -> t m a -> t m a
sampleBurstStart gap =
    let f (t1, _) (t2, _) =
            t2 - t1 >= toRelTime64 (NanoSecond64 (round (gap * 10^(9::Int))))
    in Stream.map snd
        . Stream.catMaybes
        . Stream.groupsByRolling f Fold.head
        . Stream.timeIndexed

------------------------------------------------------------------------------
-- Reordering
------------------------------------------------------------------------------
--
-- We could possibly choose different algorithms depending on whether the
-- input stream is almost sorted (ascending/descending) or random. We could
-- serialize the stream to an array and use quicksort.
--
-- | Sort the input stream using a supplied comparison function.
--
-- /O(n) space/
--
-- Note: this is not the fastest possible implementation as of now.
--
-- /Pre-release/
--
{-# INLINE sortBy #-}
sortBy :: (IsStream t, Monad m) => (a -> a -> Ordering) -> t m a -> t m a
-- XXX creating StreamD and using D.mergeBy may be more efficient due to fusion
sortBy f = Stream.concatPairsWith (Stream.mergeBy f) Stream.value

------------------------------------------------------------------------------
-- SQL Joins
------------------------------------------------------------------------------
--
-- Some references:
-- * https://en.wikipedia.org/wiki/Relational_algebra
-- * https://en.wikipedia.org/wiki/Join_(SQL)

-- TODO: OrdSet/IntSet/hashmap based versions of these. With Eq only
-- constraint, the best would be to use an Array with linear search. If the
-- second stream is sorted we can also use a binary search, using Ord
-- constraint or an ordering function.
--
-- For Storables we can cache the second stream into an unboxed array for
-- possibly faster access/compact representation?
--
-- If we do not want to keep the stream in memory but always read it from the
-- source (disk/network) every time we iterate through it then we can do that
-- too by reading the stream every time, the stream must have immutable state
-- in that case and the user is responsible for the behavior if the stream
-- source changes during iterations. We can also use an Unfold instead of
-- stream. We probably need a way to distinguish streams that can be read
-- mutliple times without any interference (e.g. unfolding a stream using an
-- immutable handle would work i.e. using pread/pwrite instead of maintianing
-- an offset in the handle).

-- XXX We can do this concurrently.
--
-- | This is the same as 'Streamly.Internal.Data.Unfold.outerProduct' but less
-- efficient.
--
-- The second stream is evaluated multiple times. If the second stream is
-- consume-once stream then it can be cached in an 'Data.Array.Array' before
-- calling this function. Caching may also improve performance if the stream is
-- expensive to evaluate.
--
-- Time: O(m x n)
--
-- /Pre-release/
{-# INLINE crossJoin #-}
crossJoin :: Monad (t m) => t m a -> t m b -> t m (a, b)
crossJoin s1 s2 = do
    -- XXX use concatMap instead?
    a <- s1
    b <- s2
    return (a, b)

-- XXX We can do this concurrently.
-- XXX If the second stream is sorted and passed as an Array we could use
-- binary search if we have an Ord instance or Ordering returning function. The
-- time complexity would then become (m x log n).
--
-- | For all elements in @t m a@, for all elements in @t m b@ if @a@ and @b@
-- are equal by the given equality pedicate then return the tuple (a, b).
--
-- The second stream is evaluated multiple times. If the stream is a
-- consume-once stream then the caller should cache it in an 'Data.Array.Array'
-- before calling this function. Caching may also improve performance if the
-- stream is expensive to evaluate.
--
-- For space efficiency use the smaller stream as the second stream.
--
-- Space: O(n) assuming the second stream is cached in memory.
--
-- Time: O(m x n)
--
-- /Pre-release/
{-# INLINE innerJoin #-}
innerJoin ::
    forall (t :: (Type -> Type) -> Type -> Type) m a b.
    (IsStream t, Monad (t m)) =>
        (a -> b -> Bool) -> t m a -> t m b -> t m (a, b)
innerJoin eq s1 s2 = do
    -- XXX use concatMap instead?
    a <- s1
    b <- s2
    if a `eq` b
    then return (a, b)
    else StreamK.nil

-- If the second stream is too big it can be partitioned based on hashes and
-- then we can process one parition at a time.
--
-- | Like 'innerJoin' but uses a hashmap for efficiency.
--
-- For space efficiency use the smaller stream as the second stream.
--
-- Space: O(n)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE hashInnerJoin #-}
hashInnerJoin :: -- Hashable b =>
    (a -> b -> Bool) -> t m a -> t m b -> t m (a, b)
hashInnerJoin = undefined

-- | Like 'innerJoin' but works only on sorted streams.
--
-- Space: O(1)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE mergeInnerJoin #-}
mergeInnerJoin :: (a -> b -> Ordering) -> t m a -> t m b -> t m (a, b)
mergeInnerJoin = undefined

-- XXX We can do this concurrently.
-- XXX If the second stream is sorted and passed as an Array or a seek capable
-- stream then we could use binary search if we have an Ord instance or
-- Ordering returning function. The time complexity would then become (m x log
-- n).
--
-- | For all elements in @t m a@, for all elements in @t m b@ if @a@ and @b@
-- are equal then return the tuple @(a, Just b)@.  If @a@ is not present in @t
-- m b@ then return @(a, Nothing)@.
--
-- The second stream is evaluated multiple times. If the stream is a
-- consume-once stream then the caller should cache it in an 'Data.Array.Array'
-- before calling this function. Caching may also improve performance if the
-- stream is expensive to evaluate.
--
-- @
-- rightJoin = flip leftJoin
-- @
--
-- Space: O(n) assuming the second stream is cached in memory.
--
-- Time: O(m x n)
--
-- /Unimplemented/
{-# INLINE leftJoin #-}
leftJoin :: Monad m =>
    (a -> b -> Bool) -> SerialT m a -> SerialT m b -> SerialT m (a, Maybe b)
leftJoin eq s1 s2 = Stream.evalStateT (return False) $ do
    a <- Stream.liftInner s1
    -- XXX should we use StreamD monad here?
    -- XXX Is there a better way to perform some action at the end of a loop
    -- iteration?
    lift $ put False
    let final = do
            r <- lift get
            if r
            then StreamK.nil
            else StreamK.value Nothing
    b <- fmap Just (Stream.liftInner s2) <> final
    case b of
        Just b1 ->
            if a `eq` b1
            then do
                lift $ put True
                return (a, Just b1)
            else StreamK.nil
        Nothing -> return (a, Nothing)

-- | Like 'outerJoin' but uses a hashmap for efficiency.
--
-- Space: O(n)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE hashLeftJoin #-}
hashLeftJoin :: -- Hashable b =>
    (a -> b -> Bool) -> t m a -> t m b -> t m (a, Maybe b)
hashLeftJoin = undefined

-- | Like 'leftJoin' but works only on sorted streams.
--
-- Space: O(1)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE mergeLeftJoin #-}
mergeLeftJoin :: -- Monad m =>
    (a -> b -> Ordering) -> t m a -> t m b -> t m (a, Maybe b)
mergeLeftJoin _eq _s1 _s2 = undefined

-- XXX We can do this concurrently.
--
-- | For all elements in @t m a@, for all elements in @t m b@ if @a@ and @b@
-- are equal by the given equality pedicate then return the tuple (Just a, Just
-- b).  If @a@ is not found in @t m b@ then return (a, Nothing), return
-- (Nothing, b) for vice-versa.
--
-- For space efficiency use the smaller stream as the second stream.
--
-- Space: O(n)
--
-- Time: O(m x n)
--
-- /Unimplemented/
{-# INLINE outerJoin #-}
outerJoin :: MonadIO m =>
       (a -> b -> Bool)
    -> SerialT m a
    -> SerialT m b
    -> SerialT m (Maybe a, Maybe b)
outerJoin eq s1 s =
    Stream.concatM $ do
        arr <- Array.fromStream $ fmap (,False) s
        return $ go arr <> leftOver arr

    where

    leftOver =
            fmap (\(x, _) -> (Nothing, Just x))
                . Stream.filter (not . snd)
                . Array.toStream

    go arr = Stream.evalStateT (return False) $ do
        a <- Stream.liftInner s1
        -- XXX should we use StreamD monad here?
        -- XXX Is there a better way to perform some action at the end of a loop
        -- iteration?
        lift $ put False
        let final = do
                r <- lift get
                if r
                then StreamK.nil
                else StreamK.value Nothing
        (_i, b) <-
            Stream.indexed
                $ fmap Just (Stream.liftInner (Array.toStream arr)) <> final
        case b of
            Just (b1, _used) ->
                if a `eq` b1
                then do
                    lift $ put True
                    -- XXX Need to use a mutable array
                    -- when (not used) $ Array.writeIndex i True
                    return (Just a, Just b1)
                else StreamK.nil
            Nothing -> return (Just a, Nothing)

-- Put the b's that have been paired, in another hash or mutate the hash to set
-- a flag. At the end go through @t m b@ and find those that are not in that
-- hash to return (Nothing, b).
--
-- | Like 'outerJoin' but uses a hashmap for efficiency.
--
-- For space efficiency use the smaller stream as the second stream.
--
-- Space: O(n)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE hashOuterJoin #-}
hashOuterJoin :: -- (Monad m, Hashable b) =>
    (a -> b -> Ordering) -> t m a -> t m b -> t m (Maybe a, Maybe b)
hashOuterJoin _eq _s1 _s2 = undefined

-- | Like 'outerJoin' but works only on sorted streams.
--
-- Space: O(1)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE mergeOuterJoin #-}
mergeOuterJoin :: -- Monad m =>
    (a -> b -> Ordering) -> t m a -> t m b -> t m (Maybe a, Maybe b)
mergeOuterJoin _eq _s1 _s2 = undefined

------------------------------------------------------------------------------
-- Set operations (special joins)
------------------------------------------------------------------------------
--
-- TODO: OrdSet/IntSet/hashmap based versions of these. With Eq only constraint
-- the best would be to use an Array with linear search. If the second stream
-- is sorted we can also use a binary search, using Ord constraint.

-- | 'intersectBy' is essentially a filtering operation that retains only those
-- elements in the first stream that are present in the second stream.
--
-- >>> Stream.toList $ Stream.intersectBy (==) (Stream.fromList [1,2,2,4]) (Stream.fromList [2,1,1,3])
-- [1,2,2]
--
-- >>> Stream.toList $ Stream.intersectBy (==) (Stream.fromList [2,1,1,3]) (Stream.fromList [1,2,2,4])
-- [2,1,1]
--
-- 'intersectBy' is similar to but not the same as 'innerJoin':
--
-- >>> Stream.toList $ fmap fst $ Stream.innerJoin (==) (Stream.fromList [1,2,2,4]) (Stream.fromList [2,1,1,3])
-- [1,1,2,2]
--
-- Space: O(n) where @n@ is the number of elements in the second stream.
--
-- Time: O(m x n) where @m@ is the number of elements in the first stream and
-- @n@ is the number of elements in the second stream.
--
-- /Pre-release/
{-# INLINE intersectBy #-}
intersectBy :: (IsStream t, Monad m) =>
    (a -> a -> Bool) -> t m a -> t m a -> t m a
intersectBy eq s1 s2 =
    concatM
        $ do
            -- This may work well when s2 is small
            xs <- Stream.toListRev $ Stream.uniqBy eq $ StreamK.adapt s2
            return $ Stream.filter (\x -> List.any (eq x) xs) s1

-- | Like 'intersectBy' but works only on sorted streams.
--
-- Space: O(1)
--
-- Time: O(m+n)
--
-- /Unimplemented/
{-# INLINE mergeIntersectBy #-}
mergeIntersectBy :: -- (IsStream t, Monad m) =>
    (a -> a -> Ordering) -> t m a -> t m a -> t m a
mergeIntersectBy _eq _s1 _s2 = undefined

-- Roughly leftJoin s1 s2 = s1 `difference` s2 + s1 `intersection` s2

-- | Delete first occurrences of those elements from the first stream that are
-- present in the second stream. If an element occurs multiple times in the
-- second stream as many occurrences of it are deleted from the first stream.
--
-- >>> Stream.toList $ Stream.differenceBy (==) (Stream.fromList [1,2,2]) (Stream.fromList [1,2,3])
-- [2]
--
-- The following laws hold:
--
-- @
-- (s1 `serial` s2) `differenceBy eq` s1 === s2
-- (s1 `wSerial` s2) `differenceBy eq` s1 === s2
-- @
--
-- Same as the list 'Data.List.//' operation.
--
-- Space: O(m) where @m@ is the number of elements in the first stream.
--
-- Time: O(m x n) where @m@ is the number of elements in the first stream and
-- @n@ is the number of elements in the second stream.
--
-- /Pre-release/
{-# INLINE differenceBy #-}
differenceBy :: (IsStream t, Monad m) =>
    (a -> a -> Bool) -> t m a -> t m a -> t m a
differenceBy eq s1 s2 =
    concatM
        $ do
            -- This may work well if s1 is small
            -- If s1 is big we can go through s1, deleting elements from s2 and
            -- not emitting an element if it was successfully deleted from s2.
            -- we will need a deleteBy that can return whether the element was
            -- deleted or not.
            xs <- Stream.toList $ StreamK.adapt s1
            fmap fromList $ foldl' (flip (List.deleteBy eq)) xs s2

-- | Like 'differenceBy' but works only on sorted streams.
--
-- Space: O(1)
--
-- /Unimplemented/
{-# INLINE mergeDifferenceBy #-}
mergeDifferenceBy :: -- (IsStream t, Monad m) =>
    (a -> a -> Ordering) -> t m a -> t m a -> t m a
mergeDifferenceBy _eq _s1 _s2 = undefined

-- | This is essentially an append operation that appends all the extra
-- occurrences of elements from the second stream that are not already present
-- in the first stream.
--
-- >>> Stream.toList $ Stream.unionBy (==) (Stream.fromList [1,2,2,4]) (Stream.fromList [1,1,2,3])
-- [1,2,2,4,3]
--
-- Equivalent to the following except that @s1@ is evaluated only once:
--
-- @
-- unionBy eq s1 s2 = s1 \`serial` (s2 `differenceBy eq` s1)
-- @
--
-- Similar to 'outerJoin' but not the same.
--
-- Space: O(n)
--
-- Time: O(m x n)
--
-- /Pre-release/
{-# INLINE unionBy #-}
unionBy :: (IsStream t, MonadAsync m, Semigroup (t m a)) =>
    (a -> a -> Bool) -> t m a -> t m a -> t m a
unionBy eq s1 s2 =
    concatM
        $ do
            xs <- Stream.toList $ StreamK.adapt s2
            -- XXX we can use postscanlMAfter' instead of IORef
            ref <- liftIO $ newIORef $! List.nubBy eq xs
            let f x = do
                    liftIO $ modifyIORef' ref (List.deleteBy eq x)
                    return x
                s3 = concatM
                        $ do
                            xs1 <- liftIO $ readIORef ref
                            return $ fromList xs1
            return $ Stream.mapM f s1 <> s3

-- | Like 'unionBy' but works only on sorted streams.
--
-- Space: O(1)
--
-- /Unimplemented/
{-# INLINE mergeUnionBy #-}
mergeUnionBy :: -- (IsStream t, Monad m) =>
    (a -> a -> Ordering) -> t m a -> t m a -> t m a
mergeUnionBy _eq _s1 _s2 = undefined
