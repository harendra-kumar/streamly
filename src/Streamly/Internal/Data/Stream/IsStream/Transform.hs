-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Transform
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.IsStream.Transform
    (
    -- * Piping
    -- | Pass through a 'Pipe'.
      transform

    -- * Folding
    , foldrS
    , foldrT

    -- * Mapping
    -- | Stateless one-to-one maps.
    , Serial.map
    , sequence
    , mapM
    , smapM

    -- * Mapping Side Effects (Observation)
    -- | See also the intersperse*_ combinators.
    , trace
    , trace_
    , tap
    , tapOffsetEvery
    , tapAsync
    , tapRate
    , pollCounts

    -- * Scanning By 'Fold'
    , scan
    , postscan

    -- * Scanning
    -- | Left scans. Stateful, mostly one-to-one maps.
    , scanl'
    , scanlM'
    , scanlMAfter'
    , postscanl'
    , postscanlM'
    , prescanl'
    , prescanlM'
    , scanl1'
    , scanl1M'

    -- XXX Once we have pipes the contravariant transformations can be
    -- represented by attaching pipes before a transformation.
    --
    -- , lscanl'
    -- , lscanlM'
    -- , lscanl1'
    -- , lscanl1M'
    --
    -- , lpostscanl'
    -- , lpostscanlM'
    -- , lprescanl'
    -- , lprescanlM'

    -- * Filtering
    -- | Produce a subset of the stream using criteria based on the values of
    -- the elements. We can use a concatMap and scan for filtering but these
    -- combinators are more efficient and convenient.

    , with
    , deleteBy
    , filter
    , filterM
    , uniq
    , uniqBy
    , nubBy
    , nubWindowBy
    , prune
    , repeated

    -- * Trimming
    -- | Produce a subset of the stream trimmed at ends.

    , take
    , takeByTime -- takeInterval
    , takeEnd
    , takeEndInterval
    , takeWhile
    , takeWhileM
    , takeWhileEnd
    , takeWhileAround
    , drop
    , dropByTime -- dropInterval
    , dropEnd
    , dropEndInterval
    , dropWhile
    , dropWhileM
    , dropWhileEnd
    , dropWhileAround

    -- * Inserting Elements
    -- | Produce a superset of the stream. This is the opposite of
    -- filtering/sampling.  We can always use concatMap and scan for inserting
    -- but these combinators are more efficient and convenient.

    -- Element agnostic (Opposite of sampling)
    , intersperse
    , intersperseM -- XXX naming
    , intersperseBySpan

    , intersperseSuffix
    , intersperseSuffixBySpan
    , interjectSuffix

    -- , interspersePrefix
    -- , interspersePrefixBySpan

    -- * Inserting Side Effects/Time
    , intersperseM_ -- XXX naming
    , delay
    , intersperseSuffix_
    , delayPost
    , interspersePrefix_
    , delayPre

    -- * Element Aware Insertion
    -- | Opposite of filtering
    , insertBy
    -- , intersperseByBefore
    -- , intersperseByAfter

    -- * Reordering
    , reverse
    , reverse'
    , reassembleBy

    -- * Position Indexing
    , indexed
    , indexedR

    -- * Time Indexing
    , timestamped
    , timestampWith
    -- , timestampedR -- timer
    , timeIndexed
    , timeIndexWith

    -- * Searching
    , findIndices -- XXX indicesBy
    , elemIndices -- XXX indicesOf

    -- * Rolling map
    -- | Map using the previous element.
    , rollingMapM
    , rollingMap

    -- * Maybe Streams
    -- Move these to Streamly.Data.Maybe.Stream?
    , catMaybes -- XXX justs (like lefts/rights)
    , mapMaybe
    , mapMaybeM

    -- * Either Streams
    -- Move these to Streamly.Data.Either.Stream?
    , lefts
    , rights

    -- * Concurrent Evaluation
    -- ** Concurrent Pipelines
    -- | Run streaming stages concurrently.

    , Par.mkParallel
    , applyAsync
    , (|$)
    , (|&)

    -- ** Concurrency Control
    , maxThreads

    -- ** Buffering and Sampling
    -- | Evaluate strictly using a buffer of results.  When the buffer becomes
    -- full we can block, drop the new elements, drop the oldest element and
    -- insert the new at the end or keep dropping elements uniformly to match
    -- the rate of the consumer.
    , maxBuffer
    , sampleOld
    , sampleNew
    , sampleRate

    -- ** Rate Limiting
    -- | Evaluate the stream at uniform intervals to maintain a specified
    -- evaluation rate.
    , Rate (..)
    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate

    -- * Diagnostics
    , inspectMode

    -- * Deprecated
    , scanx
    )
where

#include "inline.hs"

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Either (isLeft, isRight)
import Data.Kind (Type)
import Data.Maybe (isJust, fromJust)
import Streamly.Internal.BaseCompat (fromLeft, fromRight)
import Streamly.Internal.Data.Fold.Types (Fold (..))
import Streamly.Internal.Data.Pipe.Types (Pipe (..))
import Streamly.Internal.Data.Stream.IsStream.Combinators
      ( inspectMode, maxBuffer, maxThreads, rate, avgRate, minRate
      , maxRate, constRate)
import Streamly.Internal.Data.Stream.IsStream.Common
    ( absTimesWith
    , drop
    , findIndices
    , postscanlM'
    , relTimesWith
    , reverse
    , reverse'
    , scanlMAfter'
    , smapM
    , take
    , takeWhile
    , interjectSuffix
    , intersperseM
    )
import Streamly.Internal.Data.Stream.Prelude (fromStreamS, toStreamS)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Stream.StreamD (fromStreamD, toStreamD)
import Streamly.Internal.Data.Stream.StreamK (IsStream)
import Streamly.Internal.Data.SVar (MonadAsync, Rate(..))
import Streamly.Internal.Data.Time.Units (TimeUnit64, AbsTime, RelTime64)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.Parallel as Par
import qualified Streamly.Internal.Data.Stream.Prelude as P
import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.Zip as Z
#ifdef USE_STREAMK_ONLY
import qualified Streamly.Internal.Data.Stream.StreamK as S
#else
import qualified Streamly.Internal.Data.Stream.StreamD as S
#endif

import Prelude hiding
       ( filter, drop, dropWhile, take, takeWhile, foldr, map, mapM, sequence
       , reverse, foldr1 , scanl, scanl1)

--
-- $setup
-- >>> :m
-- >>> import Prelude hiding ( filter, drop, dropWhile, take, takeWhile, foldr, map, mapM, sequence, reverse, foldr1 , scanl, scanl1)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold

------------------------------------------------------------------------------
-- Piping
------------------------------------------------------------------------------

-- | Use a 'Pipe' to transform a stream.
--
-- /Pre-release/
--
{-# INLINE transform #-}
transform :: (IsStream t, Monad m) => Pipe m a b -> t m a -> t m b
transform pipe xs = fromStreamD $ D.transform pipe (toStreamD xs)

------------------------------------------------------------------------------
-- Transformation Folds
------------------------------------------------------------------------------

-- | Right fold to a streaming monad.
--
-- > foldrS S.cons S.nil === id
--
-- 'foldrS' can be used to perform stateless stream to stream transformations
-- like map and filter in general. It can be coupled with a scan to perform
-- stateful transformations. However, note that the custom map and filter
-- routines can be much more efficient than this due to better stream fusion.
--
-- >>> Stream.toList $ Stream.foldrS Stream.cons Stream.nil $ Stream.fromList [1..5]
-- [1,2,3,4,5]
--
-- Find if any element in the stream is 'True':
--
-- >>> Stream.toList $ Stream.foldrS (\x xs -> if odd x then return True else xs) (return False) $ (Stream.fromList (2:4:5:undefined) :: Stream.SerialT IO Int)
-- [True]
--
-- Map (+2) on odd elements and filter out the even elements:
--
-- >>> Stream.toList $ Stream.foldrS (\x xs -> if odd x then (x + 2) `Stream.cons` xs else xs) Stream.nil $ (Stream.fromList [1..5] :: Stream.SerialT IO Int)
-- [3,5,7]
--
-- 'foldrM' can also be represented in terms of 'foldrS', however, the former
-- is much more efficient:
--
-- > foldrM f z s = runIdentityT $ foldrS (\x xs -> lift $ f x (runIdentityT xs)) (lift z) s
--
-- /Pre-release/
{-# INLINE foldrS #-}
foldrS :: IsStream t => (a -> t m b -> t m b) -> t m b -> t m a -> t m b
foldrS = K.foldrS

-- | Right fold to a transformer monad.  This is the most general right fold
-- function. 'foldrS' is a special case of 'foldrT', however 'foldrS'
-- implementation can be more efficient:
--
-- > foldrS = foldrT
-- > foldrM f z s = runIdentityT $ foldrT (\x xs -> lift $ f x (runIdentityT xs)) (lift z) s
--
-- 'foldrT' can be used to translate streamly streams to other transformer
-- monads e.g.  to a different streaming type.
--
-- /Pre-release/
{-# INLINE foldrT #-}
foldrT :: (IsStream t, Monad m, Monad (s m), MonadTrans s)
    => (a -> s m b -> s m b) -> s m b -> t m a -> s m b
foldrT f z s = S.foldrT f z (toStreamS s)

------------------------------------------------------------------------------
-- Transformation by Mapping
------------------------------------------------------------------------------

-- |
-- @
-- mapM f = sequence . map f
-- @
--
-- Apply a monadic function to each element of the stream and replace it with
-- the output of the resulting action.
--
-- @
-- > drain $ S.mapM putStr $ S.fromList ["a", "b", "c"]
-- abc
--
-- drain $ S.replicateM 10 (return 1)
--           & (serially . S.mapM (\\x -> threadDelay 1000000 >> print x))
--
-- drain $ S.replicateM 10 (return 1)
--           & (asyncly . S.mapM (\\x -> threadDelay 1000000 >> print x))
-- @
--
-- /Concurrent (do not use with 'parallely' on infinite streams)/
--
-- @since 0.1.0
{-# INLINE_EARLY mapM #-}
mapM :: (IsStream t, MonadAsync m) => (a -> m b) -> t m a -> t m b
mapM = K.mapM

{-# RULES "mapM serial" mapM = mapMSerial #-}
{-# INLINE mapMSerial #-}
mapMSerial :: Monad m => (a -> m b) -> SerialT m a -> SerialT m b
mapMSerial = Serial.mapM

-- |
-- @
-- sequence = mapM id
-- @
--
-- Replace the elements of a stream of monadic actions with the outputs of
-- those actions.
--
-- @
-- > drain $ S.sequence $ S.fromList [putStr "a", putStr "b", putStrLn "c"]
-- abc
--
-- drain $ S.replicateM 10 (return $ threadDelay 1000000 >> print 1)
--           & (serially . S.sequence)
--
-- drain $ S.replicateM 10 (return $ threadDelay 1000000 >> print 1)
--           & (asyncly . S.sequence)
-- @
--
-- /Concurrent (do not use with 'parallely' on infinite streams)/
--
-- @since 0.1.0
{-# INLINE sequence #-}
sequence :: (IsStream t, MonadAsync m) => t m (m a) -> t m a
sequence m = fromStreamS $ S.sequence (toStreamS m)

------------------------------------------------------------------------------
-- Mapping side effects
------------------------------------------------------------------------------

-- | Tap the data flowing through a stream into a 'Fold'. For example, you may
-- add a tap to log the contents flowing through the stream. The fold is used
-- only for effects, its result is discarded.
--
-- @
--                   Fold m a b
--                       |
-- -----stream m a ---------------stream m a-----
--
-- @
--
-- >>> Stream.drain $ Stream.tap (Fold.drainBy print) (Stream.enumerateFromTo 1 2)
-- 1
-- 2
--
-- Compare with 'trace'.
--
-- @since 0.7.0
{-# INLINE tap #-}
tap :: (IsStream t, Monad m) => FL.Fold m a b -> t m a -> t m a
tap f xs = D.fromStreamD $ D.tap f (D.toStreamD xs)

-- XXX Remove this. It can be expressed in terms of Fold.sampleFromThen.
--
-- | @tapOffsetEvery offset n@ taps every @n@th element in the stream
-- starting at @offset@. @offset@ can be between @0@ and @n - 1@. Offset 0
-- means start at the first element in the stream. If the offset is outside
-- this range then @offset `mod` n@ is used as offset.
--
-- >>> Stream.drain $ Stream.tapOffsetEvery 0 2 (Fold.rmapM print Fold.toList) $ Stream.enumerateFromTo 0 10
-- [0,2,4,6,8,10]

--
-- /Pre-release/
--
{-# INLINE tapOffsetEvery #-}
tapOffsetEvery :: (IsStream t, Monad m)
    => Int -> Int -> FL.Fold m a b -> t m a -> t m a
tapOffsetEvery offset n f xs =
    D.fromStreamD $ D.tapOffsetEvery offset n f (D.toStreamD xs)

-- | Redirect a copy of the stream to a supplied fold and run it concurrently
-- in an independent thread. The fold may buffer some elements. The buffer size
-- is determined by the prevailing 'maxBuffer' setting.
--
-- @
--               Stream m a -> m b
--                       |
-- -----stream m a ---------------stream m a-----
--
-- @
--
-- @
-- > S.drain $ S.tapAsync (S.mapM_ print) (S.enumerateFromTo 1 2)
-- 1
-- 2
-- @
--
-- Exceptions from the concurrently running fold are propagated to the current
-- computation.  Note that, because of buffering in the fold, exceptions may be
-- delayed and may not correspond to the current element being processed in the
-- parent stream, but we guarantee that before the parent stream stops the tap
-- finishes and all exceptions from it are drained.
--
--
-- Compare with 'tap'.
--
-- /Pre-release/
{-# INLINE tapAsync #-}
tapAsync :: (IsStream t, MonadAsync m) => FL.Fold m a b -> t m a -> t m a
tapAsync f xs = D.fromStreamD $ Par.tapAsyncF f (D.toStreamD xs)

-- | @pollCounts predicate transform fold stream@ counts those elements in the
-- stream that pass the @predicate@. The resulting count stream is sent to
-- another thread which transforms it using @transform@ and then folds it using
-- @fold@.  The thread is automatically cleaned up if the stream stops or
-- aborts due to exception.
--
-- For example, to print the count of elements processed every second:
--
-- @
-- > S.drain $ S.pollCounts (const True) (S.rollingMap (-) . S.delayPost 1) (FL.drainBy print)
--           $ S.enumerateFrom 0
-- @
--
-- Note: This may not work correctly on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE pollCounts #-}
pollCounts ::
       (IsStream t, MonadAsync m)
    => (a -> Bool)
    -> (t m Int -> t m Int)
    -> Fold m Int b
    -> t m a
    -> t m a
pollCounts predicate transf f xs =
      D.fromStreamD
    $ D.pollCounts predicate (D.toStreamD . transf . D.fromStreamD) f
    $ D.toStreamD xs

-- | Calls the supplied function with the number of elements consumed
-- every @n@ seconds. The given function is run in a separate thread
-- until the end of the stream. In case there is an exception in the
-- stream the thread is killed during the next major GC.
--
-- Note: The action is not guaranteed to run if the main thread exits.
--
-- @
-- > delay n = threadDelay (round $ n * 1000000) >> return n
-- > S.drain $ S.tapRate 2 (\\n -> print $ show n ++ " elements processed") (delay 1 S.|: delay 0.5 S.|: delay 0.5 S.|: S.nil)
-- 2 elements processed
-- 1 elements processed
-- @
--
-- Note: This may not work correctly on 32-bit machines.
--
-- /Pre-release/
{-# INLINE tapRate #-}
tapRate ::
       (IsStream t, MonadAsync m, MonadCatch m)
    => Double
    -> (Int -> m b)
    -> t m a
    -> t m a
tapRate n f xs = D.fromStreamD $ D.tapRate n f $ D.toStreamD xs

-- | Apply a monadic function to each element flowing through the stream and
-- discard the results.
--
-- @
-- > S.drain $ S.trace print (S.enumerateFromTo 1 2)
-- 1
-- 2
-- @
--
-- Compare with 'tap'.
--
-- @since 0.7.0
{-# INLINE trace #-}
trace :: (IsStream t, MonadAsync m) => (a -> m b) -> t m a -> t m a
trace f = mapM (\x -> void (f x) >> return x)

-- | Perform a side effect before yielding each element of the stream and
-- discard the results.
--
-- @
-- > S.drain $ S.trace_ (print "got here") (S.enumerateFromTo 1 2)
-- "got here"
-- "got here"
-- @
--
-- Same as 'interspersePrefix_' but always serial.
--
-- See also: 'trace'
--
-- /Pre-release/
{-# INLINE trace_ #-}
trace_ :: (IsStream t, Monad m) => m b -> t m a -> t m a
trace_ eff = Serial.mapM (\x -> eff >> return x)

------------------------------------------------------------------------------
-- Scanning with a Fold
------------------------------------------------------------------------------

-- | Scan a stream using the given monadic fold.
--
-- @since 0.7.0
{-# INLINE scan #-}
scan :: (IsStream t, Monad m) => Fold m a b -> t m a -> t m b
scan = P.scanOnce

-- | Postscan a stream using the given monadic fold.
--
-- @since 0.7.0
{-# INLINE postscan #-}
postscan :: (IsStream t, Monad m) => Fold m a b -> t m a -> t m b
postscan = P.postscanOnce

------------------------------------------------------------------------------
-- Scanning - Transformation by Folding
------------------------------------------------------------------------------

-- XXX It may be useful to have a version of scan where we can keep the
-- accumulator independent of the value emitted. So that we do not necessarily
-- have to keep a value in the accumulator which we are not using. We can pass
-- an extraction function that will take the accumulator and the current value
-- of the element and emit the next value in the stream. That will also make it
-- possible to modify the accumulator after using it. In fact, the step function
-- can return new accumulator and the value to be emitted. The signature would
-- be more like mapAccumL. Or we can change the signature of scanx to
-- accommodate this.
--
-- | Strict left scan with an extraction function. Like 'scanl'', but applies a
-- user supplied extraction function (the third argument) at each step. This is
-- designed to work with the @foldl@ library. The suffix @x@ is a mnemonic for
-- extraction.
--
-- /Since 0.2.0/
--
-- /Since: 0.7.0 (Monad m constraint)/
{-# DEPRECATED scanx "Please use scanl followed by map instead." #-}
{-# INLINE scanx #-}
scanx :: (IsStream t, Monad m) => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scanx = P.scanlx'

-- XXX this needs to be concurrent
-- XXX because of the use of D.cons for appending, scanlM' has quadratic
-- complexity when iterated over a stream. We should use StreamK style scanlM'
-- for linear performance on iteration.
--
-- | Like 'scanl'' but with a monadic step function and a monadic seed.
--
-- /Since: 0.4.0/
--
-- /Since: 0.8.0 (signature change)/
{-# INLINE scanlM' #-}
scanlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> m b -> t m a -> t m b
scanlM' step begin m = fromStreamD $ D.scanlM' step begin $ toStreamD m

-- XXX because of the use of D.cons for appending, scanl' has quadratic
-- complexity when iterated over a stream. We should use StreamK style scanlM'
-- for linear performance on iteration.

-- | Strict left scan. Like 'map', 'scanl'' too is a one to one transformation,
-- however it adds an extra element.
--
-- @
-- > S.toList $ S.scanl' (+) 0 $ fromList [1,2,3,4]
-- [0,1,3,6,10]
-- @
--
-- @
-- > S.toList $ S.scanl' (flip (:)) [] $ S.fromList [1,2,3,4]
-- [[],[1],[2,1],[3,2,1],[4,3,2,1]]
-- @
--
-- The output of 'scanl'' is the initial value of the accumulator followed by
-- all the intermediate steps and the final result of 'foldl''.
--
-- By streaming the accumulated state after each fold step, we can share the
-- state across multiple stages of stream composition. Each stage can modify or
-- extend the state, do some processing with it and emit it for the next stage,
-- thus modularizing the stream processing. This can be useful in
-- stateful or event-driven programming.
--
-- Consider the following monolithic example, computing the sum and the product
-- of the elements in a stream in one go using a @foldl'@:
--
-- @
-- > S.foldl' (\\(s, p) x -> (s + x, p * x)) (0,1) $ S.fromList \[1,2,3,4]
-- (10,24)
-- @
--
-- Using @scanl'@ we can make it modular by computing the sum in the first
-- stage and passing it down to the next stage for computing the product:
--
-- @
-- >   S.foldl' (\\(_, p) (s, x) -> (s, p * x)) (0,1)
--   $ S.scanl' (\\(s, _) x -> (s + x, x)) (0,1)
--   $ S.fromList \[1,2,3,4]
-- (10,24)
-- @
--
-- IMPORTANT: 'scanl'' evaluates the accumulator to WHNF.  To avoid building
-- lazy expressions inside the accumulator, it is recommended that a strict
-- data structure is used for accumulator.
--
-- See also: 'usingStateT'
--
-- @since 0.2.0
{-# INLINE scanl' #-}
scanl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> t m b
scanl' step z m = fromStreamS $ S.scanl' step z $ toStreamS m

-- | Like 'scanl'' but does not stream the initial value of the accumulator.
--
-- > postscanl' f z xs = S.drop 1 $ S.scanl' f z xs
--
-- @since 0.7.0
{-# INLINE postscanl' #-}
postscanl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> t m b
postscanl' step z m = fromStreamD $ D.postscanl' step z $ toStreamD m

-- XXX prescanl does not sound very useful, enable only if there is a
-- compelling use case.
--
-- | Like scanl' but does not stream the final value of the accumulator.
--
-- /Pre-release/
{-# INLINE prescanl' #-}
prescanl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> t m b
prescanl' step z m = fromStreamD $ D.prescanl' step z $ toStreamD m

-- XXX this needs to be concurrent
-- | Like prescanl' but with a monadic step function and a monadic seed.
--
-- /Pre-release/
{-# INLINE prescanlM' #-}
prescanlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> m b -> t m a -> t m b
prescanlM' step z m = fromStreamD $ D.prescanlM' step z $ toStreamD m

-- XXX this needs to be concurrent
-- | Like 'scanl1'' but with a monadic step function.
--
-- @since 0.6.0
{-# INLINE scanl1M' #-}
scanl1M' :: (IsStream t, Monad m) => (a -> a -> m a) -> t m a -> t m a
scanl1M' step m = fromStreamD $ D.scanl1M' step $ toStreamD m

-- | Like 'scanl'' but for a non-empty stream. The first element of the stream
-- is used as the initial value of the accumulator. Does nothing if the stream
-- is empty.
--
-- @
-- > S.toList $ S.scanl1 (+) $ fromList [1,2,3,4]
-- [1,3,6,10]
-- @
--
-- @since 0.6.0
{-# INLINE scanl1' #-}
scanl1' :: (IsStream t, Monad m) => (a -> a -> a) -> t m a -> t m a
scanl1' step m = fromStreamD $ D.scanl1' step $ toStreamD m

------------------------------------------------------------------------------
-- Filtering
------------------------------------------------------------------------------

-- | Modify a @t m a -> t m a@ stream transformation that accepts a predicate
-- @(a -> b)@ to accept @((s, a) -> b)@ instead, provided a transformation @t m
-- a -> t m (s, a)@. Convenient to filter with index or time.
--
-- @
-- filterWithIndex = with indexed filter
-- filterWithAbsTime = with timestamped filter
-- filterWithRelTime = with timeIndexed filter
-- @
--
-- /Pre-release/
{-# INLINE with #-}
with :: forall (t :: (Type -> Type) -> Type -> Type) m a b s. Functor (t m) =>
       (t m a -> t m (s, a))
    -> (((s, a) -> b) -> t m (s, a) -> t m (s, a))
    -> (((s, a) -> b) -> t m a -> t m a)
with f comb g = fmap snd . comb g . f

-- | Include only those elements that pass a predicate.
--
-- @since 0.1.0
{-# INLINE filter #-}
#if __GLASGOW_HASKELL__ != 802
-- GHC 8.2.2 crashes with this code, when used with "stack"
filter :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m a
filter p m = fromStreamS $ S.filter p $ toStreamS m
#else
filter :: IsStream t => (a -> Bool) -> t m a -> t m a
filter = K.filter
#endif

-- | Same as 'filter' but with a monadic predicate.
--
-- @since 0.4.0
{-# INLINE filterM #-}
filterM :: (IsStream t, Monad m) => (a -> m Bool) -> t m a -> t m a
filterM p m = fromStreamD $ D.filterM p $ toStreamD m

-- | Drop repeated elements that are adjacent to each other using the supplied
-- comparison function.
--
-- @uniq = uniqBy (==)
--
-- To strip duplicate path separators:
--
-- @
-- f x y = x == '/' && x == y
-- Stream.toList $ Stream.uniqBy f $ Stream.fromList "//a//b"
-- "/a/b"
-- @
--
-- Space: @O(1)@
--
-- See also: 'nubBy'.
--
-- /Pre-release/
--
{-# INLINE uniqBy #-}
uniqBy :: (IsStream t, Monad m, Functor (t m)) =>
    (a -> a -> Bool) -> t m a -> t m a
uniqBy eq =
    catMaybes . rollingMap (\x y -> if x `eq` y then Nothing else Just y)

-- | Drop repeated elements that are adjacent to each other.
--
-- @since 0.6.0
{-# INLINE uniq #-}
uniq :: (Eq a, IsStream t, Monad m) => t m a -> t m a
uniq = fromStreamD . D.uniq . toStreamD

-- | Strip all leading and trailing occurrences of an element passing a
-- predicate and make all other consecutive occurrences uniq.
--
-- @
-- prune p = dropWhileAround p $ uniqBy (x y -> p x && p y)
-- @
--
-- @
-- > Stream.pruneBy isSpace (Stream.fromList "  hello      world!   ")
-- "hello world!"
-- @
--
-- Space: @O(1)@
--
-- /Unimplemented/
{-# INLINE prune #-}
prune ::
    -- (IsStream t, Monad m, Eq a) =>
    (a -> Bool) -> t m a -> t m a
prune = error "Not implemented yet!"

-- Possible implementation:
-- @repeated =
--      Stream.catMaybes . Stream.parseMany (Parser.groupBy (==) Fold.repeated)@
--
-- 'Fold.repeated' should return 'Just' when repeated, and 'Nothing' for a
-- single element.
--
-- | Emit only repeated elements, once.
--
-- /Unimplemented/
repeated :: -- (IsStream t, Monad m, Eq a) =>
    t m a -> t m a
repeated = undefined

-- We can have more efficient implementations for nubOrd and nubInt by using
-- Set and IntSet to find/remove duplication. For Hashable we can use a
-- hashmap. Use rewrite rules to specialize to more efficient impls.
--
-- | Drop repeated elements anywhere in the stream.
--
-- /Caution: not scalable for infinite streams/
--
-- /See also: nubWindowBy/
--
-- /Unimplemented/
--
{-# INLINE nubBy #-}
nubBy :: -- (IsStream t, Monad m) =>
    (a -> a -> Bool) -> t m a -> t m a
nubBy = undefined -- fromStreamD . D.nubBy . toStreamD

-- | Drop repeated elements within the specified tumbling window in the stream.
--
-- @nubBy = nubWindowBy maxBound@
--
-- /Unimplemented/
--
{-# INLINE nubWindowBy #-}
nubWindowBy :: -- (IsStream t, Monad m) =>
    Int -> (a -> a -> Bool) -> t m a -> t m a
nubWindowBy = undefined -- fromStreamD . D.nubWithinBy . toStreamD

-- | Deletes the first occurrence of the element in the stream that satisfies
-- the given equality predicate.
--
-- @
-- > S.toList $ S.deleteBy (==) 3 $ S.fromList [1,3,3,5]
-- [1,3,5]
-- @
--
-- @since 0.6.0
{-# INLINE deleteBy #-}
deleteBy :: (IsStream t, Monad m) => (a -> a -> Bool) -> a -> t m a -> t m a
deleteBy cmp x m = fromStreamS $ S.deleteBy cmp x (toStreamS m)

------------------------------------------------------------------------------
-- Lossy Buffering
------------------------------------------------------------------------------

-- XXX We could use 'maxBuffer Block/Drop/Rotate/Sample' instead. However we
-- may want to have the evaluation rate independent of the sampling rate. To
-- support that we can decouple evaluation and sampling in independent stages.
-- The sampling stage would strictly evaluate and sample, the evaluation stage
-- would control the evaluation rate.

-- | Evaluate the input stream continuously and keep only the oldest @n@
-- elements in the buffer, discard the new ones when the buffer is full.  When
-- the output stream is evaluated it consumes the values from the buffer in a
-- FIFO manner.
--
-- /Unimplemented/
--
{-# INLINE sampleOld #-}
sampleOld :: -- (IsStream t, Monad m) =>
    Int -> t m a -> t m a
sampleOld = undefined

-- | Evaluate the input stream continuously and keep only the latest @n@
-- elements in a ring buffer, keep discarding the older ones to make space for
-- the new ones.  When the output stream is evaluated it consumes the values
-- from the buffer in a FIFO manner.
--
-- /Unimplemented/
--
{-# INLINE sampleNew #-}
sampleNew :: -- (IsStream t, Monad m) =>
    Int -> t m a -> t m a
sampleNew = undefined

-- | Like 'sampleNew' but samples at uniform intervals to match the consumer
-- rate. Note that 'sampleNew' leads to non-uniform sampling depending on the
-- consumer pattern.
--
-- /Unimplemented/
--
{-# INLINE sampleRate #-}
sampleRate :: -- (IsStream t, Monad m) =>
    Double -> t m a -> t m a
sampleRate = undefined

------------------------------------------------------------------------------
-- Trimming
------------------------------------------------------------------------------

-- | Same as 'takeWhile' but with a monadic predicate.
--
-- @since 0.4.0
{-# INLINE takeWhileM #-}
takeWhileM :: (IsStream t, Monad m) => (a -> m Bool) -> t m a -> t m a
takeWhileM p m = fromStreamD $ D.takeWhileM p $ toStreamD m

-- See the lastN fold for impl hints. Use a Data.Array based ring buffer.
--
-- takeEnd n = Stream.concatM . fmap Array.toStream . fold Fold.lastN
--
-- | Take @n@ elements at the end of the stream.
--
-- O(n) space, where n is the number elements taken.
--
-- /Unimplemented/
{-# INLINE takeEnd #-}
takeEnd :: -- (IsStream t, Monad m) =>
    Int -> t m a -> t m a
takeEnd = undefined -- fromStreamD $ D.takeEnd n $ toStreamD m

-- | Take time interval @i@ seconds at the end of the stream.
--
-- O(n) space, where n is the number elements taken.
--
-- /Unimplemented/
{-# INLINE takeEndInterval #-}
takeEndInterval :: -- (IsStream t, Monad m) =>
    Double -> t m a -> t m a
takeEndInterval = undefined -- fromStreamD $ D.takeEnd n $ toStreamD m

-- | Take all consecutive elements at the end of the stream for which the
-- predicate is true.
--
-- O(n) space, where n is the number elements taken.
--
-- /Unimplemented/
{-# INLINE takeWhileEnd #-}
takeWhileEnd :: -- (IsStream t, Monad m) =>
    (a -> Bool) -> t m a -> t m a
takeWhileEnd = undefined -- fromStreamD $ D.takeWhileEnd n $ toStreamD m

-- | Like 'takeWhile' and 'takeWhileEnd' combined.
--
-- O(n) space, where n is the number elements taken from the end.
--
-- /Unimplemented/
{-# INLINE takeWhileAround #-}
takeWhileAround :: -- (IsStream t, Monad m) =>
    (a -> Bool) -> t m a -> t m a
takeWhileAround = undefined -- fromStreamD $ D.takeWhileAround n $ toStreamD m

-- | @takeByTime duration@ yields stream elements upto specified time
-- @duration@. The duration starts when the stream is evaluated for the first
-- time, before the first element is yielded. The time duration is checked
-- before generating each element, if the duration has expired the stream
-- stops.
--
-- The total time taken in executing the stream is guaranteed to be /at least/
-- @duration@, however, because the duration is checked before generating an
-- element, the upper bound is indeterminate and depends on the time taken in
-- generating and processing the last element.
--
-- No element is yielded if the duration is zero. At least one element is
-- yielded if the duration is non-zero.
--
-- /Pre-release/
--
{-# INLINE takeByTime #-}
takeByTime ::(MonadIO m, IsStream t, TimeUnit64 d) => d -> t m a -> t m a
takeByTime d = fromStreamD . D.takeByTime d . toStreamD

-- | Drop elements in the stream as long as the predicate succeeds and then
-- take the rest of the stream.
--
-- @since 0.1.0
{-# INLINE dropWhile #-}
dropWhile :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m a
dropWhile p m = fromStreamS $ S.dropWhile p $ toStreamS m

-- | Same as 'dropWhile' but with a monadic predicate.
--
-- @since 0.4.0
{-# INLINE dropWhileM #-}
dropWhileM :: (IsStream t, Monad m) => (a -> m Bool) -> t m a -> t m a
dropWhileM p m = fromStreamD $ D.dropWhileM p $ toStreamD m

-- | @dropByTime duration@ drops stream elements until specified @duration@ has
-- passed.  The duration begins when the stream is evaluated for the first
-- time. The time duration is checked /after/ generating a stream element, the
-- element is yielded if the duration has expired otherwise it is dropped.
--
-- The time elapsed before starting to generate the first element is /at most/
-- @duration@, however, because the duration expiry is checked after the
-- element is generated, the lower bound is indeterminate and depends on the
-- time taken in generating an element.
--
-- All elements are yielded if the duration is zero.
--
-- /Pre-release/
--
{-# INLINE dropByTime #-}
dropByTime ::(MonadIO m, IsStream t, TimeUnit64 d) => d -> t m a -> t m a
dropByTime d = fromStreamD . D.dropByTime d . toStreamD

-- | Drop @n@ elements at the end of the stream.
--
-- O(n) space, where n is the number elements dropped.
--
-- /Unimplemented/
{-# INLINE dropEnd #-}
dropEnd :: -- (IsStream t, Monad m) =>
    Int -> t m a -> t m a
dropEnd = undefined -- fromStreamD $ D.dropEnd n $ toStreamD m

-- | Drop time interval @i@ seconds at the end of the stream.
--
-- O(n) space, where n is the number elements dropped.
--
-- /Unimplemented/
{-# INLINE dropEndInterval #-}
dropEndInterval :: -- (IsStream t, Monad m) =>
    Int -> t m a -> t m a
dropEndInterval = undefined

-- | Drop all consecutive elements at the end of the stream for which the
-- predicate is true.
--
-- O(n) space, where n is the number elements dropped.
--
-- /Unimplemented/
{-# INLINE dropWhileEnd #-}
dropWhileEnd :: -- (IsStream t, Monad m) =>
    (a -> Bool) -> t m a -> t m a
dropWhileEnd = undefined -- fromStreamD $ D.dropWhileEnd n $ toStreamD m

-- | Like 'dropWhile' and 'dropWhileEnd' combined.
--
-- O(n) space, where n is the number elements dropped from the end.
--
-- /Unimplemented/
{-# INLINE dropWhileAround #-}
dropWhileAround :: -- (IsStream t, Monad m) =>
    (a -> Bool) -> t m a -> t m a
dropWhileAround = undefined -- fromStreamD $ D.dropWhileAround n $ toStreamD m

------------------------------------------------------------------------------
-- Inserting Elements
------------------------------------------------------------------------------

-- | @insertBy cmp elem stream@ inserts @elem@ before the first element in
-- @stream@ that is less than @elem@ when compared using @cmp@.
--
-- @
-- insertBy cmp x = 'mergeBy' cmp ('value' x)
-- @
--
-- @
-- > S.toList $ S.insertBy compare 2 $ S.fromList [1,3,5]
-- [1,2,3,5]
-- @
--
-- @since 0.6.0
{-# INLINE insertBy #-}
insertBy ::
       (IsStream t, Monad m) => (a -> a -> Ordering) -> a -> t m a -> t m a
insertBy cmp x m = fromStreamS $ S.insertBy cmp x (toStreamS m)

-- | Insert a pure value between successive elements of a stream.
--
-- >>> Stream.toList $ Stream.intersperse ',' $ Stream.fromList "hello"
-- "h,e,l,l,o"
--
-- @since 0.7.0
{-# INLINE intersperse #-}
intersperse :: (IsStream t, MonadAsync m) => a -> t m a -> t m a
intersperse a = fromStreamS . S.intersperse a . toStreamS

-- | Insert a side effect before consuming an element of a stream except the
-- first one.
--
-- >>> Stream.drain $ Stream.trace putChar $ Stream.intersperseM_ (putChar '.') $ Stream.fromList "hello"
-- h.e.l.l.o
--
-- /Pre-release/
{-# INLINE intersperseM_ #-}
intersperseM_ :: (IsStream t, Monad m) => m b -> t m a -> t m a
intersperseM_ m = fromStreamD . D.intersperseM_ m . toStreamD

-- | Intersperse a monadic action into the input stream after every @n@
-- elements.
--
-- @
-- > S.toList $ S.intersperseBySpan 2 (return ',') $ S.fromList "hello"
-- "he,ll,o"
-- @
--
-- /Unimplemented/
{-# INLINE intersperseBySpan #-}
intersperseBySpan :: -- IsStream t =>
    Int -> m a -> t m a -> t m a
intersperseBySpan _n _f _xs = undefined

-- | Insert an effect and its output after consuming an element of a stream.
--
-- >>> Stream.toList $ Stream.trace putChar $ intersperseSuffix (putChar '.' >> return ',') $ Stream.fromList "hello"
-- h.,e.,l.,l.,o.,"h,e,l,l,o,"
--
-- /Pre-release/
{-# INLINE intersperseSuffix #-}
intersperseSuffix :: (IsStream t, MonadAsync m) => m a -> t m a -> t m a
intersperseSuffix m = fromStreamD . D.intersperseSuffix m . toStreamD

-- | Insert a side effect after consuming an element of a stream.
--
-- @
-- > S.mapM_ putChar $ S.intersperseSuffix_ (threadDelay 1000000) $ S.fromList "hello"
-- @
--
-- /Pre-release/
--
{-# INLINE intersperseSuffix_ #-}
intersperseSuffix_ :: (IsStream t, Monad m) => m b -> t m a -> t m a
intersperseSuffix_ m = fromStreamD . D.intersperseSuffix_ m . toStreamD

-- XXX Use an offset argument, like tapOffsetEvery
--
-- | Like 'intersperseSuffix' but intersperses an effectful action into the
-- input stream after every @n@ elements and after the last element.
--
-- >>> Stream.toList $ Stream.intersperseSuffixBySpan 2 (return ',') $ Stream.fromList "hello"
-- "he,ll,o,"
--
-- /Pre-release/
--
{-# INLINE intersperseSuffixBySpan #-}
intersperseSuffixBySpan :: (IsStream t, MonadAsync m)
    => Int -> m a -> t m a -> t m a
intersperseSuffixBySpan n eff =
    fromStreamD . D.intersperseSuffixBySpan n eff . toStreamD

-- | Insert a side effect before consuming an element of a stream.
--
-- >>> Stream.toList $ Stream.trace putChar $ Stream.interspersePrefix_ (putChar '.' >> return ',') $ Stream.fromList "hello"
-- .h.e.l.l.o"hello"
--
-- Same as 'trace_' but may be concurrent.
--
-- /Concurrent/
--
-- /Pre-release/
--
{-# INLINE interspersePrefix_ #-}
interspersePrefix_ :: (IsStream t, MonadAsync m) => m b -> t m a -> t m a
interspersePrefix_ m = mapM (\x -> void m >> return x)

------------------------------------------------------------------------------
-- Inserting Time
------------------------------------------------------------------------------

-- Note: delay must be serial.
--
-- | Introduce a delay of specified seconds before consuming an element of the
-- stream except the first one.
--
-- >>> Stream.mapM_ print $ Stream.timestamped $ Stream.delay 1 $ Stream.enumerateFromTo 1 3
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),1)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),2)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),3)
--
-- /Pre-release/
--
{-# INLINE delay #-}
delay :: (IsStream t, MonadIO m) => Double -> t m a -> t m a
delay n = intersperseM_ $ liftIO $ threadDelay $ round $ n * 1000000

-- Note: delay must be serial.
--
-- | Introduce a delay of specified seconds after consuming an element of a
-- stream.
--
-- >>> Stream.mapM_ print $ Stream.timestamped $ Stream.delayPost 1 $ Stream.enumerateFromTo 1 3
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),1)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),2)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),3)
--
-- /Pre-release/
--
{-# INLINE delayPost #-}
delayPost :: (IsStream t, MonadIO m) => Double -> t m a -> t m a
delayPost n = intersperseSuffix_ $ liftIO $ threadDelay $ round $ n * 1000000

-- Note: delay must be serial, that's why 'trace_' is used.
--
-- | Introduce a delay of specified seconds before consuming an element of a
-- stream.
--
-- >>> Stream.mapM_ print $ Stream.timestamped $ Stream.delayPre 1 $ Stream.enumerateFromTo 1 3
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),1)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),2)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),3)
--
-- /Pre-release/
--
{-# INLINE delayPre #-}
delayPre :: (IsStream t, MonadIO m) => Double -> t m a -> t m a
delayPre n = trace_ $ liftIO $ threadDelay $ round $ n * 1000000

------------------------------------------------------------------------------
-- Reorder in sequence
------------------------------------------------------------------------------

-- | Buffer until the next element in sequence arrives. The function argument
-- determines the difference in sequence numbers. This could be useful in
-- implementing sequenced streams, for example, TCP reassembly.
--
-- /Unimplemented/
--
{-# INLINE reassembleBy #-}
reassembleBy
    :: -- (IsStream t, Monad m) =>
       Fold m a b
    -> (a -> a -> Int)
    -> t m a
    -> t m b
reassembleBy = undefined

------------------------------------------------------------------------------
-- Position Indexing
------------------------------------------------------------------------------

-- |
-- > indexed = S.postscanl' (\(i, _) x -> (i + 1, x)) (-1,undefined)
-- > indexed = S.zipWith (,) (S.enumerateFrom 0)
--
-- Pair each element in a stream with its index, starting from index 0.
--
-- >>> Stream.toList $ Stream.indexed $ Stream.fromList "hello"
-- [(0,'h'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]
--
-- @since 0.6.0
{-# INLINE indexed #-}
indexed :: (IsStream t, Monad m) => t m a -> t m (Int, a)
indexed = fromStreamD . D.indexed . toStreamD

-- |
-- > indexedR n = S.postscanl' (\(i, _) x -> (i - 1, x)) (n + 1,undefined)
-- > indexedR n = S.zipWith (,) (S.enumerateFromThen n (n - 1))
--
-- Pair each element in a stream with its index, starting from the
-- given index @n@ and counting down.
--
-- >>> Stream.toList $ Stream.indexedR 10 $ Stream.fromList "hello"
-- [(10,'h'),(9,'e'),(8,'l'),(7,'l'),(6,'o')]
--
-- @since 0.6.0
{-# INLINE indexedR #-}
indexedR :: (IsStream t, Monad m) => Int -> t m a -> t m (Int, a)
indexedR n = fromStreamD . D.indexedR n . toStreamD

-------------------------------------------------------------------------------
-- Time Indexing
-------------------------------------------------------------------------------

-- Note: The timestamp stream must be the second stream in the zip so that the
-- timestamp is generated after generating the stream element and not before.
-- If we do not do that then the following example will generate the same
-- timestamp for first two elements:
--
-- S.mapM_ print $ S.timestamped $ S.delay $ S.enumerateFromTo 1 3
--
-- | Pair each element in a stream with an absolute timestamp, using a clock of
-- specified granularity.  The timestamp is generated just before the element
-- is consumed.
--
-- >>> Stream.mapM_ print $ Stream.timestampWith 0.01 $ Stream.delay 1 $ Stream.enumerateFromTo 1 3
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),1)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),2)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),3)
--
-- /Pre-release/
--
{-# INLINE timestampWith #-}
timestampWith :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m a -> t m (AbsTime, a)
timestampWith g stream = Z.zipWith (flip (,)) stream (absTimesWith g)

-- TBD: check performance vs a custom implementation without using zipWith.
--
-- /Pre-release/
--
{-# INLINE timestamped #-}
timestamped :: (IsStream t, MonadAsync m, Functor (t m))
    => t m a -> t m (AbsTime, a)
timestamped = timestampWith 0.01

-- | Pair each element in a stream with relative times starting from 0, using a
-- clock with the specified granularity. The time is measured just before the
-- element is consumed.
--
-- >>> Stream.mapM_ print $ Stream.timeIndexWith 0.01 $ Stream.delay 1 $ Stream.enumerateFromTo 1 3
-- (RelTime64 (NanoSecond64 ...),1)
-- (RelTime64 (NanoSecond64 ...),2)
-- (RelTime64 (NanoSecond64 ...),3)
--
-- /Pre-release/
--
{-# INLINE timeIndexWith #-}
timeIndexWith :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m a -> t m (RelTime64, a)
timeIndexWith g stream = Z.zipWith (flip (,)) stream (relTimesWith g)

-- | Pair each element in a stream with relative times starting from 0, using a
-- 10 ms granularity clock. The time is measured just before the element is
-- consumed.
--
-- >>> Stream.mapM_ print $ Stream.timeIndexed $ Stream.delay 1 $ Stream.enumerateFromTo 1 3
-- (RelTime64 (NanoSecond64 ...),1)
-- (RelTime64 (NanoSecond64 ...),2)
-- (RelTime64 (NanoSecond64 ...),3)
--
-- /Pre-release/
--
{-# INLINE timeIndexed #-}
timeIndexed :: (IsStream t, MonadAsync m, Functor (t m))
    => t m a -> t m (RelTime64, a)
timeIndexed = timeIndexWith 0.01

------------------------------------------------------------------------------
-- Searching
------------------------------------------------------------------------------

-- | Find all the indices where the value of the element in the stream is equal
-- to the given value.
--
-- > elemIndices a = findIndices (== a)
--
-- @since 0.5.0
{-# INLINE elemIndices #-}
elemIndices :: (IsStream t, Eq a, Monad m) => a -> t m a -> t m Int
elemIndices a = findIndices (== a)

------------------------------------------------------------------------------
-- Rolling map
------------------------------------------------------------------------------

-- XXX this is not a one-to-one map so calling it map may not be right.
-- We can perhaps call it zipWithTail or rollWith.
--
-- | Apply a function on every two successive elements of a stream. If the
-- stream consists of a single element the output is an empty stream.
--
-- This is the stream equivalent of the list idiom @zipWith f xs (tail xs)@.
--
-- /Pre-release/
--
{-# INLINE rollingMap #-}
rollingMap :: (IsStream t, Monad m) => (a -> a -> b) -> t m a -> t m b
rollingMap f m = fromStreamD $ D.rollingMap f $ toStreamD m

-- | Like 'rollingMap' but with an effectful map function.
--
-- /Pre-release/
--
{-# INLINE rollingMapM #-}
rollingMapM :: (IsStream t, Monad m) => (a -> a -> m b) -> t m a -> t m b
rollingMapM f m = fromStreamD $ D.rollingMapM f $ toStreamD m

------------------------------------------------------------------------------
-- Maybe Streams
------------------------------------------------------------------------------

-- | Map a 'Maybe' returning function to a stream, filter out the 'Nothing'
-- elements, and return a stream of values extracted from 'Just'.
--
-- Equivalent to:
--
-- @
-- mapMaybe f = S.map 'fromJust' . S.filter 'isJust' . S.map f
-- @
--
-- @since 0.3.0
{-# INLINE mapMaybe #-}
mapMaybe :: (IsStream t, Monad m) => (a -> Maybe b) -> t m a -> t m b
mapMaybe f m = fromStreamS $ S.mapMaybe f $ toStreamS m

-- | Like 'mapMaybe' but maps a monadic function.
--
-- Equivalent to:
--
-- @
-- mapMaybeM f = S.map 'fromJust' . S.filter 'isJust' . S.mapM f
-- @
--
-- /Concurrent (do not use with 'parallely' on infinite streams)/
--
-- @since 0.3.0
{-# INLINE_EARLY mapMaybeM #-}
mapMaybeM :: (IsStream t, MonadAsync m, Functor (t m))
          => (a -> m (Maybe b)) -> t m a -> t m b
mapMaybeM f = fmap fromJust . filter isJust . K.mapM f

{-# RULES "mapMaybeM serial" mapMaybeM = mapMaybeMSerial #-}
{-# INLINE mapMaybeMSerial #-}
mapMaybeMSerial :: Monad m => (a -> m (Maybe b)) -> SerialT m a -> SerialT m b
mapMaybeMSerial f m = fromStreamD $ D.mapMaybeM f $ toStreamD m

-- | In a stream of 'Maybe's, discard 'Nothing's and unwrap 'Just's.
--
-- /Pre-release/
--
catMaybes :: (IsStream t, Monad m, Functor (t m)) => t m (Maybe a) -> t m a
catMaybes = fmap fromJust . filter isJust

------------------------------------------------------------------------------
-- Either streams
------------------------------------------------------------------------------

-- | Discard 'Right's and unwrap 'Left's in an 'Either' stream.
--
-- /Pre-release/
--
lefts :: (IsStream t, Monad m, Functor (t m)) => t m (Either a b) -> t m a
lefts = fmap (fromLeft undefined) . filter isLeft

-- | Discard 'Left's and unwrap 'Right's in an 'Either' stream.
--
-- /Pre-release/
--
rights :: (IsStream t, Monad m, Functor (t m)) => t m (Either a b) -> t m b
rights = fmap (fromRight undefined) . filter isRight

------------------------------------------------------------------------------
-- Concurrent Application
------------------------------------------------------------------------------

-- | Parallel transform application operator; applies a stream transformation
-- function @t m a -> t m b@ to a stream @t m a@ concurrently; the input stream
-- is evaluated asynchronously in an independent thread yielding elements to a
-- buffer and the transformation function runs in another thread consuming the
-- input from the buffer.  '|$' is just like regular function application
-- operator '$' except that it is concurrent.
--
-- If you read the signature as @(t m a -> t m b) -> (t m a -> t m b)@ you can
-- look at it as a transformation that converts a transform function to a
-- buffered concurrent transform function.
--
-- The following code prints a value every second even though each stage adds a
-- 1 second delay.
--
--
-- @
-- drain $
--    S.mapM (\\x -> threadDelay 1000000 >> print x)
--      |$ S.repeatM (threadDelay 1000000 >> return 1)
-- @
--
-- /Concurrent/
--
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE (|$) #-}
(|$) :: (IsStream t, MonadAsync m) => (t m a -> t m b) -> (t m a -> t m b)
-- (|$) f = f . Async.mkAsync
(|$) f = f . Par.mkParallel

infixr 0 |$

-- | Same as '|$'.
--
--  /Internal/
--
{-# INLINE applyAsync #-}
applyAsync :: (IsStream t, MonadAsync m)
    => (t m a -> t m b) -> (t m a -> t m b)
applyAsync = (|$)

-- | Parallel reverse function application operator for streams; just like the
-- regular reverse function application operator '&' except that it is
-- concurrent.
--
-- @
-- drain $
--       S.repeatM (threadDelay 1000000 >> return 1)
--    |& S.mapM (\\x -> threadDelay 1000000 >> print x)
-- @
--
-- /Concurrent/
--
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE (|&) #-}
(|&) :: (IsStream t, MonadAsync m) => t m a -> (t m a -> t m b) -> t m b
x |& f = f |$ x

infixl 1 |&
