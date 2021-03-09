{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Generate
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Most of the combinators in this module can be implemented as unfolds. Some
-- of them however can only be expressed in terms StreamK e.g. cons/consM,
-- fromFoldable, mfix. We can possibly remove those from this module which can
-- be expressed as unfolds. Unless we want to use rewrite rules to rewrite them
-- as StreamK when StreamK is used, avoiding conversion to StreamD. Will that
-- help? Are there any other reasons to keep these and not use unfolds?

module Streamly.Internal.Data.Stream.IsStream.Generate
    (
    -- * Primitives
      K.nil
    , K.nilM
    , K.cons
    , (K..:)

    , consM
    , (|:)

    -- * From 'Unfold'
    , unfold
    , unfold0

    -- * Unfolding
    , unfoldr
    , unfoldrM

    -- * From Values
    , value
    , yieldM
    , repeat
    , repeatM
    , replicate
    , replicateM

    -- * Enumeration
    , Enumerable (..)
    , enumerate
    , enumerateTo

    -- * Time Enumeration
    , times
    , absTimes
    , absTimesWith
    , relTimes
    , relTimesWith
    , durations
    , ticks
    , timeout

    -- * From Generators
    , fromIndices
    , fromIndicesM
    -- , generate
    -- , generateM

    -- * Iteration
    , iterate
    , iterateM

    -- * Cyclic Elements
    , K.mfix

    -- * From Containers
    , P.fromList
    , fromListM
    , K.fromFoldable
    , fromFoldableM
    , fromCallback

    -- * Deprecated
    , K.once
    , yield
    , each
    , fromHandle
    , currentTime
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Data.Void (Void)
import Streamly.Internal.Data.Unfold.Types (Unfold)
import Streamly.Internal.Data.SVar (MonadAsync, Rate (..))
import Streamly.Internal.Data.Stream.IsStream.Enumeration
    (Enumerable(..), enumerate, enumerateTo)
import Streamly.Internal.Data.Stream.IsStream.Common
    (absTimesWith, concatM, relTimesWith, timesWith, value, yieldM, repeatM)
import Streamly.Internal.Data.Stream.Prelude (fromStreamS)
import Streamly.Internal.Data.Stream.StreamD (fromStreamD)
import Streamly.Internal.Data.Stream.StreamK (IsStream((|:), consM))
import Streamly.Internal.Data.Stream.Serial (SerialT, WSerialT)
import Streamly.Internal.Data.Stream.Zip (ZipSerialM)
import Streamly.Internal.Data.Time.Units (AbsTime , RelTime64, addToAbsTime64)

import qualified Streamly.Internal.Data.Stream.Parallel as Par
import qualified Streamly.Internal.Data.Stream.Prelude as P
import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Stream.StreamD.Generate as D
import qualified Streamly.Internal.Data.Stream.StreamK as K
#ifdef USE_STREAMK_ONLY
import qualified Streamly.Internal.Data.Stream.StreamK as S
#else
import qualified Streamly.Internal.Data.Stream.StreamD.Generate as S
#endif
import qualified System.IO as IO

import Prelude hiding (iterate, replicate, repeat)

-- $setup
-- >>> :m
-- >>> import Prelude hiding (iterate, replicate, repeat)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import Control.Concurrent (threadDelay)

------------------------------------------------------------------------------
-- From Unfold
------------------------------------------------------------------------------

-- | Convert an 'Unfold' into a stream by supplying it an input seed.
--
-- >>> Stream.drain $ Stream.unfold (Unfold.replicateM 3) (putStrLn "hello")
-- hello
-- hello
-- hello
--
-- /Since: 0.7.0/
{-# INLINE unfold #-}
unfold :: (IsStream t, Monad m) => Unfold m a b -> a -> t m b
unfold unf x = fromStreamD $ D.unfold unf x

-- | Convert an 'Unfold' with a closed input end into a stream.
--
-- /Pre-release/
{-# INLINE unfold0 #-}
unfold0 :: (IsStream t, Monad m) => Unfold m Void b -> t m b
unfold0 unf = unfold unf (error "unfold0: unexpected void evaluation")

------------------------------------------------------------------------------
-- Generation by Unfolding
------------------------------------------------------------------------------

-- |
-- @
-- unfoldr step s =
--     case step s of
--         Nothing -> 'K.nil'
--         Just (a, b) -> a \`cons` unfoldr step b
-- @
--
-- Build a stream by unfolding a /pure/ step function @step@ starting from a
-- seed @s@.  The step function returns the next element in the stream and the
-- next seed value. When it is done it returns 'Nothing' and the stream ends.
-- For example,
--
-- @
-- let f b =
--         if b > 3
--         then Nothing
--         else Just (b, b + 1)
-- in toList $ unfoldr f 0
-- @
-- @
-- [0,1,2,3]
-- @
--
-- @since 0.1.0
{-# INLINE_EARLY unfoldr #-}
unfoldr :: (Monad m, IsStream t) => (b -> Maybe (a, b)) -> b -> t m a
unfoldr step seed = fromStreamS (S.unfoldr step seed)
{-# RULES "unfoldr fallback to StreamK" [1]
    forall a b. S.toStreamK (S.unfoldr a b) = K.unfoldr a b #-}

-- | Build a stream by unfolding a /monadic/ step function starting from a
-- seed.  The step function returns the next element in the stream and the next
-- seed value. When it is done it returns 'Nothing' and the stream ends. For
-- example,
--
-- @
-- let f b =
--         if b > 3
--         then return Nothing
--         else print b >> return (Just (b, b + 1))
-- in drain $ unfoldrM f 0
-- @
-- @
--  0
--  1
--  2
--  3
-- @
-- When run concurrently, the next unfold step can run concurrently with the
-- processing of the output of the previous step.  Note that more than one step
-- cannot run concurrently as the next step depends on the output of the
-- previous step.
--
-- @
-- (asyncly $ S.unfoldrM (\\n -> liftIO (threadDelay 1000000) >> return (Just (n, n + 1))) 0)
--     & S.foldlM' (\\_ a -> threadDelay 1000000 >> print a) ()
-- @
--
-- /Concurrent/
--
-- /Since: 0.1.0/
{-# INLINE_EARLY unfoldrM #-}
unfoldrM :: (IsStream t, MonadAsync m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM = K.unfoldrM

{-# RULES "unfoldrM serial" unfoldrM = unfoldrMSerial #-}
{-# INLINE_EARLY unfoldrMSerial #-}
unfoldrMSerial :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> SerialT m a
unfoldrMSerial = Serial.unfoldrM

{-# RULES "unfoldrM wSerial" unfoldrM = unfoldrMWSerial #-}
{-# INLINE_EARLY unfoldrMWSerial #-}
unfoldrMWSerial :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> WSerialT m a
unfoldrMWSerial = Serial.unfoldrM

{-# RULES "unfoldrM zipSerial" unfoldrM = unfoldrMZipSerial #-}
{-# INLINE_EARLY unfoldrMZipSerial #-}
unfoldrMZipSerial :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> ZipSerialM m a
unfoldrMZipSerial = Serial.unfoldrM

------------------------------------------------------------------------------
-- From Values
------------------------------------------------------------------------------

-- |
-- Generate an infinite stream by repeating a pure value.
--
-- @since 0.4.0
{-# INLINE_NORMAL repeat #-}
repeat :: (IsStream t, Monad m) => a -> t m a
repeat = fromStreamS . S.repeat

-- |
-- @
-- replicate = take n . repeat
-- @
--
-- Generate a stream of length @n@ by repeating a value @n@ times.
--
-- @since 0.6.0
{-# INLINE_NORMAL replicate #-}
replicate :: (IsStream t, Monad m) => Int -> a -> t m a
replicate n = fromStreamS . S.replicate n

-- |
-- @
-- replicateM = take n . repeatM
-- @
--
-- Generate a stream by performing a monadic action @n@ times. Same as:
--
-- @
-- drain $ serially $ S.replicateM 10 $ (threadDelay 1000000 >> print 1)
-- drain $ asyncly  $ S.replicateM 10 $ (threadDelay 1000000 >> print 1)
-- @
--
-- /Concurrent/
--
-- @since 0.1.1
{-# INLINE_EARLY replicateM #-}
replicateM :: (IsStream t, MonadAsync m) => Int -> m a -> t m a
replicateM = K.replicateM

{-# RULES "replicateM serial" replicateM = replicateMSerial #-}
{-# INLINE replicateMSerial #-}
replicateMSerial :: MonadAsync m => Int -> m a -> SerialT m a
replicateMSerial n = fromStreamS . S.replicateM n

------------------------------------------------------------------------------
-- Time Enumeration
------------------------------------------------------------------------------

-- | @times@ returns a stream of time value tuples with clock of 10 ms
-- granularity. The first component of the tuple is an absolute time reference
-- (epoch) denoting the start of the stream and the second component is a time
-- relative to the reference.
--
-- >>> Stream.mapM_ (\x -> print x >> threadDelay 1000000) $ Stream.take 3 $ Stream.times
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE times #-}
times :: (IsStream t, MonadAsync m) => t m (AbsTime, RelTime64)
times = timesWith 0.01

-- | @absTimes@ returns a stream of absolute timestamps using a clock of 10 ms
-- granularity.
--
-- >>> Stream.mapM_ print $ Stream.delayPre 1 $ Stream.take 3 $ Stream.absTimes
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE absTimes #-}
absTimes :: (IsStream t, MonadAsync m, Functor (t m)) => t m AbsTime
absTimes = fmap (uncurry addToAbsTime64) times

{-# DEPRECATED currentTime "Please use absTimes instead" #-}
{-# INLINE currentTime #-}
currentTime :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m AbsTime
currentTime = absTimesWith

-- | @relTimes@ returns a stream of relative time values starting from 0,
-- using a clock of granularity 10 ms.
--
-- >>> Stream.mapM_ print $ Stream.delayPre 1 $ Stream.take 3 $ Stream.relTimes
-- RelTime64 (NanoSecond64 ...)
-- RelTime64 (NanoSecond64 ...)
-- RelTime64 (NanoSecond64 ...)
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE relTimes #-}
relTimes :: (IsStream t, MonadAsync m, Functor (t m)) => t m RelTime64
relTimes = fmap snd times

-- | @durations g@ returns a stream of relative time values measuring the time
-- elapsed since the immediate predecessor element of the stream was generated.
-- The first element of the stream is always 0. @durations@ uses a clock of
-- granularity @g@ specified in seconds. A low granularity clock is more
-- expensive in terms of CPU usage. The minimum granularity is 1 millisecond.
-- Durations lower than 1 ms will be 0.
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Unimplemented/
--
{-# INLINE durations #-}
durations :: -- (IsStream t, MonadAsync m) =>
    Double -> t m RelTime64
durations = undefined

-- | Generate ticks at the specified rate. The rate is adaptive, the tick
-- generation speed can be increased or decreased at different times to achieve
-- the specified rate.  The specific behavior for different styles of 'Rate'
-- specifications is documented under 'Rate'.  The effective maximum rate
-- achieved by a stream is governed by the processor speed.
--
-- /Unimplemented/
--
{-# INLINE ticks #-}
ticks :: -- (IsStream t, MonadAsync m) =>
    Rate -> t m ()
ticks = undefined

-- | Generate a singleton event at or after the specified absolute time. Note
-- that this is different from a threadDelay, a threadDelay starts from the
-- time when the action is evaluated, whereas if we use AbsTime based timeout
-- it will immediately expire if the action is evaluated too late.
--
-- /Unimplemented/
--
{-# INLINE timeout #-}
timeout :: -- (IsStream t, MonadAsync m) =>
    AbsTime -> t m ()
timeout = undefined

------------------------------------------------------------------------------
-- From Generator functions
------------------------------------------------------------------------------

-- XXX we can remove it and recommend the definition in terms of enumerate and
-- map. Check performance equivalence.
--
-- |
-- @
-- fromIndices f = fmap f $ Stream.enumerateFrom 0
-- fromIndices f = let g i = f i \`cons` g (i + 1) in g 0
-- @
--
-- Generate an infinite stream, whose values are the output of a function @f@
-- applied on the corresponding index.  Index starts at 0.
--
-- >>> Stream.toList $ Stream.take 5 $ Stream.fromIndices id
-- [0,1,2,3,4]
--
-- @since 0.6.0
{-# INLINE fromIndices #-}
fromIndices :: (IsStream t, Monad m) => (Int -> a) -> t m a
fromIndices = fromStreamS . S.fromIndices

--
-- |
-- @
-- fromIndicesM f = Stream.mapM f $ Stream.enumerateFrom 0
-- fromIndicesM f = let g i = f i \`consM` g (i + 1) in g 0
-- @
--
-- Generate an infinite stream, whose values are the output of a monadic
-- function @f@ applied on the corresponding index. Index starts at 0.
--
-- /Concurrent/
--
-- @since 0.6.0
{-# INLINE_EARLY fromIndicesM #-}
fromIndicesM :: (IsStream t, MonadAsync m) => (Int -> m a) -> t m a
fromIndicesM = K.fromIndicesM

{-# RULES "fromIndicesM serial" fromIndicesM = fromIndicesMSerial #-}
{-# INLINE fromIndicesMSerial #-}
fromIndicesMSerial :: MonadAsync m => (Int -> m a) -> SerialT m a
fromIndicesMSerial = fromStreamS . S.fromIndicesM

------------------------------------------------------------------------------
-- Iterating functions
------------------------------------------------------------------------------

-- |
-- @
-- iterate f x = x \`cons` iterate f x
-- @
--
-- Generate an infinite stream with @x@ as the first element and each
-- successive element derived by applying the function @f@ on the previous
-- element.
--
-- @
-- > S.toList $ S.take 5 $ S.iterate (+1) 1
-- [1,2,3,4,5]
-- @
--
-- @since 0.1.2
{-# INLINE_NORMAL iterate #-}
iterate :: (IsStream t, Monad m) => (a -> a) -> a -> t m a
iterate step = fromStreamS . S.iterate step

-- |
-- @
-- iterateM f m = m >>= \a -> return a \`consM` iterateM f (f a)
-- @
--
-- Generate an infinite stream with the first element generated by the action
-- @m@ and each successive element derived by applying the monadic function
-- @f@ on the previous element.
--
-- When run concurrently, the next iteration can run concurrently with the
-- processing of the previous iteration. Note that more than one iteration
-- cannot run concurrently as the next iteration depends on the output of the
-- previous iteration.
--
-- @
-- drain $ serially $ S.take 10 $ S.iterateM
--      (\\x -> threadDelay 1000000 >> print x >> return (x + 1)) (return 0)
--
-- drain $ asyncly  $ S.take 10 $ S.iterateM
--      (\\x -> threadDelay 1000000 >> print x >> return (x + 1)) (return 0)
-- @
--
-- /Concurrent/
--
-- /Since: 0.1.2/
--
-- /Since: 0.7.0 (signature change)/
{-# INLINE_EARLY iterateM #-}
iterateM :: (IsStream t, MonadAsync m) => (a -> m a) -> m a -> t m a
iterateM = K.iterateM

{-# RULES "iterateM serial" iterateM = iterateMSerial #-}
{-# INLINE iterateMSerial #-}
iterateMSerial :: MonadAsync m => (a -> m a) -> m a -> SerialT m a
iterateMSerial step = fromStreamS . S.iterateM step

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- |
-- @
-- fromFoldableM = 'Prelude.foldr' 'consM' 'K.nil'
-- @
--
-- Construct a stream from a 'Foldable' containing monadic actions.
--
-- @
-- drain $ serially $ S.fromFoldableM $ replicateM 10 (threadDelay 1000000 >> print 1)
-- drain $ asyncly  $ S.fromFoldableM $ replicateM 10 (threadDelay 1000000 >> print 1)
-- @
--
-- /Concurrent (do not use with 'parallely' on infinite containers)/
--
-- @since 0.3.0
{-# INLINE fromFoldableM #-}
fromFoldableM :: (IsStream t, MonadAsync m, Foldable f) => f (m a) -> t m a
fromFoldableM = Prelude.foldr consM K.nil

-- |
-- @
-- fromListM = 'Prelude.foldr' 'K.consM' 'K.nil'
-- @
--
-- Construct a stream from a list of monadic actions. This is more efficient
-- than 'fromFoldableM' for serial streams.
--
-- @since 0.4.0
{-# INLINE_EARLY fromListM #-}
fromListM :: (MonadAsync m, IsStream t) => [m a] -> t m a
fromListM = fromFoldableM
{-# RULES "fromListM fallback to StreamK" [1]
    forall a. D.toStreamK (D.fromListM a) = fromFoldableM a #-}

{-# RULES "fromListM serial" fromListM = fromListMSerial #-}
{-# INLINE_EARLY fromListMSerial #-}
fromListMSerial :: MonadAsync m => [m a] -> SerialT m a
fromListMSerial = fromStreamD . D.fromListM

-- | Same as 'fromFoldable'.
--
-- @since 0.1.0
{-# DEPRECATED each "Please use fromFoldable instead." #-}
{-# INLINE each #-}
each :: (IsStream t, Foldable f) => f a -> t m a
each = K.fromFoldable

-- | Read lines from an IO Handle into a stream of Strings.
--
-- @since 0.1.0
{-# DEPRECATED fromHandle
   "Please use Streamly.FileSystem.Handle module (see the changelog)" #-}
fromHandle :: (IsStream t, MonadIO m) => IO.Handle -> t m String
fromHandle h = go
  where
  go = K.mkStream $ \_ yld _ stp -> do
        eof <- liftIO $ IO.hIsEOF h
        if eof
        then stp
        else do
            str <- liftIO $ IO.hGetLine h
            yld str go

-- XXX This should perhaps be moved to Parallel
--
-- | Takes a callback setter function and provides it with a callback.  The
-- callback when invoked adds a value at the tail of the stream. Returns a
-- stream of values generated by the callback.
--
-- /Pre-release/
--
{-# INLINE fromCallback #-}
fromCallback :: MonadAsync m => ((a -> m ()) -> m ()) -> SerialT m a
fromCallback setCallback = concatM $ do
    (callback, stream) <- Par.newCallbackStream
    setCallback callback
    return stream

------------------------------------------------------------------------------
-- Deprecated apis
------------------------------------------------------------------------------

-- | Same as valueM
--
-- @since 0.4.0
{-# DEPRECATED yield "Please use value instead." #-}
{-# INLINE yield #-}
yield :: IsStream t => a -> t m a
yield = value
