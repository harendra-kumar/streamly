{-# OPTIONS_GHC -Wno-orphans  #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Common
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Bottom level IsStream module that can be used by all other upper level
-- IsStream modules.

module Streamly.Internal.Data.Stream.IsStream.Common
    (
    -- * Generation
      value
    , valueM
    , repeatM
    , timesWith
    , absTimesWith
    , relTimesWith

    -- * Elimination
    , fold

    -- * Transformation
    , scanlMAfter'
    , postscanlM'
    , smapM
    -- $smapM_Notes
    , take
    , takeWhile
    , drop
    , findIndices
    , intersperseM
    , interjectSuffix
    , reverse
    , reverse'

    -- * Nesting
    , concatM
    , concatMapM
    , splitOnSeq
    )
where

#include "inline.hs"

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Storable (Storable)
import Streamly.Internal.Data.Array.Foreign.Types (Array)
import Streamly.Internal.Data.Fold.Types (Fold (..))
import Streamly.Internal.Data.Stream.IsStream.Combinators (maxYields)
import Streamly.Internal.Data.Stream.Prelude (fromStreamS, toStreamS)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Stream.StreamD.Type (fromStreamD, toStreamD)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream())
import Streamly.Internal.Data.SVar (MonadAsync)
import Streamly.Internal.Data.Time.Units (AbsTime, RelTime64, addToAbsTime64)

import qualified Streamly.Internal.Data.Stream.Parallel as Par
import qualified Streamly.Internal.Data.Stream.Prelude as P
import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Stream.StreamK as K (repeatM)
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Stream.StreamD as D
#ifdef USE_STREAMK_ONLY
import qualified Streamly.Internal.Data.Stream.StreamK as S
#else
import qualified Streamly.Internal.Data.Stream.StreamD as S
#endif

import Prelude hiding (take, takeWhile, drop, reverse)

--
-- $setup
-- >>> :m
-- >>> import Prelude hiding (take, takeWhile, drop, reverse)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Array.Foreign as Array

------------------------------------------------------------------------------
-- Generation
------------------------------------------------------------------------------

-- Faster than valueM because there is no bind.
--
-- |
-- @
-- value a = a \`cons` nil
-- @
--
-- Create a singleton stream from a pure value.
--
-- The following holds in monadic streams, but not in Zip streams:
--
-- @
-- value = pure
-- value = valueM . pure
-- @
--
-- In Zip applicative streams 'value' is not the same as 'pure' because in that
-- case 'pure' is equivalent to 'repeat' instead. 'value' and 'pure' are
-- equally efficient, in other cases 'value' may be slightly more efficient
-- than the other equivalent definitions.
--
-- @since 0.8.0
{-# INLINE value #-}
value :: IsStream t => a -> t m a
value = K.value

-- |
-- @
-- valueM m = m \`consM` nil
-- @
--
-- Create a singleton stream from a monadic action.
--
-- @
-- > Stream.toList $ Stream.valueM getLine
-- hello
-- ["hello"]
-- @
--
-- @since 0.8.0
{-# INLINE valueM #-}
valueM :: (Monad m, IsStream t) => m a -> t m a
valueM = K.valueM

-- |
-- @
-- repeatM = fix . consM
-- repeatM = cycle1 . valueM
-- @
--
-- Generate a stream by repeatedly executing a monadic action forever.
--
-- @
-- drain $ serially $ S.take 10 $ S.repeatM $ (threadDelay 1000000 >> print 1)
-- drain $ asyncly  $ S.take 10 $ S.repeatM $ (threadDelay 1000000 >> print 1)
-- @
--
-- /Concurrent, infinite (do not use with 'parallely')/
--
-- @since 0.2.0
{-# INLINE_EARLY repeatM #-}
repeatM :: (IsStream t, MonadAsync m) => m a -> t m a
repeatM = K.repeatM

{-# RULES "repeatM serial" repeatM = repeatMSerial #-}
{-# INLINE repeatMSerial #-}
repeatMSerial :: MonadAsync m => m a -> SerialT m a
repeatMSerial = fromStreamS . S.repeatM

------------------------------------------------------------------------------
-- Generation - Time related
------------------------------------------------------------------------------

-- | @timesWith g@ returns a stream of time value tuples. The first component
-- of the tuple is an absolute time reference (epoch) denoting the start of the
-- stream and the second component is a time relative to the reference.
--
-- The argument @g@ specifies the granularity of the relative time in seconds.
-- A lower granularity clock gives higher precision but is more expensive in
-- terms of CPU usage. Any granularity lower than 1 ms is treated as 1 ms.
--
-- >>> import Control.Concurrent (threadDelay)
-- >>> import Streamly.Internal.Data.Stream.IsStream.Common as Stream (timesWith)
-- >>> Stream.mapM_ (\x -> print x >> threadDelay 1000000) $ Stream.take 3 $ Stream.timesWith 0.01
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE timesWith #-}
timesWith :: (IsStream t, MonadAsync m) => Double -> t m (AbsTime, RelTime64)
timesWith g = fromStreamD $ D.times g

-- | @absTimesWith g@ returns a stream of absolute timestamps using a clock of
-- granularity @g@ specified in seconds. A low granularity clock is more
-- expensive in terms of CPU usage.  Any granularity lower than 1 ms is treated
-- as 1 ms.
--
-- >>> Stream.mapM_ print $ Stream.delayPre 1 $ Stream.take 3 $ absTimesWith 0.01
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE absTimesWith #-}
absTimesWith :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m AbsTime
absTimesWith = fmap (uncurry addToAbsTime64) . timesWith

-- | @relTimesWith g@ returns a stream of relative time values starting from 0,
-- using a clock of granularity @g@ specified in seconds. A low granularity
-- clock is more expensive in terms of CPU usage.  Any granularity lower than 1
-- ms is treated as 1 ms.
--
-- >>> Stream.mapM_ print $ Stream.delayPre 1 $ Stream.take 3 $ Stream.relTimesWith 0.01
-- RelTime64 (NanoSecond64 ...)
-- RelTime64 (NanoSecond64 ...)
-- RelTime64 (NanoSecond64 ...)
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE relTimesWith #-}
relTimesWith :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m RelTime64
relTimesWith = fmap snd . timesWith

------------------------------------------------------------------------------
-- Elimination - Running a Fold
------------------------------------------------------------------------------

-- | Fold a stream using the supplied left 'Fold' and reducing the resulting
-- expression strictly at each step. The behavior is similar to 'foldl''. A
-- 'Fold' can terminate early without consuming the full stream. See the
-- documentation of individual 'Fold's for termination behavior.
--
-- >>> Stream.fold Fold.sum (Stream.enumerateFromTo 1 100)
-- 5050
--
-- @fold f = parse (Parser.fromFold f)@
--
-- Folds can never fail, therefore, they always produce a default value even
-- when no input is provided. It means we can always fold an empty stream and
-- get a valid result.  For example:
--
-- >>> Stream.fold Fold.sum Stream.nil
-- 0
--
-- However, 'foldMany' on an empty stream results in an empty stream.
-- Therefore, @fold f@ is not the same as @head . foldMany f@.
--
-- @since 0.7.0
{-# INLINE fold #-}
fold :: Monad m => Fold m a b -> SerialT m a -> m b
fold = P.foldOnce

------------------------------------------------------------------------------
-- Transformation
------------------------------------------------------------------------------

-- | @scanlMAfter' accumulate initial done stream@ is like 'scanlM'' except
-- that it provides an additional @done@ function to be applied on the
-- accumulator when the stream stops. The result of @done@ is also emitted in
-- the stream.
--
-- This function can be used to allocate a resource in the beginning of the
-- scan and release it when the stream ends or to flush the internal state of
-- the scan at the end.
--
-- /Pre-release/
--
{-# INLINE scanlMAfter' #-}
scanlMAfter' :: (IsStream t, Monad m)
    => (b -> a -> m b) -> m b -> (b -> m b) -> t m a -> t m b
scanlMAfter' step initial done stream =
    fromStreamD $ D.scanlMAfter' step initial done $ toStreamD stream

-- XXX this needs to be concurrent
-- | Like 'postscanl'' but with a monadic step function and a monadic seed.
--
-- /Since: 0.7.0/
--
-- /Since: 0.8.0 (signature change)/
{-# INLINE postscanlM' #-}
postscanlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> m b -> t m a -> t m b
postscanlM' step z m = fromStreamD $ D.postscanlM' step z $ toStreamD m

-- $smapM_Notes
--
-- The stateful step function can be simplified to @(s -> a -> m b)@ to provide
-- a read-only environment. However, that would just be 'mapM'.
--
-- The initial action could be @m (s, Maybe b)@, and we can also add a final
-- action @s -> m (Maybe b)@. This can be used to get pre/post scan like
-- functionality and also to flush the state in the end like scanlMAfter'.
-- We can also use it along with a fusible version of bracket to get
-- scanlMAfter' like functionality. See issue #677.
--
-- This can be further generalized to a type similar to Fold/Parser, giving it
-- filtering and parsing capability as well (this is in fact equivalent to
-- parseMany):
--
-- smapM :: (s -> a -> m (Step s b)) -> m s -> t m a -> t m b
--

-- | A stateful 'mapM', equivalent to a left scan, more like mapAccumL.
-- Hopefully, this is a better alternative to @scan@. Separation of state from
-- the output makes it easier to think in terms of a shared state, and also
-- makes it easier to keep the state fully strict and the output lazy.
--
-- See also: 'scanlM''
--
-- /Pre-release/
--
{-# INLINE smapM #-}
smapM :: (IsStream t, Monad m) =>
       (s -> a -> m (s, b))
    -> m s
    -> t m a
    -> t m b
smapM step initial stream =
    -- XXX implement this directly instead of using scanlM'
    -- Once we have postscanlM' with monadic initial we can use this code
    -- let r = postscanlM'
    --              (\(s, _) a -> step s a)
    --              (fmap (,undefined) initial)
    --              stream
    let r = postscanlM'
                (\(s, _) a -> step s a)
                (fmap (,undefined) initial)
                stream
     in Serial.map snd r

------------------------------------------------------------------------------
-- Transformation - Trimming
------------------------------------------------------------------------------

-- | Take first 'n' elements from the stream and discard the rest.
--
-- @since 0.1.0
{-# INLINE take #-}
take :: (IsStream t, Monad m) => Int -> t m a -> t m a
take n m = fromStreamS $ S.take n $ toStreamS
    (maxYields (Just (fromIntegral n)) m)

-- | End the stream as soon as the predicate fails on an element.
--
-- @since 0.1.0
{-# INLINE takeWhile #-}
takeWhile :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m a
takeWhile p m = fromStreamS $ S.takeWhile p $ toStreamS m

-- | Discard first 'n' elements from the stream and take the rest.
--
-- @since 0.1.0
{-# INLINE drop #-}
drop :: (IsStream t, Monad m) => Int -> t m a -> t m a
drop n m = fromStreamS $ S.drop n $ toStreamS m

------------------------------------------------------------------------------
-- Searching
------------------------------------------------------------------------------

-- | Find all the indices where the element in the stream satisfies the given
-- predicate.
--
-- > findIndices = fold Fold.findIndices
--
-- @since 0.5.0
{-# INLINE findIndices #-}
findIndices :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m Int
findIndices p m = fromStreamS $ S.findIndices p (toStreamS m)

------------------------------------------------------------------------------
-- Transformation by Inserting
------------------------------------------------------------------------------

-- intersperseM = intersperseBySpan 1

-- | Insert an effect and its output before consuming an element of a stream
-- except the first one.
--
-- >>> Stream.toList $ Stream.trace putChar $ Stream.intersperseM (putChar '.' >> return ',') $ Stream.fromList "hello"
-- h.,e.,l.,l.,o"h,e,l,l,o"
--
-- @since 0.5.0
{-# INLINE intersperseM #-}
intersperseM :: (IsStream t, MonadAsync m) => m a -> t m a -> t m a
intersperseM m = fromStreamS . S.intersperseM m . toStreamS

-- | Intersperse a monadic action into the input stream after every @n@
-- seconds.
--
-- @
-- > import Control.Concurrent (threadDelay)
-- > Stream.drain $ Stream.interjectSuffix 1 (putChar ',') $ Stream.mapM (\x -> threadDelay 1000000 >> putChar x) $ Stream.fromList "hello"
-- h,e,l,l,o
-- @
--
-- /Pre-release/
{-# INLINE interjectSuffix #-}
interjectSuffix
    :: (IsStream t, MonadAsync m)
    => Double -> m a -> t m a -> t m a
interjectSuffix n f xs = xs `Par.parallelFst` repeatM timed
    where timed = liftIO (threadDelay (round $ n * 1000000)) >> f

------------------------------------------------------------------------------
-- Transformation by Reordering
------------------------------------------------------------------------------

-- XXX Use a compact region list to temporarily store the list, in both reverse
-- as well as in reverse'.
--
-- /Note:/ 'reverse'' is much faster than this, use that when performance
-- matters.
--
-- > reverse = S.foldlT (flip S.cons) S.nil
--
-- | Returns the elements of the stream in reverse order.  The stream must be
-- finite. Note that this necessarily buffers the entire stream in memory.
--
-- /Since 0.7.0 (Monad m constraint)/
--
-- /Since: 0.1.1/
{-# INLINE reverse #-}
reverse :: (IsStream t, Monad m) => t m a -> t m a
reverse s = fromStreamS $ S.reverse $ toStreamS s

-- | Like 'reverse' but several times faster, requires a 'Storable' instance.
--
-- /Pre-release/
{-# INLINE reverse' #-}
reverse' :: (IsStream t, MonadIO m, Storable a) => t m a -> t m a
reverse' s = fromStreamD $ D.reverse' $ toStreamD s

------------------------------------------------------------------------------
-- Combine streams and flatten
------------------------------------------------------------------------------

-- | Map a stream producing monadic function on each element of the stream
-- and then flatten the results into a single stream. Since the stream
-- generation function is monadic, unlike 'concatMap', it can produce an
-- effect at the beginning of each iteration of the inner loop.
--
-- @since 0.6.0
{-# INLINE concatMapM #-}
concatMapM :: (IsStream t, Monad m) => (a -> m (t m b)) -> t m a -> t m b
concatMapM f m = fromStreamD $ D.concatMapM (fmap toStreamD . f) (toStreamD m)

-- | Given a stream value in the underlying monad, lift and join the underlying
-- monad with the stream monad.
--
-- @
-- concatM = concat . valueM
-- concatM = concat . lift    -- requires @(MonadTrans t)@
-- concatM = join . lift      -- requires @(MonadTrans t@, @Monad (t m))@
-- @
--
-- See also: 'concat', 'sequence'
--
--  /Internal/
--
{-# INLINE concatM #-}
concatM :: (IsStream t, Monad m) => m (t m a) -> t m a
concatM generator = concatMapM (\() -> generator) (value ())

------------------------------------------------------------------------------
-- Split stream and fold
------------------------------------------------------------------------------

-- | Like 'splitOn' but the separator is a sequence of elements instead of a
-- single element.
--
-- For illustration, let's define a function that operates on pure lists:
--
-- >>> splitOnSeq' pat xs = Stream.toList $ Stream.splitOnSeq (Array.fromList pat) Fold.toList (Stream.fromList xs)
--
-- >>> splitOnSeq' "" "hello"
-- ["h","e","l","l","o"]
--
-- >>> splitOnSeq' "hello" ""
-- [""]
--
-- >>> splitOnSeq' "hello" "hello"
-- ["",""]
--
-- >>> splitOnSeq' "x" "hello"
-- ["hello"]
--
-- >>> splitOnSeq' "h" "hello"
-- ["","ello"]
--
-- >>> splitOnSeq' "o" "hello"
-- ["hell",""]
--
-- >>> splitOnSeq' "e" "hello"
-- ["h","llo"]
--
-- >>> splitOnSeq' "l" "hello"
-- ["he","","o"]
--
-- >>> splitOnSeq' "ll" "hello"
-- ["he","o"]
--
-- 'splitOnSeq' is an inverse of 'intercalate'. The following law always holds:
--
-- > intercalate . splitOn == id
--
-- The following law holds when the separator is non-empty and contains none of
-- the elements present in the input lists:
--
-- > splitOn . intercalate == id
--
-- /Pre-release/

-- XXX We can use a polymorphic vector implemented by Array# to represent the
-- sequence, that way we can avoid the Storable constraint. If we still need
-- Storable Array for performance, we can use a separate splitOnArray API for
-- that. We can also have an API where the sequence itself is a lazy stream, so
-- that we can search files in files for example.
{-# INLINE splitOnSeq #-}
splitOnSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitOnSeq patt f m = D.fromStreamD $ D.splitOnSeq patt f (D.toStreamD m)
