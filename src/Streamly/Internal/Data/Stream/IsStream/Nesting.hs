-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Nesting
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Transformations involving multiple streams, streams and unfolds or streams
-- and folds.

module Streamly.Internal.Data.Stream.IsStream.Nesting
    (
    -- * Generate
    -- | Combining streams to generate streams.

    -- ** Combine Two Streams
    -- | Functions ending in the shape:
    --
    -- @t m a -> t m a -> t m a@.

    -- *** Appending
      serial
    , append

    -- *** Interleaving
    , interleave
    , interleaveMin
    , interleaveSuffix
    , interleaveInfix

    , wSerial
    , Serial.wSerialFst
    , Serial.wSerialMin

    -- *** Scheduling
    , ahead
    , async
    , wAsync
    , roundrobin

    -- *** Parallel
    , parallel
    , Par.parallelFst
    , Par.parallelMin

    -- *** Zipping
    , Z.zipWith
    , Z.zipWithM
    , Z.zipAsyncWith
    , Z.zipAsyncWithM

    -- *** Merging
    -- , merge
    , mergeBy
    , mergeByM
    , mergeAsyncBy
    , mergeAsyncByM

    -- *** Trimming
    , dropPrefix
    , dropInfix
    , dropSuffix

    -- ** Combine N Streams
    -- | Functions generally ending in these shapes:
    --
    -- @
    -- concat: f (t m a) -> t m a
    -- concatMap: (a -> t m b) -> t m a -> t m b
    -- concatUnfold: Unfold m a b -> t m a -> t m b
    -- @

    -- *** Flatten Containers of Streams
    , concatM
    , concat
    , concatFoldableWith

    -- *** ConcatMap
    -- | Map and flatten streams.
    , concatMapFoldableWith
    , concatForFoldableWith
    , concatMap
    , concatMapM
    , concatMapWith
    , concatSmapMWith
    , K.bindWith
    , concatPairsWith

    -- *** ConcatUnfold
    -- | Unfold and flatten streams.
    , concatUnfold
    , concatUnfoldInterleave
    , concatUnfoldRoundrobin

    -- *** Interpose
    -- | Insert effects between streams. Like concatUnfold but intersperses an
    -- effect between the streams. A
    -- special case of gintercalate.
    , interpose
    , interposeSuffix
    -- , interposeBy

    -- *** Intercalate
    -- | Insert Streams between Streams.
    -- Like concatUnfold but intersperses streams from another source between
    -- the streams from the first source.
    , intercalate
    , intercalateSuffix
    , gintercalate
    , gintercalateSuffix

    -- *** IterateMap
    -- | Map and flatten Trees of Streams
    , iterateMapWith
    , iterateSmapMWith
    , iterateMapLeftsWith

    -- * Eliminate
    -- | Folding and Parsing chunks of streams to eliminate nested streams.
    -- Functions generally ending in these shapes:
    --
    -- @
    -- f (Fold m a b) -> t m a -> t m b
    -- f (Parser m a b) -> t m a -> t m b
    -- @

    -- ** Folding
    -- | Apply folds on a stream.
    , foldMany
    , foldManyPost
    , foldSequence
    , foldIterateM

    -- ** Parsing
    -- | Apply parsers on a stream.
    , parseMany
    , parseManyD
    , parseManyTill
    , parseSequence
    , parseIterate

    -- ** Chunking
    -- | Element unaware grouping.
    , chunksOf
    , chunksOf2
    , arraysOf
    , intervalsOf

    -- ** Grouping
    -- In imperative terms, grouped folding can be considered as a nested loop
    -- where we loop over the stream to group elements and then loop over
    -- individual groups to fold them to a single value that is yielded in the
    -- output stream.

    , groups
    , groupsBy
    , groupsByRolling

    -- -- *** Searching Sequences
    -- , seqIndices -- search a sequence in the stream

    -- -- *** Searching Multiple Sequences
    -- , seqIndicesAny -- search any of the given sequence in the stream

    -- -- -- ** Searching Streams
    -- -- | Finding a stream within another stream.

    -- ** Splitting
    -- | Streams can be sliced into segments in space or in time. We use the
    -- term @chunk@ to refer to a spatial length of the stream (spatial window)
    -- and the term @session@ to refer to a length in time (time window).

    -- -- *** Using Element Separators
    , splitOn
    , splitOnSuffix
    , splitOnPrefix

    -- , splitBy
    , splitWithSuffix
    -- , splitByPrefix
    , wordsBy -- stripAndCompactBy

    -- -- *** Splitting By Sequences
    , splitBySeq
    , splitOnSeq
    , splitOnSuffixSeq
    -- , splitOnPrefixSeq

    -- Keeping the delimiters
    , splitWithSuffixSeq
    -- , splitByPrefixSeq
    -- , wordsBySeq

    -- Splitting using multiple sequence separators
    -- , splitOnAnySeq
    -- , splitOnAnySuffixSeq
    -- , splitOnAnyPrefixSeq

    -- -- *** Splitting By Streams
    -- -- | Splitting a stream using another stream as separator.

    -- ** Keyed Window Classification

    -- | Split the stream into chunks or windows by position or time. Each
    -- window can be associated with a key, all events associated with a
    -- particular key in the window can be folded to a single result.  The
    -- window termination can be dynamically controlled by the fold.
    --
    -- The term "chunk" is used for a window defined by position of elements
    -- and the term "session" is used for a time window.

    -- *** Tumbling Windows
    -- | A new window starts after the previous window is finished.

    -- , classifyChunksOf
    , classifySessionsBy
    , classifySessionsOf

    -- *** Keep Alive Windows
    -- | The window size is extended if an event arrives within the specified
    -- window size. This can represent sessions with idle or inactive timeout.

    -- , classifyKeepAliveChunks
    , classifyKeepAliveSessions

    {-
    -- *** Sliding Windows
    -- | A new window starts after the specified slide from the previous
    -- window. Therefore windows can overlap.
    , classifySlidingChunks
    , classifySlidingSessions
    -- *** Sliding Window Buffers
    -- , slidingChunkBuffer
    -- , slidingSessionBuffer
    -}

    -- * Transform (Nested Containers)
    -- Nested splitting
    , splitInnerBy
    , splitInnerBySuffix
    )
where

#include "inline.hs"

import Control.Concurrent (threadDelay)
import Control.Exception (assert)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Heap (Entry(..))
import Data.Kind (Type)
import Data.Maybe (isNothing)
import Foreign.Storable (Storable)
import Streamly.Internal.Data.Fold.Types (Fold (..), Fold2 (..))
import Streamly.Internal.Data.Parser (Parser (..))
import Streamly.Internal.Data.Unfold.Types (Unfold)
import Streamly.Internal.Data.Array.Foreign.Types (Array)
import Streamly.Internal.Data.SVar (MonadAsync)
import Streamly.Internal.Data.Stream.Ahead (ahead)
import Streamly.Internal.Data.Stream.Async (async, wAsync)
import Streamly.Internal.Data.Stream.IsStream.Common
    ( concatM
    , concatMapM
    , fold
    , interjectSuffix
    , intersperseM
    , repeatM
    , scanlMAfter'
    , smapM
    , splitOnSeq
    , value
    , valueM)
import Streamly.Internal.Data.Stream.Parallel (parallel)
import Streamly.Internal.Data.Stream.Prelude
       ( fromStreamS, toStreamS, concatFoldableWith, concatMapFoldableWith
       , concatForFoldableWith)
import Streamly.Internal.Data.Stream.StreamD (fromStreamD, toStreamD)
import Streamly.Internal.Data.Stream.StreamK (IsStream)
import Streamly.Internal.Data.Stream.Serial (serial, wSerial)
import Streamly.Internal.Data.Time.Units
       ( AbsTime, MilliSecond64(..), addToAbsTime, toRelTime
       , toAbsTime)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Data.Heap as H
import qualified Data.Map.Strict as Map
import qualified Streamly.Internal.Data.Array.Foreign.Types as A
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser.ParserK.Types as PRK
import qualified Streamly.Internal.Data.Parser.ParserD as PRD
import qualified Streamly.Internal.Data.Stream.Parallel as Par
import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.Zip as Z
#ifdef USE_STREAMK_ONLY
import qualified Streamly.Internal.Data.Stream.StreamK as S
#else
import qualified Streamly.Internal.Data.Stream.StreamD as S
#endif

import Prelude hiding (zipWith, concatMap, concat)

-- $setup
-- >>> :m
-- >>> import Prelude hiding (zipWith, concatMap, concat)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Parser as Parser
-- >>> import qualified Streamly.Data.Array.Foreign as Array

-------------------------------------------------------------------------------
-- Appending
-------------------------------------------------------------------------------

-- | Append the outputs of two streams, yielding all the elements from the
-- first stream and then yielding all the elements from the second stream.
--
-- IMPORTANT NOTE: This could be 100x faster than @serial/<>@ for appending a
-- few (say 100) streams because it can fuse via stream fusion. However, it
-- does not scale for a large number of streams (say 1000s) and becomes
-- qudartically slow. Therefore use this for custom appending of a few streams
-- but use 'concatMap' or 'concatMapWith serial' for appending @n@ streams or
-- infinite containers of streams.
--
-- /Pre-release/
{-# INLINE append #-}
append ::(IsStream t, Monad m) => t m b -> t m b -> t m b
append m1 m2 = fromStreamD $ D.append (toStreamD m1) (toStreamD m2)

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------

-- XXX Same as 'wSerial'. We should perhaps rename wSerial to interleave.
-- XXX Document the interleaving behavior of side effects in all the
-- interleaving combinators.
-- XXX Write time-domain equivalents of these. In the time domain we can
-- interleave two streams such that the value of second stream is always taken
-- from its last value even if no new value is being yielded, like
-- zipWithLatest. It would be something like interleaveWithLatest.
--
-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream. If any of the streams finishes
-- early the other stream continues alone until it too finishes.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleave "ab" ",,,," :: Stream.SerialT Identity Char
-- fromList "a,b,,,"
-- >>> Stream.interleave "abcd" ",," :: Stream.SerialT Identity Char
-- fromList "a,b,cd"
--
-- 'interleave' is dual to 'interleaveMin', it can be called @interleaveMax@.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleave #-}
interleave ::(IsStream t, Monad m) => t m b -> t m b -> t m b
interleave m1 m2 = fromStreamD $ D.interleave (toStreamD m1) (toStreamD m2)

-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream. As soon as the first stream
-- finishes, the output stops, discarding the remaining part of the second
-- stream. In this case, the last element in the resulting stream would be from
-- the second stream. If the second stream finishes early then the first stream
-- still continues to yield elements until it finishes.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleaveSuffix "abc" ",,,," :: Stream.SerialT Identity Char
-- fromList "a,b,c,"
-- >>> Stream.interleaveSuffix "abc" "," :: Stream.SerialT Identity Char
-- fromList "a,bc"
--
-- 'interleaveSuffix' is a dual of 'interleaveInfix'.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleaveSuffix #-}
interleaveSuffix ::(IsStream t, Monad m) => t m b -> t m b -> t m b
interleaveSuffix m1 m2 =
    fromStreamD $ D.interleaveSuffix (toStreamD m1) (toStreamD m2)

-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream and ending at the first stream.
-- If the second stream is longer than the first, elements from the second
-- stream are infixed with elements from the first stream. If the first stream
-- is longer then it continues yielding elements even after the second stream
-- has finished.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleaveInfix "abc" ",,,," :: Stream.SerialT Identity Char
-- fromList "a,b,c"
-- >>> Stream.interleaveInfix "abc" "," :: Stream.SerialT Identity Char
-- fromList "a,bc"
--
-- 'interleaveInfix' is a dual of 'interleaveSuffix'.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleaveInfix #-}
interleaveInfix ::(IsStream t, Monad m) => t m b -> t m b -> t m b
interleaveInfix m1 m2 =
    fromStreamD $ D.interleaveInfix (toStreamD m1) (toStreamD m2)

-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream. The output stops as soon as any
-- of the two streams finishes, discarding the remaining part of the other
-- stream. The last element of the resulting stream would be from the longer
-- stream.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleaveMin "ab" ",,,," :: Stream.SerialT Identity Char
-- fromList "a,b,"
-- >>> Stream.interleaveMin "abcd" ",," :: Stream.SerialT Identity Char
-- fromList "a,b,c"
--
-- 'interleaveMin' is dual to 'interleave'.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleaveMin #-}
interleaveMin ::(IsStream t, Monad m) => t m b -> t m b -> t m b
interleaveMin m1 m2 = fromStreamD $ D.interleaveMin (toStreamD m1) (toStreamD m2)

-------------------------------------------------------------------------------
-- Scheduling
-------------------------------------------------------------------------------

-- | Schedule the execution of two streams in a fair round-robin manner,
-- executing each stream once, alternately. Execution of a stream may not
-- necessarily result in an output, a stream may chose to @Skip@ producing an
-- element until later giving the other stream a chance to run. Therefore, this
-- combinator fairly interleaves the execution of two streams rather than
-- fairly interleaving the output of the two streams. This can be useful in
-- co-operative multitasking without using explicit threads. This can be used
-- as an alternative to `async`.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE roundrobin #-}
roundrobin ::(IsStream t, Monad m) => t m b -> t m b -> t m b
roundrobin m1 m2 = fromStreamD $ D.roundRobin (toStreamD m1) (toStreamD m2)

------------------------------------------------------------------------------
-- Merging (sorted streams)
------------------------------------------------------------------------------

-- | Merge two streams using a comparison function. The head elements of both
-- the streams are compared and the smaller of the two elements is emitted, if
-- both elements are equal then the element from the first stream is used
-- first.
--
-- If the streams are sorted in ascending order, the resulting stream would
-- also remain sorted in ascending order.
--
-- @
-- >>> Stream.toList $ Stream.mergeBy compare (Stream.fromList [1,3,5]) (Stream.fromList [2,4,6,8])
-- [1,2,3,4,5,6,8]
--
-- @
--
-- @since 0.6.0
{-# INLINABLE mergeBy #-}
mergeBy ::
       (IsStream t, Monad m) => (a -> a -> Ordering) -> t m a -> t m a -> t m a
mergeBy f m1 m2 = fromStreamS $ S.mergeBy f (toStreamS m1) (toStreamS m2)

-- | Like 'mergeBy' but with a monadic comparison function.
--
-- Merge two streams randomly:
--
-- @
-- > randomly _ _ = randomIO >>= \x -> return $ if x then LT else GT
-- > S.toList $ S.mergeByM randomly (S.fromList [1,1,1,1]) (S.fromList [2,2,2,2])
-- [2,1,2,2,2,1,1,1]
-- @
--
-- Merge two streams in a proportion of 2:1:
--
-- @
-- proportionately m n = do
--  ref <- newIORef $ cycle $ concat [replicate m LT, replicate n GT]
--  return $ \\_ _ -> do
--      r <- readIORef ref
--      writeIORef ref $ tail r
--      return $ head r
--
-- main = do
--  f <- proportionately 2 1
--  xs <- S.toList $ S.mergeByM f (S.fromList [1,1,1,1,1,1]) (S.fromList [2,2,2])
--  print xs
-- @
-- @
-- [1,1,2,1,1,2,1,1,2]
-- @
--
-- @since 0.6.0
{-# INLINABLE mergeByM #-}
mergeByM
    :: (IsStream t, Monad m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeByM f m1 m2 = fromStreamS $ S.mergeByM f (toStreamS m1) (toStreamS m2)

{-
-- | Like 'mergeByM' but stops merging as soon as any of the two streams stops.
{-# INLINABLE mergeEndByAny #-}
mergeEndByAny
    :: (IsStream t, Monad m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeEndByAny f m1 m2 = fromStreamD $
    D.mergeEndByAny f (toStreamD m1) (toStreamD m2)

-- Like 'mergeByM' but stops merging as soon as the first stream stops.
{-# INLINABLE mergeEndByFirst #-}
mergeEndByFirst
    :: (IsStream t, Monad m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeEndByFirst f m1 m2 = fromStreamS $
    D.mergeEndByFirst f (toStreamD m1) (toStreamD m2)

-- Holding this back for now, we may want to use the name "merge" differently
-- | Same as @'mergeBy' 'compare'@.
--
-- @
-- > S.toList $ S.merge (S.fromList [1,3,5]) (S.fromList [2,4,6,8])
-- [1,2,3,4,5,6,8]
-- @
--
-- @since 0.6.0
{-# INLINABLE merge #-}
merge ::
       (IsStream t, Monad m, Ord a) => t m a -> t m a -> t m a
merge = mergeBy compare
-}

-- | Like 'mergeBy' but merges concurrently (i.e. both the elements being
-- merged are generated concurrently).
--
-- @since 0.6.0
{-# INLINE mergeAsyncBy #-}
mergeAsyncBy :: (IsStream t, MonadAsync m)
    => (a -> a -> Ordering) -> t m a -> t m a -> t m a
mergeAsyncBy f = mergeAsyncByM (\a b -> return $ f a b)

-- | Like 'mergeByM' but merges concurrently (i.e. both the elements being
-- merged are generated concurrently).
--
-- @since 0.6.0
{-# INLINE mergeAsyncByM #-}
mergeAsyncByM :: (IsStream t, MonadAsync m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeAsyncByM f m1 m2 =
    fromStreamD $
        let par = Par.mkParallelD . toStreamD
        in D.mergeByM f (par m1) (par m2)

------------------------------------------------------------------------------
-- Combine N Streams - concatMap
------------------------------------------------------------------------------

-- | @concatMapWith merge map stream@ is a two dimensional looping combinator.
-- The first argument specifies a merge or concat function that is used to
-- merge the streams generated by applying the second argument i.e. the @map@
-- function to each element of the input stream. The concat function could be
-- 'serial', 'parallel', 'async', 'ahead' or any other zip or merge function
-- and the second argument could be any stream generation function using a
-- seed.
--
-- /Compare 'foldMapWith'/
--
-- /Since: 0.7.0/
--
-- /Since: 0.8.0 (signature change)/
{-# INLINE concatMapWith #-}
concatMapWith
    :: IsStream t
    => (t m b -> t m b -> t m b)
    -> (a -> t m b)
    -> t m a
    -> t m b
concatMapWith = K.concatMapBy

-- | Like 'concatMapWith' but carries a state which can be used to share
-- information across multiple steps of concat.
--
-- @
-- concatSmapMWith combine f initial = concatMapWith combine id . smapM f initial
-- @
--
-- /Pre-release/
--
{-# INLINE concatSmapMWith #-}
concatSmapMWith
    :: (IsStream t, Monad m)
    => (t m b -> t m b -> t m b)
    -> (s -> a -> m (s, t m b))
    -> m s
    -> t m a
    -> t m b
concatSmapMWith combine f initial = concatMapWith combine id . smapM f initial

-- | Map a stream producing function on each element of the stream and then
-- flatten the results into a single stream.
--
-- @
-- concatMap f = 'concat . map f'
-- concatMap = 'concatMapWith' 'Serial.serial'
-- concatMap f = 'concatMapM' (return . f)
-- concatMap f = 'concatUnfold' (UF.lmap f UF.fromStream)
-- @
--
-- @since 0.6.0
{-# INLINE concatMap #-}
concatMap ::(IsStream t, Monad m) => (a -> t m b) -> t m a -> t m b
concatMap f m = fromStreamD $ D.concatMap (toStreamD . f) (toStreamD m)

-- | Flatten a stream of streams to a single stream.
--
-- @
-- concat = concatMap id
-- @
--
-- /Pre-release/
{-# INLINE concat #-}
concat :: (IsStream t, Monad m) => t m (t m a) -> t m a
concat = concatMap id

-- Keep concating either streams as long as rights are generated, stop as soon
-- as a left is generated and concat the left stream.
--
-- See also: 'handle'
--
-- /Unimplemented/
--
{-
concatMapEitherWith
    :: -- (IsStream t, MonadAsync m) =>
       (forall x. t m x -> t m x -> t m x)
    -> (a -> t m (Either (t m b) b))
    -> t m a
    -> t m b
concatMapEitherWith = undefined
-}

-- XXX Implement a StreamD version for fusion.
--
-- | Combine streams in pairs using a binary stream combinator, then combine
-- the resulting streams in pairs recursively until we get to a single combined
-- stream.
--
-- For example, you can sort a stream using merge sort like this:
--
-- >>> Stream.toList $ Stream.concatPairsWith (Stream.mergeBy compare) Stream.value $ Stream.fromList [5,1,7,9,2]
-- [1,2,5,7,9]
--
-- /Caution: the stream of streams must be finite/
--
-- /Pre-release/
--
{-# INLINE concatPairsWith #-}
concatPairsWith :: IsStream t =>
       (t m b -> t m b -> t m b)
    -> (a -> t m b)
    -> t m a
    -> t m b
concatPairsWith = K.concatPairsWith

------------------------------------------------------------------------------
-- Combine N Streams - concatUnfold
------------------------------------------------------------------------------

-- | Like 'concatMap' but uses an 'Unfold' for stream generation. Unlike
-- 'concatMap' this can fuse the 'Unfold' code with the inner loop and
-- therefore provide many times better performance.
--
-- @since 0.7.0
{-# INLINE concatUnfold #-}
concatUnfold ::(IsStream t, Monad m) => Unfold m a b -> t m a -> t m b
concatUnfold u m = fromStreamD $ D.concatUnfold u (toStreamD m)

-- | Like 'concatUnfold' but interleaves the streams in the same way as
-- 'interleave' behaves instead of appending them.
--
-- /Pre-release/
{-# INLINE concatUnfoldInterleave #-}
concatUnfoldInterleave ::(IsStream t, Monad m)
    => Unfold m a b -> t m a -> t m b
concatUnfoldInterleave u m =
    fromStreamD $ D.concatUnfoldInterleave u (toStreamD m)

-- | Like 'concatUnfold' but executes the streams in the same way as
-- 'roundrobin'.
--
-- /Pre-release/
{-# INLINE concatUnfoldRoundrobin #-}
concatUnfoldRoundrobin ::(IsStream t, Monad m)
    => Unfold m a b -> t m a -> t m b
concatUnfoldRoundrobin u m =
    fromStreamD $ D.concatUnfoldRoundrobin u (toStreamD m)

------------------------------------------------------------------------------
-- Combine N Streams - interpose
------------------------------------------------------------------------------

-- > interpose x unf str = gintercalate unf str UF.identity (repeat x)
--
-- | Unfold the elements of a stream, intersperse the given element between the
-- unfolded streams and then concat them into a single stream.
--
-- > unwords = S.interpose ' '
--
-- /Pre-release/
{-# INLINE interpose #-}
interpose :: (IsStream t, Monad m)
    => c -> Unfold m b c -> t m b -> t m c
interpose x unf str =
    D.fromStreamD $ D.interpose (return x) unf (D.toStreamD str)

-- interposeSuffix x unf str = gintercalateSuffix unf str UF.identity (repeat x)
--
-- | Unfold the elements of a stream, append the given element after each
-- unfolded stream and then concat them into a single stream.
--
-- > unlines = S.interposeSuffix '\n'
--
-- /Pre-release/
{-# INLINE interposeSuffix #-}
interposeSuffix :: (IsStream t, Monad m)
    => c -> Unfold m b c -> t m b -> t m c
interposeSuffix x unf str =
    D.fromStreamD $ D.interposeSuffix (return x) unf (D.toStreamD str)

------------------------------------------------------------------------------
-- Combine N Streams - intercalate
------------------------------------------------------------------------------

-- XXX we can swap the order of arguments to gintercalate so that the
-- definition of concatUnfold becomes simpler? The first stream should be
-- infixed inside the second one. However, if we change the order in
-- "interleave" as well similarly, then that will make it a bit unintuitive.
--
-- > concatUnfold unf str =
-- >     gintercalate unf str (UF.nilM (\_ -> return ())) (repeat ())
--
-- | 'interleaveInfix' followed by unfold and concat.
--
-- /Pre-release/
{-# INLINE gintercalate #-}
gintercalate
    :: (IsStream t, Monad m)
    => Unfold m a c -> t m a -> Unfold m b c -> t m b -> t m c
gintercalate unf1 str1 unf2 str2 =
    D.fromStreamD $ D.gintercalate
        unf1 (D.toStreamD str1)
        unf2 (D.toStreamD str2)

-- XXX The order of arguments in "intercalate" is consistent with the list
-- intercalate but inconsistent with gintercalate and other stream interleaving
-- combinators. We can change the order of the arguments in other combinators
-- but then 'interleave' combinator may become a bit unintuitive because we
-- will be starting with the second stream.

-- > intercalate seed unf str = gintercalate unf str unf (repeatM seed)
-- > intercalate a unf str = concatUnfold unf $ intersperse a str
--
-- | 'intersperse' followed by unfold and concat.
--
-- > unwords = intercalate " " Unfold.fromList
--
-- >>> Stream.toList $ Stream.intercalate " " Unfold.fromList $ Stream.fromList ["abc", "def", "ghi"]
-- "abc def ghi"
--
-- /Pre-release/
{-# INLINE intercalate #-}
intercalate :: (IsStream t, Monad m)
    => b -> Unfold m b c -> t m b -> t m c
intercalate seed unf str = D.fromStreamD $
    D.concatUnfold unf $ D.intersperse seed (toStreamD str)

-- | 'interleaveSuffix' followed by unfold and concat.
--
-- /Pre-release/
{-# INLINE gintercalateSuffix #-}
gintercalateSuffix
    :: (IsStream t, Monad m)
    => Unfold m a c -> t m a -> Unfold m b c -> t m b -> t m c
gintercalateSuffix unf1 str1 unf2 str2 =
    D.fromStreamD $ D.gintercalateSuffix
        unf1 (D.toStreamD str1)
        unf2 (D.toStreamD str2)

-- > intercalateSuffix seed unf str = gintercalateSuffix unf str unf (repeatM seed)
-- > intercalateSuffix a unf str = concatUnfold unf $ intersperseSuffix a str
--
-- | 'intersperseSuffix' followed by unfold and concat.
--
-- > unlines = intercalateSuffix "\n" Unfold.fromList
--
-- >>> Stream.toList $ Stream.intercalateSuffix "\n" Unfold.fromList $ Stream.fromList ["abc", "def", "ghi"]
-- "abc\ndef\nghi\n"
--
-- /Pre-release/
{-# INLINE intercalateSuffix #-}
intercalateSuffix :: (IsStream t, Monad m)
    => b -> Unfold m b c -> t m b -> t m c
intercalateSuffix seed unf str = fromStreamD $ D.concatUnfold unf
    $ D.intersperseSuffix (return seed) (D.toStreamD str)

------------------------------------------------------------------------------
-- IterateMap - Map and flatten Trees of Streams
------------------------------------------------------------------------------

-- | Like 'iterateM' but iterates after mapping a stream generator on the
-- output.
--
-- Yield an input element in the output stream, map a stream generator on it
-- and then do the same on the resulting stream. This can be used for a depth
-- first traversal of a tree like structure.
--
-- Note that 'iterateM' is a special case of 'iterateMapWith':
--
-- @
-- iterateM f = iterateMapWith serial (valueM . f) . valueM
-- @
--
-- It can be used to traverse a tree structure.  For example, to list a
-- directory tree:
--
-- @
-- Stream.iterateMapWith Stream.serial
--     (either Dir.toEither (const nil))
--     (value (Left "tmp"))
-- @
--
-- /Pre-release/
--
{-# INLINE iterateMapWith #-}
iterateMapWith
    :: IsStream t
    => (t m a -> t m a -> t m a)
    -> (a -> t m a)
    -> t m a
    -> t m a
iterateMapWith combine f = concatMapWith combine go
    where
    go x = value x `combine` concatMapWith combine go (f x)

{-
{-# INLINE iterateUnfold #-}
iterateUnfold :: (IsStream t, MonadAsync m)
    => Unfold m a a -> t m a -> t m a
iterateUnfold unf xs = undefined
-}

------------------------------------------------------------------------------
-- Flattening Graphs
------------------------------------------------------------------------------

-- To traverse graphs we need a state to be carried around in the traversal.
-- For example, we can use a hashmap to store the visited status of nodes.

-- | Like 'iterateMap' but carries a state in the stream generation function.
-- This can be used to traverse graph like structures, we can remember the
-- visited nodes in the state to avoid cycles.
--
-- Note that a combination of 'iterateMap' and 'usingState' can also be used to
-- traverse graphs. However, this function provides a more localized state
-- instead of using a global state.
--
-- See also: 'mfix'
--
-- /Pre-release/
--
{-# INLINE iterateSmapMWith #-}
iterateSmapMWith
    :: (IsStream t, Monad m)
    => (t m a -> t m a -> t m a)
    -> (b -> a -> m (b, t m a))
    -> m b
    -> t m a
    -> t m a
iterateSmapMWith combine f initial stream =
    concatMap (\b -> concatMapWith combine (go b) stream) (valueM initial)

    where

    go b a = value a `combine` feedback b a

    feedback b a =
        concatMap
            (\(b1, s) -> concatMapWith combine (go b1) s)
            (valueM $ f b a)

------------------------------------------------------------------------------
-- iterateMap - Either streams
------------------------------------------------------------------------------

-- | In an 'Either' stream iterate on 'Left's.  This is a special case of
-- 'iterateMapWith':
--
-- @
-- iterateMapLeftsWith combine f = iterateMapWith combine (either f (const nil))
-- @
--
-- To traverse a directory tree:
--
-- @
-- iterateMapLeftsWith serial Dir.toEither (value (Left "tmp"))
-- @
--
-- /Pre-release/
--
{-# INLINE iterateMapLeftsWith #-}
iterateMapLeftsWith
    :: IsStream t
    => (t m (Either a b) -> t m (Either a b) -> t m (Either a b))
    -> (a -> t m (Either a b))
    -> t m (Either a b)
    -> t m (Either a b)
iterateMapLeftsWith combine f = iterateMapWith combine (either f (const K.nil))

------------------------------------------------------------------------------
-- Folding
------------------------------------------------------------------------------

-- Splitting operations that take a predicate and a Fold can be
-- expressed using parseMany. Operations like chunksOf, intervalsOf, split*,
-- can be expressed using parseMany when used with an appropriate Parser.
--
-- XXX We need takeGE/takeBetween to implement "some" using "many".

-- | Like 'foldMany' but appends empty fold output if the fold and stream
-- termination aligns:
--
-- >>> f = Fold.take 2 Fold.sum
-- >>> Stream.toList $ Stream.foldManyPost f $ Stream.fromList []
-- [0]
-- >>> Stream.toList $ Stream.foldManyPost f $ Stream.fromList [1..9]
-- [3,7,11,15,9]
-- >>> Stream.toList $ Stream.foldManyPost f $ Stream.fromList [1..10]
-- [3,7,11,15,19,0]
--
-- /Pre-release/
--
{-# INLINE foldManyPost #-}
foldManyPost
    :: (IsStream t, Monad m)
    => Fold m a b
    -> t m a
    -> t m b
foldManyPost f m = D.fromStreamD $ D.foldManyPost f (D.toStreamD m)

-- | Apply a 'Fold' repeatedly on a stream and emit the fold outputs in the
-- output stream.
--
-- To sum every two contiguous elements in a stream:
--
-- >>> f = Fold.take 2 Fold.sum
-- >>> Stream.toList $ Stream.foldMany f $ Stream.fromList [1..10]
-- [3,7,11,15,19]
--
-- On an empty stream the output is empty:
--
-- >>> Stream.toList $ Stream.foldMany f $ Stream.fromList []
-- []
--
-- Note @foldMany (take 0)@ would result in an infinite loop in a non-empty
-- stream.
--
-- /Pre-release/
--
{-# INLINE foldMany #-}
foldMany
    :: (IsStream t, Monad m)
    => Fold m a b
    -> t m a
    -> t m b
foldMany f m = D.fromStreamD $ D.foldMany f (D.toStreamD m)

-- | Apply a stream of folds to an input stream and emit the results in the
-- output stream.
--
-- /Pre-release/
--
{-# INLINE foldSequence #-}
foldSequence
       :: -- (IsStream t, Monad m) =>
       t m (Fold m a b)
    -> t m a
    -> t m b
foldSequence _f _m = undefined

-- | Iterate a fold generator on a stream. The initial value @b@ is used to
-- generate the first fold, the fold is applied on the stream and the result of
-- the fold is used to generate the next fold and so on.
--
-- @
-- > import Data.Monoid (Sum(..))
-- > f x = return (Fold.take 2 (Fold.sconcat x))
-- > s = Stream.map Sum $ Stream.fromList [1..10]
-- > Stream.toList $ Stream.map getSum $ Stream.foldIterateM f 0 s
-- [3,10,21,36,55,55]
--
-- @
--
-- This is the streaming equivalent of monad like sequenced application of
-- folds where next fold is dependent on the previous fold.
--
-- /Pre-release/
--
{-# INLINE foldIterateM #-}
foldIterateM ::
       (IsStream t, Monad m) => (b -> m (Fold m a b)) -> b -> t m a -> t m b
foldIterateM f i m = D.fromStreamD $ D.foldIterateM f i (D.toStreamD m)

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- | Apply a 'Parser' repeatedly on a stream and emit the parsed values in the
-- output stream.
--
-- This is the streaming equivalent of the 'Streamly.Internal.Data.Parser.many'
-- parse combinator.
--
-- >>> Stream.toList $ Stream.parseMany (Parser.takeBetween 0 2 Fold.sum) $ Stream.fromList [1..10]
-- [3,7,11,15,19]
--
-- @
-- > Stream.toList $ Stream.parseMany (Parser.line Fold.toList) $ Stream.fromList "hello\\nworld"
-- ["hello\\n","world"]
--
-- @
--
-- @
-- foldMany f = parseMany (fromFold f)
-- @
--
-- Known Issues: When the parser fails there is no way to get the remaining
-- stream.
--
-- /Pre-release/
--
{-# INLINE parseMany #-}
parseMany
    :: (IsStream t, MonadThrow m)
    => Parser m a b
    -> t m a
    -> t m b
parseMany p m =
    D.fromStreamD $ D.parseMany (PRK.fromParserK p) (D.toStreamD m)

{-# INLINE parseManyD #-}
parseManyD
    :: (IsStream t, MonadThrow m)
    => PRD.Parser m a b
    -> t m a
    -> t m b
parseManyD p m =
    D.fromStreamD $ D.parseMany p (D.toStreamD m)

-- | Apply a stream of parsers to an input stream and emit the results in the
-- output stream.
--
-- /Pre-release/
--
{-# INLINE parseSequence #-}
parseSequence
       :: -- (IsStream t, Monad m) =>
       t m (Parser m a b)
    -> t m a
    -> t m b
parseSequence _f _m = undefined

-- | @parseManyTill collect test stream@ tries the parser @test@ on the input,
-- if @test@ fails it backtracks and tries @collect@, after @collect@ succeeds
-- @test@ is tried again and so on. The parser stops when @test@ succeeds.  The
-- output of @test@ is discarded and the output of @collect@ is emitted in the
-- output stream. The parser fails if @collect@ fails.
--
-- /Unimplemented/
--
{-# INLINE parseManyTill #-}
parseManyTill ::
    -- (IsStream t, MonadThrow m) =>
       Parser m a b
    -> Parser m a x
    -> t m a
    -> t m b
parseManyTill = undefined

-- | Iterate a parser generating function on a stream. The initial value @b@ is
-- used to generate the first parser, the parser is applied on the stream and
-- the result is used to generate the next parser and so on.
--
-- >>> import Data.Monoid (Sum(..))
-- >>> Stream.toList $ Stream.map getSum $ Stream.parseIterate (\b -> Parser.takeBetween 0 2 (Fold.sconcat b)) 0 $ Stream.map Sum $ Stream.fromList [1..10]
-- [3,10,21,36,55,55]
--
-- This is the streaming equivalent of monad like sequenced application of
-- parsers where next parser is dependent on the previous parser.
--
-- /Pre-release/
--
{-# INLINE parseIterate #-}
parseIterate
    :: (IsStream t, MonadThrow m)
    => (b -> Parser m a b)
    -> b
    -> t m a
    -> t m b
parseIterate f i m = D.fromStreamD $
    D.parseIterate (PRK.fromParserK . f) i (D.toStreamD m)

------------------------------------------------------------------------------
-- Trimming
------------------------------------------------------------------------------

-- | Drop prefix from the input stream if present.
--
-- Space: @O(1)@
--
-- /Unimplemented/ - Help wanted.
{-# INLINE dropPrefix #-}
dropPrefix ::
    -- (Eq a, IsStream t, Monad m) =>
    t m a -> t m a -> t m a
dropPrefix = error "Not implemented yet!"

-- | Drop all matching infix from the input stream if present. Infix stream
-- may be consumed multiple times.
--
-- Space: @O(n)@ where n is the length of the infix.
--
-- /Unimplemented/ - Help wanted.
{-# INLINE dropInfix #-}
dropInfix ::
    -- (Eq a, IsStream t, Monad m) =>
    t m a -> t m a -> t m a
dropInfix = error "Not implemented yet!"

-- | Drop suffix from the input stream if present. Suffix stream may be
-- consumed multiple times.
--
-- Space: @O(n)@ where n is the length of the suffix.
--
-- /Unimplemented/ - Help wanted.
{-# INLINE dropSuffix #-}
dropSuffix ::
    -- (Eq a, IsStream t, Monad m) =>
    t m a -> t m a -> t m a
dropSuffix = error "Not implemented yet!"

------------------------------------------------------------------------------
-- Generalized grouping
------------------------------------------------------------------------------

-- This combinator is the most general grouping combinator and can be used to
-- implement all other grouping combinators.
--
-- XXX check if this can implement the splitOn combinator i.e. we can slide in
-- new elements, slide out old elements and incrementally compute the hash.
-- Also, can we implement the windowed classification combinators using this?
--
-- In fact this is a parse. Instead of using a special return value in the fold
-- we are using a mapping function.
--
-- Note that 'scanl'' (usually followed by a map to extract the desired value
-- from the accumulator) can be used to realize many implementations e.g. a
-- sliding window implementation. A scan followed by a mapMaybe is also a good
-- pattern to express many problems where we want to emit a filtered output and
-- not emit an output on every input.
--
-- Passing on of the initial accumulator value to the next fold is equivalent
-- to returning the leftover concept.

{-
-- | @groupScan splitter fold stream@ folds the input stream using @fold@.
-- @splitter@ is applied on the accumulator of the fold every time an item is
-- consumed by the fold. The fold continues until @splitter@ returns a 'Just'
-- value.  A 'Just' result from the @splitter@ specifies a result to be emitted
-- in the output stream and the initial value of the accumulator for the next
-- group's fold. This allows us to control whether to start fresh for the next
-- fold or to continue from the previous fold's output.
--
{-# INLINE groupScan #-}
groupScan
    :: (IsStream t, Monad m)
    => (x -> m (Maybe (b, x))) -> Fold m a x -> t m a -> t m b
groupScan split fold m = undefined
-}

------------------------------------------------------------------------------
-- Grouping
------------------------------------------------------------------------------

-- | @groupsBy cmp f $ S.fromList [a,b,c,...]@ assigns the element @a@ to the
-- first group, if @b \`cmp` a@ is 'True' then @b@ is also assigned to the same
-- group.  If @c \`cmp` a@ is 'True' then @c@ is also assigned to the same
-- group and so on. When the comparison fails a new group is started. Each
-- group is folded using the fold @f@ and the result of the fold is emitted in
-- the output stream.
--
-- >>> Stream.toList $ Stream.groupsBy (>) Fold.toList $ Stream.fromList [1,3,7,0,2,5]
-- [[1,3,7],[0,2,5]]
--
-- @since 0.7.0
{-# INLINE groupsBy #-}
groupsBy
    :: (IsStream t, Monad m)
    => (a -> a -> Bool)
    -> Fold m a b
    -> t m a
    -> t m b
groupsBy cmp f m = D.fromStreamD $ D.groupsBy cmp f (D.toStreamD m)

-- | Unlike @groupsBy@ this function performs a rolling comparison of two
-- successive elements in the input stream. @groupsByRolling cmp f $ S.fromList
-- [a,b,c,...]@ assigns the element @a@ to the first group, if @a \`cmp` b@ is
-- 'True' then @b@ is also assigned to the same group.  If @b \`cmp` c@ is
-- 'True' then @c@ is also assigned to the same group and so on. When the
-- comparison fails a new group is started. Each group is folded using the fold
-- @f@.
--
-- >>> Stream.toList $ Stream.groupsByRolling (\a b -> a + 1 == b) Fold.toList $ Stream.fromList [1,2,3,7,8,9]
-- [[1,2,3],[7,8,9]]
--
-- @since 0.7.0
{-# INLINE groupsByRolling #-}
groupsByRolling
    :: (IsStream t, Monad m)
    => (a -> a -> Bool)
    -> Fold m a b
    -> t m a
    -> t m b
groupsByRolling cmp f m =  D.fromStreamD $ D.groupsRollingBy cmp f (D.toStreamD m)

-- |
-- > groups = groupsBy (==)
-- > groups = groupsByRolling (==)
--
-- Groups contiguous spans of equal elements together in individual groups.
--
-- >>> Stream.toList $ Stream.groups Fold.toList $ Stream.fromList [1,1,2,2]
-- [[1,1],[2,2]]
--
-- @since 0.7.0
{-# INLINE groups #-}
groups :: (IsStream t, Monad m, Eq a) => Fold m a b -> t m a -> t m b
groups = groupsBy (==)

------------------------------------------------------------------------------
-- Splitting - by a predicate
------------------------------------------------------------------------------

-- In general we can use deintercalate for splitting.  Then we can also use
-- uniqBy to condense the separators.  One way to generalize splitting is to
-- output:
--
-- data Segment a b = Empty | Segment b | Separator a
--
-- XXX splitOn and splitOnSuffix have a different behavior on an empty stream,
-- is that desirable?

-- | Split on an infixed separator element, dropping the separator.  The
-- supplied 'Fold' is applied on the split segments.  Splits the stream on
-- separator elements determined by the supplied predicate, separator is
-- considered as infixed between two segments:
--
-- >>> splitOn' p xs = Stream.toList $ Stream.splitOn p Fold.toList (Stream.fromList xs)
-- >>> splitOn' (== '.') "a.b"
-- ["a","b"]
--
-- An empty stream is folded to the default value of the fold:
--
-- >>> splitOn' (== '.') ""
-- [""]
--
-- If one or both sides of the separator are missing then the empty segment on
-- that side is folded to the default output of the fold:
--
-- >>> splitOn' (== '.') "."
-- ["",""]
--
-- >>> splitOn' (== '.') ".a"
-- ["","a"]
--
-- >>> splitOn' (== '.') "a."
-- ["a",""]
--
-- >>> splitOn' (== '.') "a..b"
-- ["a","","b"]
--
-- splitOn is an inverse of intercalating single element:
--
-- @Stream.intercalate (Stream.value '.') Unfold.fromList . Stream.splitOn (== '.') Fold.toList === id@
--
-- Assuming the input stream does not contain the separator:
--
-- @Stream.splitOn (== '.') Fold.toList . Stream.intercalate (Stream.value '.') Unfold.fromList === id@
--
-- @since 0.7.0

{-# INLINE splitOn #-}
splitOn
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitOn predicate f =
    -- We can express the infix splitting in terms of optional suffix split
    -- fold.  After applying a suffix split fold repeatedly if the last segment
    -- ends with a suffix then we need to return the default output of the fold
    -- after that to make it an infix split.
    --
    -- Alternately, we can also express it using an optional prefix split fold.
    -- If the first segment starts with a prefix then we need to emit the
    -- default output of the fold before that to make it an infix split, and
    -- then apply prefix split fold repeatedly.
    --
    -- Since a suffix split fold can be easily expressed using a
    -- non-backtracking fold, we use that.
    foldManyPost (FL.takeEndBy_ predicate f)

-- | Split on a suffixed separator element, dropping the separator.  The
-- supplied 'Fold' is applied on the split segments.
--
-- >>> splitOnSuffix' p xs = Stream.toList $ Stream.splitOnSuffix p Fold.toList (Stream.fromList xs)
-- >>> splitOnSuffix' (== '.') "a.b."
-- ["a","b"]
--
-- >>> splitOnSuffix' (== '.') "a."
-- ["a"]
--
-- An empty stream results in an empty output stream:
--
-- >>> splitOnSuffix' (== '.') ""
-- []
--
-- An empty segment consisting of only a suffix is folded to the default output
-- of the fold:
--
-- >>> splitOnSuffix' (== '.') "."
-- [""]
--
-- >>> splitOnSuffix' (== '.') "a..b.."
-- ["a","","b",""]
--
-- A suffix is optional at the end of the stream:
--
-- >>> splitOnSuffix' (== '.') "a"
-- ["a"]
--
-- >>> splitOnSuffix' (== '.') ".a"
-- ["","a"]
--
-- >>> splitOnSuffix' (== '.') "a.b"
-- ["a","b"]
--
-- > lines = splitOnSuffix (== '\n')
--
-- 'splitOnSuffix' is an inverse of 'intercalateSuffix' with a single element:
--
-- @Stream.intercalateSuffix (Stream.value '.') Unfold.fromList . Stream.splitOnSuffix (== '.') Fold.toList === id@
--
-- Assuming the input stream does not contain the separator:
--
-- @Stream.splitOnSuffix (== '.') Fold.toList . Stream.intercalateSuffix (Stream.value '.') Unfold.fromList === id@
--
-- @since 0.7.0

{-# INLINE splitOnSuffix #-}
splitOnSuffix
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitOnSuffix predicate f = foldMany (FL.takeEndBy_ predicate f)

-- | Split on a prefixed separator element, dropping the separator.  The
-- supplied 'Fold' is applied on the split segments.
--
-- @
-- > splitOnPrefix' p xs = Stream.toList $ Stream.splitOnPrefix p (Fold.toList) (Stream.fromList xs)
-- > splitOnPrefix' (== '.') ".a.b"
-- ["a","b"]
-- @
--
-- An empty stream results in an empty output stream:
-- @
-- > splitOnPrefix' (== '.') ""
-- []
-- @
--
-- An empty segment consisting of only a prefix is folded to the default output
-- of the fold:
--
-- @
-- > splitOnPrefix' (== '.') "."
-- [""]
--
-- > splitOnPrefix' (== '.') ".a.b."
-- ["a","b",""]
--
-- > splitOnPrefix' (== '.') ".a..b"
-- ["a","","b"]
--
-- @
--
-- A prefix is optional at the beginning of the stream:
--
-- @
-- > splitOnPrefix' (== '.') "a"
-- ["a"]
--
-- > splitOnPrefix' (== '.') "a.b"
-- ["a","b"]
-- @
--
-- 'splitOnPrefix' is an inverse of 'intercalatePrefix' with a single element:
--
-- @Stream.intercalatePrefix (Stream.value '.') Unfold.fromList . Stream.splitOnPrefix (== '.') Fold.toList === id@
--
-- Assuming the input stream does not contain the separator:
--
-- @Stream.splitOnPrefix (== '.') Fold.toList . Stream.intercalatePrefix (Stream.value '.') Unfold.fromList === id@
--
-- /Unimplemented/

{-# INLINE splitOnPrefix #-}
splitOnPrefix :: -- (IsStream t, MonadCatch m) =>
    (a -> Bool) -> Fold m a b -> t m a -> t m b
splitOnPrefix _predicate _f = undefined
    -- parseMany (Parser.sliceBeginBy predicate f)

-- | Like 'splitOn' after stripping leading, trailing, and repeated separators.
-- Therefore, @".a..b."@ with '.' as the separator would be parsed as
-- @["a","b"]@.  In other words, its like parsing words from whitespace
-- separated text.
--
-- >>> wordsBy' p xs = Stream.toList $ Stream.wordsBy p Fold.toList (Stream.fromList xs)
--
-- >>> wordsBy' (== ',') ""
-- []
--
-- >>> wordsBy' (== ',') ","
-- []
--
-- >>> wordsBy' (== ',') ",a,,b,"
-- ["a","b"]
--
-- > words = wordsBy isSpace
--
-- @since 0.7.0

-- It is equivalent to splitting in any of the infix/prefix/suffix styles
-- followed by removal of empty segments.
{-# INLINE wordsBy #-}
wordsBy
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
wordsBy predicate f m =
    D.fromStreamD $ D.wordsBy predicate f (D.toStreamD m)

-- | Like 'splitOnSuffix' but keeps the suffix attached to the resulting
-- splits.
--
-- >>> splitWithSuffix' p xs = Stream.toList $ splitWithSuffix p Fold.toList (Stream.fromList xs)
--
-- >>> splitWithSuffix' (== '.') ""
-- []
--
-- >>> splitWithSuffix' (== '.') "."
-- ["."]
--
-- >>> splitWithSuffix' (== '.') "a"
-- ["a"]
--
-- >>> splitWithSuffix' (== '.') ".a"
-- [".","a"]
--
-- >>> splitWithSuffix' (== '.') "a."
-- ["a."]
--
-- >>> splitWithSuffix' (== '.') "a.b"
-- ["a.","b"]
--
-- >>> splitWithSuffix' (== '.') "a.b."
-- ["a.","b."]
--
-- >>> splitWithSuffix' (== '.') "a..b.."
-- ["a.",".","b.","."]
--
-- @since 0.7.0

{-# INLINE splitWithSuffix #-}
splitWithSuffix
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitWithSuffix predicate f = foldMany (FL.takeEndBy predicate f)

------------------------------------------------------------------------------
-- Splitting - on a delimiter sequence
------------------------------------------------------------------------------

-- Int list examples for splitOn:
--
-- >>> splitList [] [1,2,3,3,4]
-- > [[1],[2],[3],[3],[4]]
--
-- >>> splitList [5] [1,2,3,3,4]
-- > [[1,2,3,3,4]]
--
-- >>> splitList [1] [1,2,3,3,4]
-- > [[],[2,3,3,4]]
--
-- >>> splitList [4] [1,2,3,3,4]
-- > [[1,2,3,3],[]]
--
-- >>> splitList [2] [1,2,3,3,4]
-- > [[1],[3,3,4]]
--
-- >>> splitList [3] [1,2,3,3,4]
-- > [[1,2],[],[4]]
--
-- >>> splitList [3,3] [1,2,3,3,4]
-- > [[1,2],[4]]
--
-- >>> splitList [1,2,3,3,4] [1,2,3,3,4]
-- > [[],[]]

{-
-- This can be implemented easily using Rabin Karp
-- | Split on any one of the given patterns.
{-# INLINE splitOnAny #-}
splitOnAny
    :: (IsStream t, Monad m, Storable a, Integral a)
    => [Array a] -> Fold m a b -> t m a -> t m b
splitOnAny subseq f m = undefined -- D.fromStreamD $ D.splitOnAny f subseq (D.toStreamD m)
-}

-- XXX use a non-monadic intersperse to remove the MonadAsync constraint.
-- XXX Use two folds, one ring buffer fold for separator sequence and the other
-- split consumer fold. The input is fed to the ring fold first and the
-- rejected input is fed to the split fold. If the separator matches, the ring
-- fold would consume all.
--
-- | Like 'splitOnSeq' but splits the separator as well, as an infix token.
--
-- >>> splitOn'_ pat xs = Stream.toList $ Stream.splitBySeq (Array.fromList pat) Fold.toList (Stream.fromList xs)
--
-- >>> splitOn'_ "" "hello"
-- ["h","","e","","l","","l","","o"]
--
-- >>> splitOn'_ "hello" ""
-- [""]
--
-- >>> splitOn'_ "hello" "hello"
-- ["","hello",""]
--
-- >>> splitOn'_ "x" "hello"
-- ["hello"]
--
-- >>> splitOn'_ "h" "hello"
-- ["","h","ello"]
--
-- >>> splitOn'_ "o" "hello"
-- ["hell","o",""]
--
-- >>> splitOn'_ "e" "hello"
-- ["h","e","llo"]
--
-- >>> splitOn'_ "l" "hello"
-- ["he","l","","l","o"]
--
-- >>> splitOn'_ "ll" "hello"
-- ["he","ll","o"]
--
-- /Pre-release/
{-# INLINE splitBySeq #-}
splitBySeq
    :: (IsStream t, MonadAsync m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitBySeq patt f m =
    intersperseM (fold f (A.toStream patt)) $ splitOnSeq patt f m

-- | Like 'splitSuffixBy' but the separator is a sequence of elements, instead
-- of a predicate for a single element.
--
-- >>> splitOnSuffixSeq_ pat xs = Stream.toList $ Stream.splitOnSuffixSeq (Array.fromList pat) Fold.toList (Stream.fromList xs)
--
-- >>> splitOnSuffixSeq_ "." ""
-- []
--
-- >>> splitOnSuffixSeq_ "." "."
-- [""]
--
-- >>> splitOnSuffixSeq_ "." "a"
-- ["a"]
--
-- >>> splitOnSuffixSeq_ "." ".a"
-- ["","a"]
--
-- >>> splitOnSuffixSeq_ "." "a."
-- ["a"]
--
-- >>> splitOnSuffixSeq_ "." "a.b"
-- ["a","b"]
--
-- >>> splitOnSuffixSeq_ "." "a.b."
-- ["a","b"]
--
-- >>> splitOnSuffixSeq_ "." "a..b.."
-- ["a","","b",""]
--
-- > lines = splitOnSuffixSeq "\n"
--
-- 'splitOnSuffixSeq' is an inverse of 'intercalateSuffix'. The following law
-- always holds:
--
-- > intercalateSuffix . splitOnSuffixSeq == id
--
-- The following law holds when the separator is non-empty and contains none of
-- the elements present in the input lists:
--
-- > splitSuffixOn . intercalateSuffix == id
--
-- /Pre-release/
{-# INLINE splitOnSuffixSeq #-}
splitOnSuffixSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitOnSuffixSeq patt f m =
    D.fromStreamD $ D.splitOnSuffixSeq False patt f (D.toStreamD m)

{-
-- | Like 'splitOn' but drops any empty splits.
--
{-# INLINE wordsOn #-}
wordsOn
    :: (IsStream t, Monad m, Storable a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
wordsOn subseq f m = undefined -- D.fromStreamD $ D.wordsOn f subseq (D.toStreamD m)
-}

-- | Like 'splitOnSuffixSeq' but keeps the suffix intact in the splits.
--
-- >>> splitWithSuffixSeq' pat xs = Stream.toList $ Stream.splitWithSuffixSeq (Array.fromList pat) Fold.toList (Stream.fromList xs)
--
-- >>> splitWithSuffixSeq' "." ""
-- []
--
-- >>> splitWithSuffixSeq' "." "."
-- ["."]
--
-- >>> splitWithSuffixSeq' "." "a"
-- ["a"]
--
-- >>> splitWithSuffixSeq' "." ".a"
-- [".","a"]
--
-- >>> splitWithSuffixSeq' "." "a."
-- ["a."]
--
-- >>> splitWithSuffixSeq' "." "a.b"
-- ["a.","b"]
--
-- >>> splitWithSuffixSeq' "." "a.b."
-- ["a.","b."]
--
-- >>> splitWithSuffixSeq' "." "a..b.."
-- ["a.",".","b.","."]
--
-- /Pre-release/
{-# INLINE splitWithSuffixSeq #-}
splitWithSuffixSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitWithSuffixSeq patt f m =
    D.fromStreamD $ D.splitOnSuffixSeq True patt f (D.toStreamD m)

{-
-- This can be implemented easily using Rabin Karp
-- | Split post any one of the given patterns.
{-# INLINE splitOnSuffixSeqAny #-}
splitOnSuffixSeqAny
    :: (IsStream t, Monad m, Storable a, Integral a)
    => [Array a] -> Fold m a b -> t m a -> t m b
splitOnSuffixSeqAny subseq f m = undefined
    -- D.fromStreamD $ D.splitPostAny f subseq (D.toStreamD m)
-}

------------------------------------------------------------------------------
-- Chunking
------------------------------------------------------------------------------

-- | Group the input stream into groups of @n@ elements each and then fold each
-- group using the provided fold function.
--
-- >>> Stream.toList $ Stream.chunksOf 2 Fold.sum (Stream.enumerateFromTo 1 10)
-- [3,7,11,15,19]
--
-- This can be considered as an n-fold version of 'take' where we apply
-- 'take' repeatedly on the leftover stream until the stream exhausts.
--
-- @chunksOf n f = foldMany (FL.take n f)@
--
-- @since 0.7.0
{-# INLINE chunksOf #-}
chunksOf
    :: (IsStream t, Monad m)
    => Int -> Fold m a b -> t m a -> t m b
chunksOf n f = D.fromStreamD . D.chunksOf n f . D.toStreamD

-- |
--
-- /Pre-release/
{-# INLINE chunksOf2 #-}
chunksOf2
    :: (IsStream t, Monad m)
    => Int -> m c -> Fold2 m c a b -> t m a -> t m b
chunksOf2 n action f m = D.fromStreamD $ D.groupsOf2 n action f (D.toStreamD m)

-- | @arraysOf n stream@ groups the elements in the input stream into arrays of
-- @n@ elements each.
--
-- Same as the following but may be more efficient:
--
-- > arraysOf n = Stream.foldMany (A.writeN n)
--
-- /Pre-release/
{-# INLINE arraysOf #-}
arraysOf :: (IsStream t, MonadIO m, Storable a)
    => Int -> t m a -> t m (Array a)
arraysOf n = D.fromStreamD . A.arraysOf n . D.toStreamD

-- XXX we can implement this by repeatedly applying the 'lrunFor' fold.
-- XXX add this example after fixing the serial stream rate control
--
-- | Group the input stream into windows of @n@ second each and then fold each
-- group using the provided fold function.
--
-- >>> Stream.toList $ Stream.take 5 $ Stream.intervalsOf 1 Fold.sum $ Stream.constRate 2 $ Stream.enumerateFrom 1
-- [...,...,...,...,...]
--
-- @since 0.7.0
{-# INLINE intervalsOf #-}
intervalsOf
    :: (IsStream t, MonadAsync m)
    => Double -> Fold m a b -> t m a -> t m b
intervalsOf n f xs =
    splitWithSuffix isNothing (FL.catMaybes f)
        (interjectSuffix n (return Nothing) (Serial.map Just xs))

------------------------------------------------------------------------------
-- Windowed classification
------------------------------------------------------------------------------

-- We divide the stream into windows or chunks in space or time and each window
-- can be associated with a key, all events associated with a particular key in
-- the window can be folded to a single result. The stream can be split into
-- windows by size or by using a split predicate on the elements in the stream.
-- For example, when we receive a closing flag, we can close the window.
--
-- A "chunk" is a space window and a "session" is a time window. Are there any
-- other better short words to describe them. An alternative is to use
-- "swindow" and "twindow". Another word for "session" could be "spell".
--
-- TODO: To mark the position in space or time we can have Indexed or
-- TimeStamped types. That can make it easy to deal with the position indices
-- or timestamps.

------------------------------------------------------------------------------
-- Keyed Sliding Windows
------------------------------------------------------------------------------

{-
{-# INLINABLE classifySlidingChunks #-}
classifySlidingChunks
    :: (IsStream t, MonadAsync m, Ord k)
    => Int              -- ^ window size
    -> Int              -- ^ window slide
    -> Fold m a b       -- ^ Fold to be applied to window events
    -> t m (k, a, Bool) -- ^ window key, data, close event
    -> t m (k, b)
classifySlidingChunks wsize wslide (Fold step initial extract) str
    = undefined

-- XXX Another variant could be to slide the window on an event, e.g. in TCP we
-- slide the send window when an ack is received and we slide the receive
-- window when a sequence is complete. Sliding is stateful in case of TCP,
-- sliding releases the send buffer or makes data available to the user from
-- the receive buffer.
{-# INLINABLE classifySlidingSessions #-}
classifySlidingSessions
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ timer tick in seconds
    -> Double         -- ^ time window size
    -> Double         -- ^ window slide
    -> Fold m a b     -- ^ Fold to be applied to window events
    -> t m (k, a, Bool, AbsTime) -- ^ window key, data, close flag, timestamp
    -> t m (k, b)
classifySlidingSessions tick interval slide (Fold step initial extract) str
    = undefined
-}

------------------------------------------------------------------------------
-- Sliding Window Buffers
------------------------------------------------------------------------------

-- These buffered versions could be faster than concurrent incremental folds of
-- all overlapping windows as in many cases we may not need all the values to
-- compute the fold, we can just compute the result using the old value and new
-- value.  However, we may need the buffer once in a while, for example for
-- string search we usually compute the hash incrementally but when the hash
-- matches the hash of the pattern we need to compare the whole string.
--
-- XXX we should be able to implement sequence based splitting combinators
-- using this combinator.

{-
-- | Buffer n elements of the input in a ring buffer. When t new elements are
-- collected, slide the window to remove the same number of oldest elements,
-- insert the new elements, and apply an incremental fold on the sliding
-- window, supplying the outgoing elements, the new ring buffer as arguments.
slidingChunkBuffer
    :: (IsStream t, Monad m, Ord a, Storable a)
    => Int -- window size
    -> Int -- window slide
    -> Fold m (Ring a, Array a) b
    -> t m a
    -> t m b
slidingChunkBuffer = undefined

-- Buffer n seconds worth of stream elements of the input in a radix tree.
-- Every t seconds, remove the items that are older than n seconds, and apply
-- an incremental fold on the sliding window, supplying the outgoing elements,
-- and the new radix tree buffer as arguments.
slidingSessionBuffer
    :: (IsStream t, Monad m, Ord a, Storable a)
    => Int    -- window size
    -> Int    -- tick size
    -> Fold m (RTree a, Array a) b
    -> t m a
    -> t m b
slidingSessionBuffer = undefined
-}

------------------------------------------------------------------------------
-- Keyed Session Windows
------------------------------------------------------------------------------

{-
-- | Keyed variable size space windows. Close the window if we do not receive a
-- window event in the next "spaceout" elements.
{-# INLINABLE classifyChunksBy #-}
classifyChunksBy
    :: (IsStream t, MonadAsync m, Ord k)
    => Int   -- ^ window spaceout (spread)
    -> Bool  -- ^ reset the spaceout when a chunk window element is received
    -> Fold m a b       -- ^ Fold to be applied to chunk window elements
    -> t m (k, a, Bool) -- ^ chunk key, data, last element
    -> t m (k, b)
classifyChunksBy spanout reset (Fold step initial extract) str = undefined

-- | Like 'classifyChunksOf' but the chunk size is reset if an element is
-- received within the chunk size window. The chunk gets closed only if no
-- element is received within the chunk window.
--
{-# INLINABLE classifyKeepAliveChunks #-}
classifyKeepAliveChunks
    :: (IsStream t, MonadAsync m, Ord k)
    => Int   -- ^ window spaceout (spread)
    -> Fold m a b       -- ^ Fold to be applied to chunk window elements
    -> t m (k, a, Bool) -- ^ chunk key, data, last element
    -> t m (k, b)
classifyKeepAliveChunks spanout = classifyChunksBy spanout True
-}

data SessionState t m k a b = SessionState
    { sessionCurTime :: !AbsTime  -- ^ time since last event
    , sessionEventTime :: !AbsTime -- ^ time as per last event
    , sessionCount :: !Int -- ^ total number sessions in progress
    , sessionTimerHeap :: H.Heap (H.Entry AbsTime k) -- ^ heap for timeouts
    , sessionKeyValueMap :: Map.Map k a -- ^ Stored sessions for keys
    , sessionOutputStream :: t (m :: Type -> Type) (k, b) -- ^ Completed sessions
    }

-- | @classifySessionsBy tick keepalive predicate timeout fold stream@
-- classifies an input event @stream@ consisting of  @(timestamp, (key,
-- value))@ into sessions based on the @key@, folding all the values
-- corresponding to the same key into a session using the supplied @fold@.
--
-- When the fold terminates or a @timeout@ occurs, a tuple consisting of the
-- session key and the folded value is emitted in the output stream. The
-- timeout is measured from the first event in the session.  If the @keepalive@
-- option is set to 'True' the timeout is reset to 0 whenever an event is
-- received.
--
-- The @timestamp@ in the input stream is an absolute time from some epoch,
-- characterizing the time when the input event was generated.  The notion of
-- current time is maintained by a monotonic event time clock using the
-- timestamps seen in the input stream. The latest timestamp seen till now is
-- used as the base for the current time.  When no new events are seen, a timer
-- is started with a clock resolution of @tick@ seconds. This timer is used to
-- detect session timeouts in the absence of new events.
--
-- To ensure an upper bound on the memory used the number of sessions can be
-- limited to an upper bound. If the ejection @predicate@ returns 'True', the
-- oldest session is ejected before inserting a new session.
--
-- >>> :{
-- Stream.mapM_ print
--     $ Stream.classifySessionsBy 1 False (const (return False)) 3 (Fold.take 3 Fold.toList)
--     $ Stream.timestamped
--     $ Stream.delay 0.1
--     $ (,) <$> Stream.fromList [1,2,3] <*> Stream.fromList ['a','b','c']
-- :}
-- (1,"abc")
-- (2,"abc")
-- (3,"abc")
--
-- /Pre-release/
--
{-# INLINABLE classifySessionsBy #-}
classifySessionsBy
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ timer tick in seconds
    -> Bool           -- ^ reset the timer when an event is received
    -> (Int -> m Bool) -- ^ predicate to eject sessions based on session count
    -> Double         -- ^ session timeout in seconds
    -> Fold m a b  -- ^ Fold to be applied to session data
    -> t m (AbsTime, (k, a)) -- ^ timestamp, (session key, session data)
    -> t m (k, b) -- ^ session key, fold result
classifySessionsBy tick reset ejectPred tmout
    (Fold step initial extract) str =
    concatMap sessionOutputStream $
        scanlMAfter' sstep (return szero) flush stream

    where

    timeoutMs = toRelTime (round (tmout * 1000) :: MilliSecond64)
    tickMs = toRelTime (round (tick * 1000) :: MilliSecond64)
    szero = SessionState
        { sessionCurTime = toAbsTime (0 :: MilliSecond64)
        , sessionEventTime = toAbsTime (0 :: MilliSecond64)
        , sessionCount = 0
        , sessionTimerHeap = H.empty
        , sessionKeyValueMap = Map.empty
        , sessionOutputStream = K.nil
        }

    -- We can eject sessions based on the current session count to limit
    -- memory consumption. There are two possible strategies:
    --
    -- 1) Eject old sessions or sessions beyond a certain/lower timeout
    -- threshold even before timeout, effectively reduce the timeout.
    -- 2) Drop creation of new sessions but keep accepting new events for the
    -- old ones.
    --
    -- We use the first strategy as of now.

    -- Got a new stream input element
    sstep session@SessionState{..} (Just (timestamp, (key, val))) = do
        -- XXX we should use a heap in pinned memory to scale it to a large
        -- size
        --
        -- To detect session inactivity we keep a timestamp of the latest event
        -- in the Map along with the fold result.  When we purge the session
        -- from the heap we match the timestamp in the heap with the timestamp
        -- in the Map, if the latest timestamp is newer and has not expired we
        -- reinsert the key in the heap.
        --
        -- XXX if the key is an Int, we can also use an IntMap for slightly
        -- better performance.
        --
        let curTime = max sessionEventTime timestamp
            mOld = Map.lookup key sessionKeyValueMap
        let done fb = do
                -- deleting a key from the heap is expensive, so we never
                -- delete a key from heap, we just purge it from the Map and it
                -- gets purged from the heap on timeout. We just need an extra
                -- lookup in the Map when the key is purged from the heap, that
                -- should not be expensive.
                --
                let (mp, cnt) = case mOld of
                        Nothing -> (sessionKeyValueMap, sessionCount)
                        Just _ -> (Map.delete key sessionKeyValueMap
                                  , sessionCount - 1)
                return $ session
                    { sessionCurTime = curTime
                    , sessionEventTime = curTime
                    , sessionCount = cnt
                    , sessionKeyValueMap = mp
                    , sessionOutputStream = value (key, fb)
                    }
            partial fs1 = do
                let acc = Tuple' timestamp fs1
                (hp1, mp1, out1, cnt1) <- do
                        let vars = (sessionTimerHeap, sessionKeyValueMap,
                                           K.nil, sessionCount)
                        case mOld of
                            -- inserting new entry
                            Nothing -> do
                                -- Eject a session from heap and map is needed
                                eject <- ejectPred sessionCount
                                (hp, mp, out, cnt) <-
                                    if eject
                                    then ejectOne vars
                                    else return vars

                                -- Insert the new session in heap
                                let expiry = addToAbsTime timestamp timeoutMs
                                    hp' = H.insert (Entry expiry key) hp
                                 in return (hp', mp, out, cnt + 1)
                            -- updating old entry
                            Just _ -> return vars

                let mp2 = Map.insert key acc mp1
                return $ SessionState
                    { sessionCurTime = curTime
                    , sessionEventTime = curTime
                    , sessionCount = cnt1
                    , sessionTimerHeap = hp1
                    , sessionKeyValueMap = mp2
                    , sessionOutputStream = out1
                    }
        res0 <- do
            case mOld of
                Nothing -> initial
                Just (Tuple' _ acc) -> return $ FL.Partial acc
        case res0 of
            FL.Done fb -> done fb
            FL.Partial fs -> do
                res <- step fs val
                case res of
                    FL.Done fb -> done fb
                    FL.Partial fs1 -> partial fs1

    -- Got a timer tick event
    sstep sessionState@SessionState{..} Nothing =
        let curTime = addToAbsTime sessionCurTime tickMs
        in ejectExpired sessionState curTime

    flush session@SessionState{..} = do
        (hp', mp', out, count) <-
            ejectAll
                ( sessionTimerHeap
                , sessionKeyValueMap
                , K.nil
                , sessionCount
                )
        return $ session
            { sessionCount = count
            , sessionTimerHeap = hp'
            , sessionKeyValueMap = mp'
            , sessionOutputStream = out
            }

    -- delete from map and output the fold accumulator
    ejectEntry hp mp out cnt acc key = do
        sess <- extract acc
        let out1 = (key, sess) `K.cons` out
        let mp1 = Map.delete key mp
        return (hp, mp1, out1, cnt - 1)

    ejectAll (hp, mp, out, !cnt) = do
        let hres = H.uncons hp
        case hres of
            Just (Entry _ key, hp1) -> do
                r <- case Map.lookup key mp of
                    Nothing -> return (hp1, mp, out, cnt)
                    Just (Tuple' _ acc) -> ejectEntry hp1 mp out cnt acc key
                ejectAll r
            Nothing -> do
                assert (Map.null mp) (return ())
                return (hp, mp, out, cnt)

    ejectOne (hp, mp, out, !cnt) = do
        let hres = H.uncons hp
        case hres of
            Just (Entry expiry key, hp1) ->
                case Map.lookup key mp of
                    Nothing -> ejectOne (hp1, mp, out, cnt)
                    Just (Tuple' latestTS acc) -> do
                        let expiry1 = addToAbsTime latestTS timeoutMs
                        if not reset || expiry1 <= expiry
                        then ejectEntry hp1 mp out cnt acc key
                        else
                            -- reset the session timeout and continue
                            let hp2 = H.insert (Entry expiry1 key) hp1
                            in ejectOne (hp2, mp, out, cnt)
            Nothing -> do
                assert (Map.null mp) (return ())
                return (hp, mp, out, cnt)

    ejectExpired session@SessionState{..} curTime = do
        (hp', mp', out, count) <-
            ejectLoop sessionTimerHeap sessionKeyValueMap K.nil sessionCount
        return $ session
            { sessionCurTime = curTime
            , sessionCount = count
            , sessionTimerHeap = hp'
            , sessionKeyValueMap = mp'
            , sessionOutputStream = out
            }

        where

        ejectLoop hp mp out !cnt = do
            let hres = H.uncons hp
            case hres of
                Just (Entry expiry key, hp1) -> do
                    (eject, force) <-
                        if curTime >= expiry
                        then return (True, False)
                        else do
                            r <- ejectPred cnt
                            return (r, r)
                    if eject
                    then
                        case Map.lookup key mp of
                            Nothing -> ejectLoop hp1 mp out cnt
                            Just (Tuple' latestTS acc) -> do
                                let expiry1 = addToAbsTime latestTS timeoutMs
                                if expiry1 <= curTime || not reset || force
                                then do
                                    (hp2,mp1,out1,cnt1) <-
                                        ejectEntry hp1 mp out cnt acc key
                                    ejectLoop hp2 mp1 out1 cnt1
                                else
                                    -- reset the session timeout and continue
                                    let hp2 = H.insert (Entry expiry1 key) hp1
                                    in ejectLoop hp2 mp out cnt
                    else return (hp, mp, out, cnt)
                Nothing -> do
                    assert (Map.null mp) (return ())
                    return (hp, mp, out, cnt)

    -- merge timer events in the stream
    stream = Serial.map Just str `Par.parallelFst` repeatM timer
    timer = do
        liftIO $ threadDelay (round $ tick * 1000000)
        return Nothing

-- | Same as 'classifySessionsBy' with a timer tick of 1 second and keepalive
-- option set to 'True'.
--
-- @
-- classifyKeepAliveSessions = classifySessionsBy 1 True
-- @
--
-- /Pre-release/
--
{-# INLINABLE classifyKeepAliveSessions #-}
classifyKeepAliveSessions ::
       (IsStream t, MonadAsync m, Ord k)
    => (Int -> m Bool) -- ^ predicate to eject sessions on session count
    -> Double -- ^ session inactive timeout
    -> Fold m a b -- ^ Fold to be applied to session payload data
    -> t m (AbsTime, (k, a)) -- ^ timestamp, (session key, session data)
    -> t m (k, b)
classifyKeepAliveSessions = classifySessionsBy 1 True

------------------------------------------------------------------------------
-- Keyed tumbling windows
------------------------------------------------------------------------------

-- Tumbling windows is a special case of sliding windows where the window slide
-- is the same as the window size. Or it can be a special case of session
-- windows where the reset flag is set to False.

-- XXX instead of using the early termination flag in the stream, we can use an
-- early terminating fold instead.

{-
-- | Split the stream into fixed size chunks of specified size. Within each
-- such chunk fold the elements in buckets identified by the keys. A particular
-- bucket fold can be terminated early if a closing flag is encountered in an
-- element for that key.
--
-- @since 0.7.0
{-# INLINABLE classifyChunksOf #-}
classifyChunksOf
    :: (IsStream t, MonadAsync m, Ord k)
    => Int              -- ^ window size
    -> Fold m a b       -- ^ Fold to be applied to window events
    -> t m (k, a, Bool) -- ^ window key, data, close event
    -> t m (k, b)
classifyChunksOf wsize = classifyChunksBy wsize False
-}

-- | Same as 'classifySessionsBy' with a timer tick of 1 second and keepalive
-- option set to 'False'.
--
-- @
-- classifySessionsOf = classifySessionsBy 1 False
-- @
--
-- /Pre-release/
--
{-# INLINABLE classifySessionsOf #-}
classifySessionsOf ::
       (IsStream t, MonadAsync m, Ord k)
    => (Int -> m Bool) -- ^ predicate to eject sessions on session count
    -> Double -- ^ time window size
    -> Fold m a b -- ^ Fold to be applied to session data
    -> t m (AbsTime, (k, a)) -- ^ timestamp, (session key, session data)
    -> t m (k, b)
classifySessionsOf = classifySessionsBy 1 False

------------------------------------------------------------------------------
-- Nested Split
------------------------------------------------------------------------------

-- | @splitInnerBy splitter joiner stream@ splits the inner containers @f a@ of
-- an input stream @t m (f a)@ using the @splitter@ function. Container
-- elements @f a@ are collected until a split occurs, then all the elements
-- before the split are joined using the @joiner@ function.
--
-- For example, if we have a stream of @Array Word8@, we may want to split the
-- stream into arrays representing lines separated by '\n' byte such that the
-- resulting stream after a split would be one array for each line.
--
-- CAUTION! This is not a true streaming function as the container size after
-- the split and merge may not be bounded.
--
-- /Pre-release/
{-# INLINE splitInnerBy #-}
splitInnerBy
    :: (IsStream t, Monad m)
    => (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> t m (f a)
    -> t m (f a)
splitInnerBy splitter joiner xs =
    D.fromStreamD $ D.splitInnerBy splitter joiner $ D.toStreamD xs

-- | Like 'splitInnerBy' but splits assuming the separator joins the segment in
-- a suffix style.
--
-- /Pre-release/
{-# INLINE splitInnerBySuffix #-}
splitInnerBySuffix
    :: (IsStream t, Monad m, Eq (f a), Monoid (f a))
    => (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> t m (f a)
    -> t m (f a)
splitInnerBySuffix splitter joiner xs =
    D.fromStreamD $ D.splitInnerBySuffix splitter joiner $ D.toStreamD xs
