{-# OPTIONS_GHC -Wno-orphans  #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This is an Internal module consisting of released, unreleased and
-- unimplemented APIs. For stable and released APIs please see
-- "Streamly.Prelude" module. This module provides documentation only for the
-- unreleased and unimplemented APIs. For documentation on released APIs please
-- see "Streamly.Prelude" module.

module Streamly.Internal.Data.Stream.IsStream
    (
    -- * Stream Types
    -- ** Serial Streams
      SerialT
    , Serial
    , WSerialT
    , WSerial

    -- ** Speculative Streams
    , AheadT
    , Ahead

    -- ** Asynchronous Streams
    , AsyncT
    , Async
    , WAsyncT
    , WAsync
    , ParallelT
    , Parallel
    , mkAsync

    -- ** Zipping Streams
    , ZipSerialM
    , ZipSerial
    , ZipAsyncM
    , ZipAsync

    -- * Stream Type Adapters
    , IsStream ()

    , serially
    , wSerially
    , asyncly
    , aheadly
    , wAsyncly
    , parallely
    , zipSerially
    , zipAsyncly
    , adapt

    -- * Type Synonyms
    , MonadAsync

    -- * Construction
    -- ** Primitives
    , K.nil
    , K.nilM
    , K.cons
    , (K..:)

    , consM
    , (|:)

    -- ** From Values
    , yield
    , yieldM
    , repeat
    , repeatM
    , replicate
    , replicateM

    -- ** Enumeration
    , Enumerable (..)
    , enumerate
    , enumerateTo

    -- ** Time Enumeration
    , times
    , absTimes
    , relTimes
    , durations
    , ticks
    , timeout
    , currentTime

    -- ** From Generators
    , unfoldr
    , unfoldrM
    , unfold
    , unfold0
    , fromIndices
    , fromIndicesM

    -- ** Iteration
    , iterate
    , iterateM

    -- ** Cyclic Elements
    , K.mfix

    -- ** From Containers
    , P.fromList
    , fromListM
    , K.fromFoldable
    , fromFoldableM
    , fromPrimIORef
    , fromCallback

    -- * Elimination

    -- ** Deconstruction
    , uncons
    , tail
    , init

    -- ** Folding
    -- ** Right Folds
    , foldrM
    , foldrS
    , foldrT
    , foldr

    -- ** Left Folds
    , foldl'
    , foldl1'
    , foldlM'

    -- ** Composable Left Folds
    -- | See "Streamly.Internal.Data.Fold"
    , fold
    , foldMany
    , foldSequence
    , foldIterate

    -- ** Parsers
    -- | See "Streamly.Internal.Data.Parser"
    , parse
    , parseK
    , parseD
    , parseMany
    , parseManyD
    , parseManyTill
    , parseSequence
    , parseIterate

    -- ** Concurrent Folds
    , foldAsync
    , (|$.)
    , (|&.)

    -- ** Full Folds

    -- -- ** To Summary (Full Folds)
    , drain
    , last
    , length
    , sum
    , product
    , mconcat

    -- -- ** To Summary (Maybe) (Full Folds)
    , maximumBy
    , maximum
    , minimumBy
    , minimum
    , the

    -- ** Lazy Folds
    -- -- ** To Containers (Full Folds)
    , toList
    , toListRev
    , toPure
    , toPureRev

    -- ** Composable Left Folds

    , toStream    -- XXX rename to write?
    , toStreamRev -- XXX rename to writeRev?

    -- ** Partial Folds

    -- -- ** To Elements (Partial Folds)
    , drainN
    , drainWhile

    -- -- | Folds that extract selected elements of a stream or their properties.
    , (!!)
    , head
    , headElse
    , findM
    , find
    , lookup
    , findIndex
    , elemIndex

    -- -- ** To Boolean (Partial Folds)
    , null
    , elem
    , notElem
    , all
    , any
    , and
    , or

    -- ** Multi-Stream folds
    -- Full equivalence
    , eqBy
    , cmpBy

    -- finding subsequences
    , isPrefixOf
    , isSuffixOf
    , isInfixOf
    , isSubsequenceOf

    -- trimming sequences
    , stripPrefix
    , stripSuffix
    -- , stripInfix
    , dropPrefix
    , dropInfix
    , dropSuffix

    -- * Transformation
    , transform

    -- ** Mapping
    , Serial.map
    , sequence
    , mapM
    , smapM
    -- $smapM_Notes

    -- ** Special Maps
    , mapM_
    , trace
    , trace_
    , tap
    , tapOffsetEvery
    , tapAsync
    , tapRate
    , pollCounts

    -- ** Scanning
    -- ** Left scans
    , scanl'
    , scanlM'
    , scanlMAfter'
    , postscanl'
    , postscanlM'
    , prescanl'
    , prescanlM'
    , scanl1'
    , scanl1M'

    -- ** Scan Using Fold
    , scan
    , postscan

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

    -- ** Concurrent Transformation
    , D.mkParallel
    -- Par.mkParallel
    , applyAsync
    , (|$)
    , (|&)

    -- ** Filtering

    , filter
    , filterM

    -- ** Deleting Elements
    , deleteBy
    , uniq
    -- , uniqBy -- by predicate e.g. to remove duplicate "/" in a path
    -- , uniqOn -- to remove duplicate sequences
    -- , pruneBy -- dropAround + uniqBy - like words

    -- ** Inserting Elements

    , insertBy
    , intersperseM
    , intersperseM_
    , intersperse
    , intersperseSuffix
    , intersperseSuffix_
    , interspersePrefix_
    , intersperseSuffixBySpan
    , justsOfTimeout
    -- , intersperseBySpan
    -- , intersperseByIndices -- using an index function/stream

    -- time domain intersperse
    -- , intersperseByTime
    -- , intersperseByEvent
    , interjectSuffix
    , delay
    , delayPost
    , delayPre

    -- ** Indexing
    , indexed
    , indexedR
    , timestamped
    , timeIndexed
    -- , timestampedR -- timer

    -- ** Reordering
    , reverse
    , reverse'

    -- ** Trimming
    , take
    -- , takeGE
    -- , takeBetween
    , takeByTime
    -- , takeEnd
    , takeWhile
    , takeWhileM
    -- , takeWhileEnd
    , drop
    , dropByTime
    -- , dropEnd
    , dropWhile
    , dropWhileM
    -- , dropWhileEnd
    -- , dropAround

    -- ** Breaking

    -- Nary
    , chunksOf
    , chunksOf2
    , arraysOf
    , intervalsOf

    -- ** Searching
    -- -- *** Searching Elements
    , findIndices
    , elemIndices

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
    -- , splitOnPrefix

    -- , splitBy
    , splitWithSuffix
    -- , splitByPrefix
    , wordsBy -- stripAndCompactBy

    -- -- *** Splitting By Sequences
    , splitOnSeq
    , splitOnSuffixSeq
    -- , splitOnPrefixSeq

    -- Keeping the delimiters
    , splitBySeq
    , splitWithSuffixSeq
    -- , splitByPrefixSeq
    -- , wordsBySeq

    -- Splitting using multiple sequence separators
    -- , splitOnAnySeq
    -- , splitOnAnySuffixSeq
    -- , splitOnAnyPrefixSeq

    -- -- *** Splitting By Streams
    -- -- | Splitting a stream using another stream as separator.

    -- Nested splitting
    , splitInnerBy
    , splitInnerBySuffix

    -- ** Grouping
    -- In imperative terms, grouped folding can be considered as a nested loop
    -- where we loop over the stream to group elements and then loop over
    -- individual groups to fold them to a single value that is yielded in the
    -- output stream.

    -- , groupScan

    , groups
    , groupsBy
    , groupsByRolling

    -- ** Group map
    , rollingMapM
    , rollingMap

    -- * Windowed Classification

    -- | Split the stream into windows or chunks in space or time. Each window
    -- can be associated with a key, all events associated with a particular
    -- key in the window can be folded to a single result. The stream is split
    -- into windows of specified size, the window can be terminated early if
    -- the closing flag is specified in the input stream.
    --
    -- The term "chunk" is used for a space window and the term "session" is
    -- used for a time window.

    -- ** Tumbling Windows
    -- | A new window starts after the previous window is finished.

    -- , classifyChunksOf
    , classifySessionsBy
    , classifySessionsOf

    -- ** Keep Alive Windows
    -- | The window size is extended if an event arrives within the specified
    -- window size. This can represent sessions with idle or inactive timeout.

    -- , classifyKeepAliveChunks
    , classifyKeepAliveSessions

    {-
    -- ** Sliding Windows
    -- | A new window starts after the specified slide from the previous
    -- window. Therefore windows can overlap.
    , classifySlidingChunks
    , classifySlidingSessions
    -}
    -- ** Sliding Window Buffers
    -- , slidingChunkBuffer
    -- , slidingSessionBuffer

    -- * Combining Streams

    -- ** Appending
    , serial
    , append

    -- ** Interleaving
    , interleave
    , interleaveMin
    , interleaveSuffix
    , interleaveInfix

    , wSerial
    , Serial.wSerialFst
    , Serial.wSerialMin

    -- ** Scheduling
    , ahead
    , async
    , wAsync
    , roundrobin

    -- ** Parallel
    , parallel
    , Par.parallelFst
    , Par.parallelMin

    -- ** Merging
    -- , merge
    , mergeBy
    , mergeByM
    , mergeAsyncBy
    , mergeAsyncByM

    -- ** Zipping
    , Z.zipWith
    , Z.zipWithM
    , Z.zipAsyncWith
    , Z.zipAsyncWithM

    -- ** Flattening a Container of Streams
    , concatFoldableWith
    , concatMapFoldableWith
    , concatForFoldableWith

    -- ** Flattening a Stream of Streams
    , concat
    , concatM
    , concatMap
    , concatMapM
    , concatMapWith
    , concatSmapMWith
    -- , bindWith

    -- ** Flattening Using Unfolds
    , concatUnfold
    , concatUnfoldInterleave
    , concatUnfoldRoundrobin

    -- ** Flattening a Tree of Streams
    , iterateMapWith

    -- ** Flattening a Graph of Streams
    , iterateSmapMWith

    -- ** Inserting Streams in Streams
    , gintercalate
    , gintercalateSuffix
    , intercalate
    , intercalateSuffix
    , interpose
    , interposeSuffix
    -- , interposeBy

    -- * Exceptions
    , before
    , after_
    , after
    , bracket_
    , bracket
    , onException
    , finally_
    , finally
    , ghandle
    , handle

    -- * Generalize Inner Monad
    , hoist
    , generally

    -- * Transform Inner Monad
    , liftInner
    , usingReaderT
    , runReaderT
    , evalStateT
    , usingStateT
    , runStateT

    -- * Maybe Streams
    , catMaybes
    , mapMaybe
    , mapMaybeM

    -- * Either Streams
    , lefts
    , rights
    , iterateMapLeftsWith

    -- * Concurrency Control
    , maxThreads
    , maxBuffer

    -- * Rate Limiting
    , Rate (..)
    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate

    -- * Diagnostics
    , inspectMode

    -- * Deprecated
    , K.once
    , each
    , scanx
    , foldx
    , foldxM
    , foldr1
    , runStream
    , runN
    , runWhile
    , fromHandle
    , toHandle
    )
where

import Streamly.Internal.Data.SVar (MonadAsync, Rate (..))
import Streamly.Internal.Data.Stream.Ahead (AheadT, Ahead, aheadly)
import Streamly.Internal.Data.Stream.Async
       ( AsyncT, Async, WAsyncT, WAsync, mkAsync, asyncly
       , wAsyncly)
import Streamly.Internal.Data.Stream.Combinators
      ( inspectMode, maxBuffer, maxThreads, rate, avgRate, minRate
      , maxRate, constRate)
import Streamly.Internal.Data.Stream.Parallel
       ( ParallelT, Parallel, parallely)
import Streamly.Internal.Data.Stream.StreamK (IsStream(), adapt)
import Streamly.Internal.Data.Stream.Serial
       ( SerialT, WSerialT, Serial, WSerial, serially
       , wSerially)
import Streamly.Internal.Data.Stream.Zip
       ( ZipSerialM, ZipSerial, ZipAsyncM, ZipAsync, zipSerially, zipAsyncly)

import qualified Streamly.Internal.Data.Stream.Prelude as P
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Stream.Parallel as Par
import qualified Streamly.Internal.Data.Stream.Zip as Z

import Streamly.Internal.Data.Stream.IsStream.Generate
import Streamly.Internal.Data.Stream.IsStream.Eliminate
import Streamly.Internal.Data.Stream.IsStream.Transform
import Streamly.Internal.Data.Stream.IsStream.Nesting
import Streamly.Internal.Data.Stream.IsStream.Exception
import Streamly.Internal.Data.Stream.IsStream.Lift

import Prelude hiding
       ( filter, drop, dropWhile, take, takeWhile, zipWith, foldr
       , foldl, map, mapM, mapM_, sequence, all, any, sum, product, elem
       , notElem, maximum, minimum, head, last, tail, length, null
       , reverse, iterate, init, and, or, lookup, foldr1, (!!)
       , scanl, scanl1, replicate, concatMap, span, splitAt, break
       , repeat, concat, mconcat)
