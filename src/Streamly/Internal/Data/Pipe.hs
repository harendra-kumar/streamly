-- |
-- Module      : Streamly.Internal.Data.Pipe
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- There are three fundamental types in streamly. They are streams
-- ("Streamly.Prelude"), pipes ("Streamly.Internal.Data.Pipe") and folds ("Streamly.Data.Fold").
-- Streams are sources or producers of values, multiple sources can be merged
-- into a single source but a source cannot be split into multiple stream
-- sources.  Folds are sinks or consumers, a stream can be split and
-- distributed to multiple folds but the results cannot be merged back into a
-- stream source again. Pipes are transformations, a stream source can be split
-- and distributed to multiple pipes each pipe can apply its own transform on
-- the stream and the results can be merged back into a single pipe. Pipes can
-- be attached to a source to produce a source or they can be attached to a
-- fold to produce a fold, or multiple pipes can be merged or zipped into a
-- single pipe.
--
-- > import qualified Streamly.Internal.Data.Pipe as P

module Streamly.Internal.Data.Pipe
    (
    -- * Pipe Type
      Pipe

    -- * Pipes
    -- ** Mapping
    , map
    , mapM

    {-
    -- ** Filtering
    , lfilter
    , lfilterM
    -- , ldeleteBy
    -- , luniq

    {-
    -- ** Mapping Filters
    , lmapMaybe
    , lmapMaybeM

    -- ** Scanning Filters
    , lfindIndices
    , lelemIndices

    -- ** Insertion
    -- | Insertion adds more elements to the stream.

    , linsertBy
    , lintersperseM

    -- ** Reordering
    , lreverse
    -}

    -- * Parsing
    -- ** Trimming
    , ltake
    -- , lrunFor -- time
    , ltakeWhile
    {-
    , ltakeWhileM
    , ldrop
    , ldropWhile
    , ldropWhileM
    -}

    -- ** Splitting
    -- | Streams can be split into segments in space or in time. We use the
    -- term @chunk@ to refer to a spatial length of the stream (spatial window)
    -- and the term @session@ to refer to a length in time (time window).

    -- In imperative terms, grouped folding can be considered as a nested loop
    -- where we loop over the stream to group elements and then loop over
    -- individual groups to fold them to a single value that is yielded in the
    -- output stream.

    -- *** By Chunks
    , chunksOf
    , sessionsOf

    -- *** By Elements
    , splitBy
    , splitSuffixBy
    , splitSuffixBy'
    -- , splitPrefixBy
    , wordsBy

    -- *** By Sequences
    , splitOn
    , splitSuffixOn
    -- , splitPrefixOn
    -- , wordsOn

    -- Keeping the delimiters
    , splitOn'
    , splitSuffixOn'
    -- , splitPrefixOn'

    -- Splitting by multiple sequences
    -- , splitOnAny
    -- , splitSuffixOnAny
    -- , splitPrefixOnAny

    -- ** Grouping
    , groups
    , groupsBy
    , groupsRollingBy
    -}

    -- * Composing Pipes
    , tee
    , zipWith
    , compose

    {-
    -- * Distributing
    -- |
    -- The 'Applicative' instance of a distributing 'Fold' distributes one copy
    -- of the stream to each fold and combines the results using a function.
    --
    -- @
    --
    --                 |-------Fold m a b--------|
    -- ---stream m a---|                         |---m (b,c,...)
    --                 |-------Fold m a c--------|
    --                 |                         |
    --                            ...
    -- @
    --
    -- To compute the average of numbers in a stream without going through the
    -- stream twice:
    --
    -- >>> let avg = (/) <$> FL.sum <*> fmap fromIntegral FL.length
    -- >>> FL.foldl' avg (S.enumerateFromTo 1.0 100.0)
    -- 50.5
    --
    -- The 'Semigroup' and 'Monoid' instances of a distributing fold distribute
    -- the input to both the folds and combines the outputs using Monoid or
    -- Semigroup instances of the output types:
    --
    -- >>> import Data.Monoid (Sum)
    -- >>> FL.foldl' (FL.head <> FL.last) (fmap Sum $ S.enumerateFromTo 1.0 100.0)
    -- Just (Sum {getSum = 101.0})
    --
    -- The 'Num', 'Floating', and 'Fractional' instances work in the same way.

    , tee
    , distribute

    -- * Partitioning
    -- |
    -- Direct items in the input stream to different folds using a function to
    -- select the fold. This is useful to demultiplex the input stream.
    -- , partitionByM
    -- , partitionBy
    , partition

    -- * Demultiplexing
    , demux
    -- , demuxWith
    , demux_
    -- , demuxWith_

    -- * Classifying
    , classify
    -- , classifyWith

    -- * Unzipping
    , unzip
    -- These can be expressed using lmap/lmapM and unzip
    -- , unzipWith
    -- , unzipWithM

    -- * Nested Folds
    -- , concatMap
    -- , chunksOf
    , duplicate  -- experimental

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
-}
    )
where

-- import Control.Concurrent (threadDelay, forkIO, killThread)
-- import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
-- import Control.Exception (SomeException(..), catch, mask)
-- import Control.Monad (void)
-- import Control.Monad.Catch (throwM)
-- import Control.Monad.IO.Class (MonadIO(..))
-- import Control.Monad.Trans (lift)
-- import Control.Monad.Trans.Control (control)
-- import Data.Functor.Identity (Identity)
-- import Data.Heap (Entry(..))
-- import Data.Map.Strict (Map)
-- import Data.Maybe (fromJust, isJust, isNothing)

-- import Foreign.Storable (Storable(..))
import Prelude
       hiding (id, filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, replicate, concatMap, mconcat, foldMap, unzip,
               span, splitAt, break, mapM)

-- import qualified Data.Heap as H
-- import qualified Data.Map.Strict as Map
-- import qualified Prelude

-- import Streamly.Prelude (MonadAsync, parallel)
-- import Streamly.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Pipe.Types
       (Pipe(..), PipeState(..), Step(..), zipWith, tee, map, compose)
-- import Streamly.Internal.Data.Array.Foreign.Types (Array)
-- import Streamly.Internal.Ring.Foreign (Ring)
-- import Streamly.Internal.Data.Stream.Serial (SerialT)
-- import Streamly.Internal.Data.Stream.StreamK (IsStream())
-- import Streamly.Internal.Data.Time.Units
-- (AbsTime, MilliSecond64(..), addToAbsTime, diffAbsTime, toRelTime,
-- toAbsTime)

-- import Streamly.Internal.Data.Strict

-- import qualified Streamly.Internal.Data.Array.Foreign.Types as A
-- import qualified Streamly.Prelude as S
-- import qualified Streamly.Internal.Data.Stream.StreamD as D
-- import qualified Streamly.Internal.Data.Stream.StreamK as K
-- import qualified Streamly.Internal.Data.Stream.Prelude as P

------------------------------------------------------------------------------
-- Pipes
------------------------------------------------------------------------------

-- | Lift a monadic function to a 'Pipe'.
--
-- @since 0.7.0
{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> Pipe m a b
mapM f = Pipe consume undefined ()
    where
    consume _ a = do
        r <- f a
        return $ Yield r (Consume ())
{-
------------------------------------------------------------------------------
-- Filtering
------------------------------------------------------------------------------

-- | Include only those elements that pass a predicate.
--
-- >>> FL.foldl (lfilter (> 5) FL.sum) [1..10]
-- 40
--
-- @since 0.7.0
{-# INLINABLE lfilter #-}
lfilter :: Monad m => (a -> Bool) -> Fold m a r -> Fold m a r
lfilter f (Fold step begin done) = Fold step' begin done
  where
    step' x a = if f a then step x a else return x

-- | Like 'lfilter' but with a monadic predicate.
--
-- @since 0.7.0
{-# INLINABLE lfilterM #-}
lfilterM :: Monad m => (a -> m Bool) -> Fold m a r -> Fold m a r
lfilterM f (Fold step begin done) = Fold step' begin done
  where
    step' x a = do
      use <- f a
      if use then step x a else return x

-- | Take first 'n' elements from the stream and discard the rest.
--
-- @since 0.7.0
{-# INLINABLE ltake #-}
ltake :: Monad m => Int -> Fold m a b -> Fold m a b
ltake n (Fold step initial done) = Fold step' initial' done'
    where
    initial' = fmap (Tuple' 0) initial
    step' (Tuple' i r) a = do
        if i < n
        then do
            res <- step r a
            return $ Tuple' (i + 1) res
        else return $ Tuple' i r
    done' (Tuple' _ r) = done r

-- | Takes elements from the input as long as the predicate succeeds.
--
-- @since 0.7.0
{-# INLINABLE ltakeWhile #-}
ltakeWhile :: Monad m => (a -> Bool) -> Fold m a b -> Fold m a b
ltakeWhile predicate (Fold step initial done) = Fold step' initial' done'
    where
    initial' = fmap Left' initial
    step' (Left' r) a = do
        if predicate a
        then fmap Left' $ step r a
        else return (Right' r)
    step' r _ = return r
    done' (Left' r) = done r
    done' (Right' r) = done r

------------------------------------------------------------------------------
-- Grouping/Splitting
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Grouping without looking at elements
------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------
--

-- | @splitAt n f1 f2@ composes folds @f1@ and @f2@ such that first @n@
-- elements of its input are sent to fold @f1@ and the rest of the stream is
-- sent to fold @f2@.
--
-- > let splitAt_ n xs = FL.foldl' (FL.splitAt n FL.toList FL.toList) $ S.fromList xs
--
-- >>> splitAt_ 6 "Hello World!"
-- > ("Hello ","World!")
--
-- >>> splitAt_ (-1) [1,2,3]
-- > ([],[1,2,3])
--
-- >>> splitAt_ 0 [1,2,3]
-- > ([],[1,2,3])
--
-- >>> splitAt_ 1 [1,2,3]
-- > ([1],[2,3])
--
-- >>> splitAt_ 3 [1,2,3]
-- > ([1,2,3],[])
--
-- >>> splitAt_ 4 [1,2,3]
-- > ([1,2,3],[])
--
-- This can be considered as a two-fold version of 'ltake' where we take both
-- the segments instead of discarding the leftover.
--
-- @since 0.7.0
{-# INLINE splitAt #-}
splitAt
    :: Monad m
    => Int
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
splitAt n (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step init extract
    where
      init  = Tuple3' <$> return n <*> initialL <*> initialR

      step (Tuple3' i xL xR) input =
        if i > 0
        then stepL xL input >>= (\a -> return (Tuple3' (i - 1) a xR))
        else stepR xR input >>= (\b -> return (Tuple3' i xL b))

      extract (Tuple3' _ a b) = (,) <$> extractL a <*> extractR b

------------------------------------------------------------------------------
-- N-ary APIs
------------------------------------------------------------------------------

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

-- | Group the input stream into groups of @n@ elements each and then fold each
-- group using the provided fold function.
--
-- >> S.toList $ FL.chunksOf 2 FL.sum (S.enumerateFromTo 1 10)
-- > [3,7,11,15,19]
--
-- This can be considered as an n-fold version of 'ltake' where we apply
-- 'ltake' repeatedly on the leftover stream until the stream exhausts.
--
-- @since 0.7.0
{-# INLINE chunksOf #-}
chunksOf
    :: (IsStream t, Monad m)
    => Int -> Fold m a b -> t m a -> t m b
chunksOf n f m = D.fromStreamD $ D.groupsOf n f (D.toStreamD m)

-- | Transform a fold from a pure input to a 'Maybe' input, consuming only
-- 'Just' values.
{-# INLINE lcatMaybes #-}
lcatMaybes :: Monad m => Fold m a b -> Fold m (Maybe a) b
lcatMaybes = lfilter isJust . lmap fromJust

-- XXX we can implement this by repeatedly applying the 'lrunFor' fold.
-- XXX add this example after fixing the serial stream rate control
-- >>> S.toList $ S.take 5 $ sessionsOf 1 FL.sum $ constRate 2 $ S.enumerateFrom 1
-- > [3,7,11,15,19]
--
-- | Group the input stream into windows of @n@ second each and then fold each
-- group using the provided fold function.
--
-- @since 0.7.0
{-# INLINE sessionsOf #-}
sessionsOf
    :: (IsStream t, MonadAsync m)
    => Double -> Fold m a b -> t m a -> t m b
sessionsOf n f xs =
    splitSuffixBy' isNothing (lcatMaybes f)
        (S.intersperseByTime n (return Nothing) (S.map Just xs))

------------------------------------------------------------------------------
-- Element Aware APIs
------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------

-- | Break the input stream into two groups, the first group takes the input as
-- long as the predicate applied to the first element of the stream and next
-- input element holds 'True', the second group takes the rest of the input.
--
_spanBy
    :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
_spanBy cmp (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step init extract

    where
      init = Tuple3' <$> initialL <*> initialR <*> return (Tuple' Nothing True)

      step (Tuple3' a b (Tuple' (Just frst) isFirstG)) input =
        if cmp frst input && isFirstG
        then stepL a input
              >>= (\a' -> return (Tuple3' a' b (Tuple' (Just frst) isFirstG)))
        else stepR b input
              >>= (\a' -> return (Tuple3' a a' (Tuple' Nothing False)))

      step (Tuple3' a b (Tuple' Nothing isFirstG)) input =
        if isFirstG
        then stepL a input
              >>= (\a' -> return (Tuple3' a' b (Tuple' (Just input) isFirstG)))
        else stepR b input
              >>= (\a' -> return (Tuple3' a a' (Tuple' Nothing False)))

      extract (Tuple3' a b _) = (,) <$> extractL a <*> extractR b

-- | Span as long as the predicate is 'True'. @span p f1 f2@ composes folds
-- @f1@ and @f2@ such that the composed fold continues sending the input to
-- @f1@ as long as the predicate @p@ is 'True'.  The rest of the input is sent
-- to @f2@.
--
-- > let span_ p xs = FL.foldl' (FL.span p FL.toList FL.toList) $ S.fromList xs
--
-- >>> span_ (< 1) [1,2,3]
-- > ([],[1,2,3])
--
-- >>> span_ (< 2) [1,2,3]
-- > ([1],[2,3])
--
-- >>> span_ (< 4) [1,2,3]
-- > ([1,2,3],[])
--
-- This can be considered as a two-fold version of 'ltakeWhile' where we take
-- both the segments instead of discarding the leftover.
--
-- @since 0.7.0
{-# INLINE span #-}
span
    :: Monad m
    => (a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
span p (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step init extract

    where
      init = Tuple3' <$> initialL <*> initialR <*> return True

      step (Tuple3' a b isFirstG) input =
        if isFirstG && p input
        then stepL a input >>= (\a' -> return (Tuple3' a' b True))
        else stepR b input >>= (\a' -> return (Tuple3' a a' False))

      extract (Tuple3' a b _) = (,) <$> extractL a <*> extractR b

-- |
-- > break p = span (not . p)
--
-- Break as soon as the predicate becomes 'True'. @break p f1 f2@ composes
-- folds @f1@ and @f2@ such that @f1@ stops receiving input as soon as the
-- predicate @p@ becomes 'True'. The rest of the input is sent to @f2@.
--
-- This is the binary version of 'splitBy'.
--
-- > let break_ p xs = FL.foldl' (FL.break p FL.toList FL.toList) $ S.fromList xs
--
-- >>> break_ (< 1) [3,2,1]
-- > ([3,2,1],[])
--
-- >>> break_ (< 2) [3,2,1]
-- > ([3,2],[1])
--
-- >>> break_ (< 4) [3,2,1]
-- > ([],[3,2,1])
--
-- @since 0.7.0
{-# INLINE break #-}
break
    :: Monad m
    => (a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
break p = span (not . p)

-- | Like 'spanBy' but applies the predicate in a rolling fashion i.e.
-- predicate is applied to the previous and the next input elements.
{-# INLINE _spanRollingBy #-}
_spanRollingBy
    :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
_spanRollingBy cmp (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step init extract

  where
    init = Tuple3' <$> initialL <*> initialR <*> return Nothing

    step (Tuple3' a b (Just frst)) input =
      if cmp input frst
      then stepL a input >>= (\a' -> return (Tuple3' a' b (Just input)))
      else stepR b input >>= (\b' -> return (Tuple3' a b' (Just input)))

    step (Tuple3' a b Nothing) input =
      stepL a input >>= (\a' -> return (Tuple3' a' b (Just input)))

    extract (Tuple3' a b _) = (,) <$> extractL a <*> extractR b

------------------------------------------------------------------------------
-- N-ary APIs
------------------------------------------------------------------------------
--
-- | @groupsBy cmp f $ S.fromList [a,b,c,...]@ assigns the element @a@ to the
-- first group, if @a \`cmp` b@ is 'True' then @b@ is also assigned to the same
-- group.  If @a \`cmp` c@ is 'True' then @c@ is also assigned to the same
-- group and so on. When the comparison fails a new group is started. Each
-- group is folded using the fold @f@.
--
-- >>> S.toList $ FL.groupsBy (>) FL.toList $ S.fromList [1,3,7,0,2,5]
-- > [[1,3,7],[0,2,5]]
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
-- successive elements in the input stream. @groupsRollingBy cmp f $ S.fromList
-- [a,b,c,...]@ assigns the element @a@ to the first group, if @a \`cmp` b@ is
-- 'True' then @b@ is also assigned to the same group.  If @b \`cmp` c@ is
-- 'True' then @c@ is also assigned to the same group and so on. When the
-- comparison fails a new group is started. Each group is folded using the fold
-- @f@.
--
-- >>> S.toList $ FL.groupsRollingBy (\a b -> a + 1 == b) FL.toList $ S.fromList [1,2,3,7,8,9]
-- > [[1,2,3],[7,8,9]]
--
-- @since 0.7.0
{-# INLINE groupsRollingBy #-}
groupsRollingBy
    :: (IsStream t, Monad m)
    => (a -> a -> Bool)
    -> Fold m a b
    -> t m a
    -> t m b
groupsRollingBy cmp f m =  D.fromStreamD $ D.groupsRollingBy cmp f (D.toStreamD m)

-- |
-- > groups = groupsBy (==)
-- > groups = groupsRollingBy (==)
--
-- Groups a contiguous span of equal elements together in one group.
--
-- >>> S.toList $ FL.groups FL.toList $ S.fromList [1,1,2,2]
-- > [[1,1],[2,2]]
--
-- @since 0.7.0
groups :: (IsStream t, Monad m, Eq a) => Fold m a b -> t m a -> t m b
groups = groupsBy (==)

------------------------------------------------------------------------------
-- Binary splitting on a separator
------------------------------------------------------------------------------

{-
-- | Find the first occurrence of the specified sequence in the input stream
-- and break the input stream into two parts, the first part consisting of the
-- stream before the sequence and the second part consisting of the sequence
-- and the rest of the stream.
--
-- > let breakOn_ pat xs = FL.foldl' (FL.breakOn pat FL.toList FL.toList) $ S.fromList xs
--
-- >>> breakOn_ "dear" "Hello dear world!"
-- > ("Hello ","dear world!")
--
{-# INLINE breakOn #-}
breakOn :: Monad m => Array a -> Fold m a b -> Fold m a c -> Fold m a (b,c)
breakOn pat f m = undefined
-}

------------------------------------------------------------------------------
-- N-ary split on a predicate
------------------------------------------------------------------------------

-- TODO: Use a Splitter configuration similar to the "split" package to make it
-- possible to express all splitting combinations. In general, we can have
-- infix/suffix/prefix/condensing of separators, dropping both leading/trailing
-- separators. We can have a single split operation taking the splitter config
-- as argument.

-- | Split a stream on separator elements determined by a predicate, dropping
-- the separator.  Separators are not considered part of stream segments on
-- either side of it instead they are treated as infixed between two stream
-- segments. For example, with @.@ as separator, @"a.b.c"@ would be parsed as
-- @["a","b","c"]@. When @.@ is in leading or trailing position it is still
-- considered as infixed, treating the first or the last segment as empty.  For
-- example, @".a."@ would be parsed as @["","a",""]@.  This operation is
-- opposite of 'intercalate'.
--
-- Let's use the following definition for illustration:
--
-- > splitBy_ p xs = S.toList $ FL.splitBy p (FL.toList) (S.fromList xs)
--
-- >>> splitBy_ (== '.') ""
-- [""]
--
-- >>> splitBy_ (== '.') "."
-- ["",""]
--
-- >>> splitBy_ (== '.') ".a"
-- > ["","a"]
--
-- >>> splitBy_ (== '.') "a."
-- > ["a",""]
--
-- >>> splitBy_ (== '.') "a.b"
-- > ["a","b"]
--
-- >>> splitBy_ (== '.') "a..b"
-- > ["a","","b"]
--
-- This can be considered as an n-fold version of 'break' where we apply
-- 'break' successively on the input stream, dropping the first element
-- of the second segment after each break.
--
-- @since 0.7.0
{-# INLINE splitBy #-}
splitBy
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitBy predicate f m =
    D.fromStreamD $ D.splitBy predicate f (D.toStreamD m)

-- | Like 'splitBy' but the separator is treated as part of the previous
-- stream segment (suffix).  Therefore, when the separator is in trailing
-- position, no empty segment is considered to follow it. For example, @"a.b."@
-- would be parsed as @["a","b"]@ instead of @["a","b",""]@ as in the case of
-- 'splitBy'.
--
-- > splitSuffixBy_ p xs = S.toList $ FL.splitSuffixBy p (FL.toList) (S.fromList xs)
--
-- >>> splitSuffixBy_ (== '.') ""
-- []
--
-- >>> splitSuffixBy_ (== '.') "."
-- [""]
--
-- >>> splitSuffixBy_ (== '.') "a"
-- ["a"]
--
-- >>> splitSuffixBy_ (== '.') ".a"
-- > ["","a"]
--
-- >>> splitSuffixBy_ (== '.') "a."
-- > ["a"]
--
-- >>> splitSuffixBy_ (== '.') "a.b"
-- > ["a","b"]
--
-- >>> splitSuffixBy_ (== '.') "a.b."
-- > ["a","b"]
--
-- >>> splitSuffixBy_ (== '.') "a..b.."
-- > ["a","","b",""]
--
-- > lines = splitSuffixBy (== '\n')
--
-- This can be considered as an n-fold version of 'breakPost' where we apply
-- 'breakPost' successively on the input stream, dropping the first element
-- of the second segment after each break.
--
-- @since 0.7.0
{-# INLINE splitSuffixBy #-}
splitSuffixBy
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitSuffixBy predicate f m =
    D.fromStreamD $ D.splitSuffixBy predicate f (D.toStreamD m)

-- | Like 'splitBy' but ignores repeated separators or separators in leading
-- or trailing position. Therefore, @"..a..b.."@ would be parsed as
-- @["a","b"]@.  In other words, it treats the input like words separated by
-- whitespace elements determined by the predicate.
--
-- > wordsBy' p xs = S.toList $ FL.wordsBy p (FL.toList) (S.fromList xs)
--
-- >>> wordsBy' (== ',') ""
-- > []
--
-- >>> wordsBy' (== ',') ","
-- > []
--
-- >>> wordsBy' (== ',') ",a,,b,"
-- > ["a","b"]
--
-- > words = wordsBy isSpace
--
-- @since 0.7.0
{-# INLINE wordsBy #-}
wordsBy
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
wordsBy predicate f m =
    D.fromStreamD $ D.wordsBy predicate f (D.toStreamD m)

-- XXX we should express this using the Splitter config.
--
-- We can get splitSuffixBy' by appending the suffix to the output segments
-- produced by splitSuffixBy. However, it may add an additional suffix if the last
-- fragment did not have a suffix in the first place.

-- | Like 'splitSuffixBy' but keeps the suffix in the splits.
--
-- > splitSuffixBy'_ p xs = S.toList $ FL.splitSuffixBy' p (FL.toList) (S.fromList xs)
--
-- >>> splitSuffixBy'_ (== '.') ""
-- []
--
-- >>> splitSuffixBy'_ (== '.') "."
-- ["."]
--
-- >>> splitSuffixBy'_ (== '.') "a"
-- ["a"]
--
-- >>> splitSuffixBy'_ (== '.') ".a"
-- > [".","a"]
--
-- >>> splitSuffixBy'_ (== '.') "a."
-- > ["a."]
--
-- >>> splitSuffixBy'_ (== '.') "a.b"
-- > ["a.","b"]
--
-- >>> splitSuffixBy'_ (== '.') "a.b."
-- > ["a.","b."]
--
-- >>> splitSuffixBy'_ (== '.') "a..b.."
-- > ["a.",".","b.","."]
--
-- This can be considered as an n-fold version of 'breakPost' where we apply
-- 'breakPost' successively on the input stream.
--
-- @since 0.7.0
{-# INLINE splitSuffixBy' #-}
splitSuffixBy'
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitSuffixBy' predicate f m =
    D.fromStreamD $ D.splitSuffixBy' predicate f (D.toStreamD m)

------------------------------------------------------------------------------
-- Split on a delimiter sequence
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

-- | Split the stream on both sides of a separator sequence, dropping the
-- separator.
--
-- For illustration, let's define a function that operates on pure lists:
--
-- @
-- splitOn_ pat xs = S.toList $ FL.splitOn (A.fromList pat) (FL.toList) (S.fromList xs)
-- @
--
-- >>> splitOn_ "" "hello"
-- > ["h","e","l","l","o"]
--
-- >>> splitOn_ "hello" ""
-- > [""]
--
-- >>> splitOn_ "hello" "hello"
-- > ["",""]
--
-- >>> splitOn_ "x" "hello"
-- > ["hello"]
--
-- >>> splitOn_ "h" "hello"
-- > ["","ello"]
--
-- >>> splitOn_ "o" "hello"
-- > ["hell",""]
--
-- >>> splitOn_ "e" "hello"
-- > ["h","llo"]
--
-- >>> splitOn_ "l" "hello"
-- > ["he","","o"]
--
-- >>> splitOn_ "ll" "hello"
-- > ["he","o"]
--
-- 'splitOn' is an inverse of 'intercalate'. The following law always holds:
--
-- > intercalate . splitOn == id
--
-- The following law holds when the separator is non-empty and contains none of
-- the elements present in the input lists:
--
-- > splitOn . intercalate == id
--
-- The following law always holds:
--
-- > concat . splitOn . intercalate == concat
--
-- @since 0.7.0
{-# INLINE splitOn #-}
splitOn
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitOn patt f m = D.fromStreamD $ D.splitOn patt f (D.toStreamD m)

{-
-- This can be implemented easily using Rabin Karp
-- | Split on any one of the given patterns.
{-# INLINE splitOnAny #-}
splitOnAny
    :: (IsStream t, Monad m, Storable a, Integral a)
    => [Array a] -> Fold m a b -> t m a -> t m b
splitOnAny subseq f m = undefined -- D.fromStreamD $ D.splitOnAny f subseq (D.toStreamD m)
-}

-- | Like 'splitSuffixBy' but the separator is a sequence of elements, instead
-- of a predicate for a single element.
--
-- > splitSuffixOn_ pat xs = S.toList $ FL.splitSuffixOn (A.fromList pat) (FL.toList) (S.fromList xs)
--
-- >>> splitSuffixOn_ "." ""
-- [""]
--
-- >>> splitSuffixOn_ "." "."
-- [""]
--
-- >>> splitSuffixOn_ "." "a"
-- ["a"]
--
-- >>> splitSuffixOn_ "." ".a"
-- > ["","a"]
--
-- >>> splitSuffixOn_ "." "a."
-- > ["a"]
--
-- >>> splitSuffixOn_ "." "a.b"
-- > ["a","b"]
--
-- >>> splitSuffixOn_ "." "a.b."
-- > ["a","b"]
--
-- >>> splitSuffixOn_ "." "a..b.."
-- > ["a","","b",""]
--
-- > lines = splitSuffixOn "\n"
--
-- @since 0.7.0
{-# INLINE splitSuffixOn #-}
splitSuffixOn
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitSuffixOn patt f m =
    D.fromStreamD $ D.splitSuffixOn False patt f (D.toStreamD m)

{-
-- | Like 'splitOn' but drops any empty splits.
--
{-# INLINE wordsOn #-}
wordsOn
    :: (IsStream t, Monad m, Storable a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
wordsOn subseq f m = undefined -- D.fromStreamD $ D.wordsOn f subseq (D.toStreamD m)
-}

-- XXX use a non-monadic intersperse to remove the MonadAsync constraint.
--
-- | Like 'splitOn' but splits the separator as well, as an infix token.
--
-- > splitOn'_ pat xs = S.toList $ FL.splitOn' (A.fromList pat) (FL.toList) (S.fromList xs)
--
-- >>> splitOn'_ "" "hello"
-- > ["h","","e","","l","","l","","o"]
--
-- >>> splitOn'_ "hello" ""
-- > [""]
--
-- >>> splitOn'_ "hello" "hello"
-- > ["","hello",""]
--
-- >>> splitOn'_ "x" "hello"
-- > ["hello"]
--
-- >>> splitOn'_ "h" "hello"
-- > ["","h","ello"]
--
-- >>> splitOn'_ "o" "hello"
-- > ["hell","o",""]
--
-- >>> splitOn'_ "e" "hello"
-- > ["h","e","llo"]
--
-- >>> splitOn'_ "l" "hello"
-- > ["he","l","","l","o"]
--
-- >>> splitOn'_ "ll" "hello"
-- > ["he","ll","o"]
--
-- @since 0.7.0
{-# INLINE splitOn' #-}
splitOn'
    :: (IsStream t, MonadAsync m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitOn' patt f m = S.intersperseM (foldl' f (A.read patt)) $ splitOn patt f m

-- | Like 'splitSuffixOn' but keeps the suffix intact in the splits.
--
-- > splitSuffixOn'_ pat xs = S.toList $ FL.splitSuffixOn' (A.fromList pat) (FL.toList) (S.fromList xs)
--
-- >>> splitSuffixOn'_ "." ""
-- [""]
--
-- >>> splitSuffixOn'_ "." "."
-- ["."]
--
-- >>> splitSuffixOn'_ "." "a"
-- ["a"]
--
-- >>> splitSuffixOn'_ "." ".a"
-- > [".","a"]
--
-- >>> splitSuffixOn'_ "." "a."
-- > ["a."]
--
-- >>> splitSuffixOn'_ "." "a.b"
-- > ["a.","b"]
--
-- >>> splitSuffixOn'_ "." "a.b."
-- > ["a.","b."]
--
-- >>> splitSuffixOn'_ "." "a..b.."
-- > ["a.",".","b.","."]
--
-- @since 0.7.0
{-# INLINE splitSuffixOn' #-}
splitSuffixOn'
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitSuffixOn' patt f m =
    D.fromStreamD $ D.splitSuffixOn True patt f (D.toStreamD m)

{-
-- This can be implemented easily using Rabin Karp
-- | Split post any one of the given patterns.
{-# INLINE splitSuffixOnAny #-}
splitSuffixOnAny
    :: (IsStream t, Monad m, Storable a, Integral a)
    => [Array a] -> Fold m a b -> t m a -> t m b
splitSuffixOnAny subseq f m = undefined
    -- D.fromStreamD $ D.splitPostAny f subseq (D.toStreamD m)
-}

------------------------------------------------------------------------------
-- Reorder in sequence
------------------------------------------------------------------------------

{-
-- Buffer until the next element in sequence arrives. The function argument
-- determines the difference in sequence numbers. This could be useful in
-- implementing sequenced streams, for example, TCP reassembly.
{-# INLINE reassembleBy #-}
reassembleBy
    :: (IsStream t, Monad m)
    => Fold m a b
    -> (a -> a -> Int)
    -> t m a
    -> t m b
reassembleBy = undefined
-}

------------------------------------------------------------------------------
-- Distributing
------------------------------------------------------------------------------
--
-- | Distribute one copy of the stream to each fold and zip the results.
--
-- @
--                 |-------Fold m a b--------|
-- ---stream m a---|                         |---m (b,c)
--                 |-------Fold m a c--------|
-- @
-- >>> FL.foldl' (FL.tee FL.sum FL.length) (S.enumerateFromTo 1.0 100.0)
-- (5050.0,100)
--
-- @since 0.7.0
{-# INLINE tee #-}
tee :: Monad m => Fold m a b -> Fold m a c -> Fold m a (b,c)
tee f1 f2 = (,) <$> f1 <*> f2

{-# INLINE foldNil #-}
foldNil :: Monad m => Fold m a [b]
foldNil = Fold step begin done  where
  begin = return []
  step _ _ = return []
  done = return

{-# INLINE foldCons #-}
foldCons :: Monad m => Fold m a b -> Fold m a [b] -> Fold m a [b]
foldCons (Fold stepL beginL doneL) (Fold stepR beginR doneR) =
    Fold step begin done

    where

    begin = Tuple' <$> beginL <*> beginR
    step (Tuple' xL xR) a = Tuple' <$> stepL xL a <*> stepR xR a
    done (Tuple' xL xR) = (:) <$> (doneL xL) <*> (doneR xR)

-- XXX use "List" instead of "[]"?, use Array for output to scale it to a large
-- number of consumers?
--
-- | Distribute one copy of the stream to each fold and collect the results in
-- a container.
--
-- @
--
--                 |-------Fold m a b--------|
-- ---stream m a---|                         |---m [b]
--                 |-------Fold m a b--------|
--                 |                         |
--                            ...
-- @
--
-- >>> FL.foldl' (FL.distribute [FL.sum, FL.length]) (S.enumerateFromTo 1 5)
-- [15,5]
--
-- This is the consumer side dual of the producer side 'sequence' operation.
--
-- @since 0.7.0
{-# INLINE distribute #-}
distribute :: Monad m => [Fold m a b] -> Fold m a [b]
distribute [] = foldNil
distribute (x:xs) = foldCons x (distribute xs)

------------------------------------------------------------------------------
-- Partitioning
------------------------------------------------------------------------------
--
-- | Partition the input over two folds using an 'Either' partitioning
-- predicate.
--
-- @
--
--                                     |-------Fold b x--------|
-- -----stream m a --> (Either b c)----|                       |----(x,y)
--                                     |-------Fold c y--------|
-- @
--
-- Send input to either fold randomly:
--
-- >>> import System.Random (randomIO)
-- >>> randomly a = randomIO >>= \x -> return $ if x then Left a else Right a
-- >>> FL.foldl' (FL.partitionByM randomly FL.length FL.length) (S.enumerateFromTo 1 100)
-- (59,41)
--
-- Send input to the two folds in a proportion of 2:1:
--
-- @
-- import Data.IORef (newIORef, readIORef, writeIORef)
-- proportionately m n = do
--  ref <- newIORef $ cycle $ concat [replicate m Left, replicate n Right]
--  return $ \\a -> do
--      r <- readIORef ref
--      writeIORef ref $ tail r
--      return $ head r a
--
-- main = do
--  f <- proportionately 2 1
--  r <- FL.foldl' (FL.partitionByM f FL.length FL.length) (S.enumerateFromTo (1 :: Int) 100)
--  print r
-- @
-- @
-- (67,33)
-- @
--
-- This is the consumer side dual of the producer side 'mergeBy' operation.
--
-- @since 0.7.0
{-# INLINE partitionByM #-}
partitionByM :: Monad m
    => (a -> m (Either b c)) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
partitionByM f (Fold stepL beginL doneL) (Fold stepR beginR doneR) =

    Fold step begin done

    where

    begin = Tuple' <$> beginL <*> beginR
    step (Tuple' xL xR) a = do
        r <- f a
        case r of
            Left b -> Tuple' <$> stepL xL b <*> return xR
            Right c -> Tuple' <$> return xL <*> stepR xR c
    done (Tuple' xL xR) = (,) <$> doneL xL <*> doneR xR

-- Note: we could use (a -> Bool) instead of (a -> Either b c), but the latter
-- makes the signature clearer as to which case belongs to which fold.
-- XXX need to check the performance in both cases.

-- | Same as 'partitionByM' but with a pure partition function.
--
-- Count even and odd numbers in a stream:
--
-- @
-- >>> let f = FL.partitionBy (\\n -> if even n then Left n else Right n)
--                       (fmap (("Even " ++) . show) FL.length)
--                       (fmap (("Odd "  ++) . show) FL.length)
--   in FL.foldl' f (S.enumerateFromTo 1 100)
-- ("Even 50","Odd 50")
-- @
--
-- @since 0.7.0
{-# INLINE partitionBy #-}
partitionBy :: Monad m
    => (a -> Either b c) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
partitionBy f = partitionByM (return . f)

-- | Compose two folds such that the combined fold accepts a stream of 'Either'
-- and routes the 'Left' values to the first fold and 'Right' values to the
-- second fold.
--
-- > partition = partitionBy id
--
-- @since 0.7.0
{-# INLINE partition #-}
partition :: Monad m
    => Fold m b x -> Fold m c y -> Fold m (Either b c) (x, y)
partition = partitionBy id

{-
-- | Send one item to each fold in a round-robin fashion. This is the consumer
-- side dual of producer side 'mergeN' operation.
--
-- partitionN :: Monad m => [Fold m a b] -> Fold m a [b]
-- partitionN fs = Fold step begin done
-}

-- Demultiplex an input element into a number of typed variants. We want to
-- statically restrict the target values within a set of predefined types, an
-- enumeration of a GADT. We also want to make sure that the Map contains only
-- those types and the full set of those types.  Instead of Map it should
-- probably be a lookup-table using an array and not in GC memory.
--
-- This is the consumer side dual of the producer side 'mux' operation (XXX to
-- be implemented).

-- | Split the input stream based on a key field and fold each split using a
-- specific fold collecting the results in a map from the keys to the results.
-- Useful for cases like protocol handlers to handle different type of packets
-- using different handlers.
--
-- @
--
--                             |-------Fold m a b
-- -----stream m a-----Map-----|
--                             |-------Fold m a b
--                             |
--                                       ...
-- @
--
-- @since 0.7.0
{-# INLINE demuxWith #-}
demuxWith :: (Monad m, Ord k)
    => (a -> k) -> Map k (Fold m a b) -> Fold m a (Map k b)
demuxWith f kv = Fold step initial extract

    where

    initial = return kv
    step mp a =
        -- XXX should we raise an exception in Nothing case?
        -- Ideally we should enforce that it is a total map over k so that look
        -- up never fails
        -- XXX we could use a monadic update function for a single lookup and
        -- update in the map.
        let k = f a
        in case Map.lookup k mp of
            Nothing -> return mp
            Just (Fold step' acc extract') -> do
                !r <- acc >>= \x -> step' x a
                return $ Map.insert k (Fold step' (return r) extract') mp
    extract = Prelude.mapM (\(Fold _ acc e) -> acc >>= e)

-- | Fold a stream of key value pairs using a map of specific folds for each
-- key into a map from keys to the results of fold outputs of the corresponding
-- values.
--
-- @
-- > let table = Data.Map.fromList [(\"SUM", FL.sum), (\"PRODUCT", FL.product)]
--       input = S.fromList [(\"SUM",1),(\"PRODUCT",2),(\"SUM",3),(\"PRODUCT",4)]
--   in FL.foldl' (FL.demux table) input
-- One 1
-- Two 2
-- @
--
-- @since 0.7.0
{-# INLINE demux #-}
demux :: (Monad m, Ord k)
    => Map k (Fold m a b) -> Fold m (k, a) (Map k b)
demux fs = demuxWith fst (Map.map (lmap snd) fs)

-- | Split the input stream based on a key field and fold each split using a
-- specific fold without collecting the results. Useful for cases like protocol
-- handlers to handle different type of packets.
--
-- @
--
--                             |-------Fold m a ()
-- -----stream m a-----Map-----|
--                             |-------Fold m a ()
--                             |
--                                       ...
-- @
--
--
-- @since 0.7.0

-- demuxWith_ can be slightly faster than demuxWith because we do not need to
-- update the Map in this case. This may be significant only if the map is
-- large.
{-# INLINE demuxWith_ #-}
demuxWith_ :: (Monad m, Ord k)
    => (a -> k) -> Map k (Fold m a b) -> Fold m a ()
demuxWith_ f kv = Fold step initial extract

    where

    initial = do
        Prelude.mapM (\(Fold s i e) ->
            i >>= \r -> return (Fold s (return r) e)) kv
    step mp a =
        -- XXX should we raise an exception in Nothing case?
        -- Ideally we should enforce that it is a total map over k so that look
        -- up never fails
        case Map.lookup (f a) mp of
            Nothing -> return mp
            Just (Fold step' acc _) -> do
                _ <- acc >>= \x -> step' x a
                return mp
    extract mp = Prelude.mapM (\(Fold _ acc e) -> acc >>= e) mp >> return ()

-- | Given a stream of key value pairs and a map from keys to folds, fold the
-- values for each key using the corresponding folds, discarding the outputs.
--
-- @
-- > let prn = FL.drainBy print
-- > let table = Data.Map.fromList [(\"ONE", prn), (\"TWO", prn)]
--       input = S.fromList [(\"ONE",1),(\"TWO",2)]
--   in FL.foldl' (FL.demux_ table) input
-- One 1
-- Two 2
-- @
--
-- @since 0.7.0
{-# INLINE demux_ #-}
demux_ :: (Monad m, Ord k) => Map k (Fold m a ()) -> Fold m (k, a) ()
demux_ fs = demuxWith_ fst (Map.map (lmap snd) fs)

-- XXX instead of a Map we could yield the results as a pure stream as they
-- complete. We could then concatMap the fold results to implement the
-- windowing combinators.
--
-- | Split the input stream based on a key field and fold each split using the
-- given fold. Useful for map/reduce, bucketizing the input in different bins
-- or for generating histograms.
--
-- @
-- > let input = S.fromList [(\"ONE",1),(\"ONE",1.1),(\"TWO",2), (\"TWO",2.2)]
--   in FL.foldl' (FL.classify FL.toListRev) input
-- fromList [(\"ONE",[1.1,1.0]),(\"TWO",[2.2,2.0])]
-- @
--
-- @since 0.7.0
{-# INLINE classifyWith #-}
classifyWith :: (Monad m, Ord k) => (a -> k) -> Fold m a b -> Fold m a (Map k b)
classifyWith f (Fold step initial extract) = Fold step' initial' extract'

    where

    initial' = return Map.empty
    step' kv a =
        let k = f a
        in case Map.lookup k kv of
            Nothing -> do
                x <- initial
                r <- step x a
                return $ Map.insert k r kv
            Just x -> do
                r <- step x a
                return $ Map.insert k r kv
    extract' = Prelude.mapM extract

-- | Given an input stream of key value pairs and a fold for values, fold all
-- the values belonging to each key.  Useful for map/reduce, bucketizing the
-- input in different bins or for generating histograms.
--
-- @
-- > let input = S.fromList [(\"ONE",1),(\"ONE",1.1),(\"TWO",2), (\"TWO",2.2)]
--   in FL.foldl' (FL.classify FL.toListRev) input
-- fromList [(\"ONE",[1.1,1.0]),(\"TWO",[2.2,2.0])]
-- @
--
-- Same as:
--
-- > classify fld = classifyWith fst (lmap snd fld)
--
-- @since 0.7.0
{-# INLINE classify #-}
classify :: (Monad m, Ord k) => Fold m a b -> Fold m (k, a) (Map k b)
classify fld = classifyWith fst (lmap snd fld)

------------------------------------------------------------------------------
-- Unzipping
------------------------------------------------------------------------------
--
-- | Like 'unzipWith' but with a monadic splitter function.
--
-- @since 0.7.0
{-# INLINE unzipWithM #-}
unzipWithM :: Monad m
    => (a -> m (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
unzipWithM f (Fold stepL beginL doneL) (Fold stepR beginR doneR) =
    Fold step begin done

    where

    step (Tuple' xL xR) a = do
        (b,c) <- f a
        Tuple' <$> stepL xL b <*> stepR xR c
    begin = Tuple' <$> beginL <*> beginR
    done (Tuple' xL xR) = (,) <$> doneL xL <*> doneR xR

-- | Split elements in the input stream into two parts using a pure splitter
-- function, direct each part to a different fold and zip the results.
--
-- @since 0.7.0
{-# INLINE unzipWith #-}
unzipWith :: Monad m
    => (a -> (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
unzipWith f = unzipWithM (return . f)

-- | Send the elements of tuples in a stream of tuples through two different
-- folds.
--
-- @
--
--                           |-------Fold a x--------|
-- -----Stream m x----(a,b)--|                       |----m (x,y)
--                           |-------Fold b y--------|
--
-- @
--
-- This is the consumer side dual of the producer side 'zip' operation.
--
-- @since 0.7.0
{-# INLINE unzip #-}
unzip :: Monad m => Fold m a x -> Fold m b y -> Fold m (a,b) (x,y)
unzip = unzipWith id

------------------------------------------------------------------------------
-- Nesting
------------------------------------------------------------------------------
--
-- | Modify the fold such that when the fold is done, instead of returning the
-- accumulator, it returns a fold. The returned fold starts from where we left
-- i.e. it uses the last accumulator value as the initial value of the
-- accumulator. Thus we can resume the fold later and feed it more input.
--
-- >> do
-- >    more <- FL.foldl (FL.duplicate FL.sum) (S.enumerateFromTo 1 10)
-- >    evenMore <- FL.foldl (FL.duplicate more) (S.enumerateFromTo 11 20)
-- >    FL.foldl evenMore (S.enumerateFromTo 21 30)
-- > 465
--
-- @since 0.7.0
{-# INLINABLE duplicate #-}
duplicate :: Applicative m => Fold m a b -> Fold m a (Fold m a b)
duplicate (Fold step begin done) =
    Fold step begin (\x -> pure (Fold step (pure x) done))

{-
-- All the stream flattening transformations can also be applied to a fold
-- input stream.

-- | This can be used to apply all the stream generation operations on folds.
lconcatMap ::(IsStream t, Monad m) => (a -> t m b)
    -> Fold m b c
    -> Fold m a c
lconcatMap s f1 f2 = undefined
-}

-- All the grouping transformation that we apply to a stream can also be
-- applied to a fold input stream.

{-
-- | Group the input stream into groups of elements between @low@ and @high@.
-- Collection starts in chunks of @low@ and then keeps doubling until we reach
-- @high@. Each chunk is folded using the provided fold function.
--
-- This could be useful, for example, when we are folding a stream of unknown
-- size to a stream of arrays and we want to minimize the number of
-- allocations.
--
-- @
--
-- XXX we should be able to implement it with parsers/terminating folds.
--
{-# INLINE lchunksInRange #-}
lchunksInRange :: Monad m
    => Int -> Int -> Fold m a b -> Fold m b c -> Fold m a c
lchunksInRange low high (Fold step1 initial1 extract1)
                        (Fold step2 initial2 extract2) = undefined
-}

-- | Group the input stream into groups of @n@ elements each and then fold each
-- group using the provided fold function.
--
-- @
--
-- -----Fold m a b----|-Fold n a c-|-Fold n a c-|-...-|----Fold m a c
--
-- @
--
{-# INLINE lchunksOf #-}
lchunksOf :: Monad m => Int -> Fold m a b -> Fold m b c -> Fold m a c
lchunksOf n (Fold step1 initial1 extract1) (Fold step2 initial2 extract2) =
    Fold step' initial' extract'

    where

    initial' = (Tuple3' 0) <$> initial1 <*> initial2
    step' (Tuple3' i r1 r2) a = do
        if i < n
        then do
            res <- step1 r1 a
            return $ Tuple3' (i + 1) res r2
        else do
            res <- extract1 r1
            acc2 <- step2 r2 res

            i1 <- initial1
            acc1 <- step1 i1 a
            return $ Tuple3' 1 acc1 acc2
    extract' (Tuple3' _ _ r) = extract2 r

-- | Group the input stream into windows of n second each and then fold each
-- group using the provided fold function.
--
-- For example, we can copy and distribute a stream to multiple folds where
-- each fold can group the input differently e.g. by one second, one minute and
-- one hour windows respectively and fold each resulting stream of folds.
--
-- @
--
-- -----Fold m a b----|-Fold n a c-|-Fold n a c-|-...-|----Fold m a c
--
-- @
{-# INLINE lsessionsOf #-}
lsessionsOf :: MonadAsync m => Double -> Fold m a b -> Fold m b c -> Fold m a c
lsessionsOf n (Fold step1 initial1 extract1) (Fold step2 initial2 extract2) =
    Fold step' initial' extract'

    where

    -- XXX MVar may be expensive we need a cheaper synch mechanism here
    initial' = do
        i1 <- initial1
        i2 <- initial2
        mv1 <- liftIO $ newMVar i1
        mv2 <- liftIO $ newMVar (Right i2)
        t <- control $ \run ->
            mask $ \restore -> do
                tid <- forkIO $ catch (restore $ void $ run (timerThread mv1 mv2))
                                      (handleChildException mv2)
                run (return tid)
        return $ Tuple3' t mv1 mv2
    step' acc@(Tuple3' _ mv1 _) a = do
            r1 <- liftIO $ takeMVar mv1
            res <- step1 r1 a
            liftIO $ putMVar mv1 res
            return acc
    extract' (Tuple3' tid _ mv2) = do
        r2 <- liftIO $ takeMVar mv2
        liftIO $ killThread tid
        case r2 of
            Left e -> throwM e
            Right x -> extract2 x

    timerThread mv1 mv2 = do
        liftIO $ threadDelay (round $ n * 1000000)

        r1 <- liftIO $ takeMVar mv1
        i1 <- initial1
        liftIO $ putMVar mv1 i1

        res1 <- extract1 r1
        r2 <- liftIO $ takeMVar mv2
        res <- case r2 of
                    Left _ -> return r2
                    Right x -> fmap Right $ step2 x res1
        liftIO $ putMVar mv2 res
        timerThread mv1 mv2

    handleChildException ::
        MVar (Either SomeException a) -> SomeException -> IO ()
    handleChildException mv2 e = do
        r2 <- takeMVar mv2
        let r = case r2 of
                    Left _ -> r2
                    Right _ -> Left e
        putMVar mv2 r

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

-- | @classifySessionsBy tick timeout reset f stream@ groups together all input
-- stream elements that belong to the same session. @timeout@ is the maximum
-- lifetime of a session in seconds. All elements belonging to a session are
-- purged after this duration.  If "reset" is 'Ture' then the timeout is reset
-- after every event received in the session. Session duration is measured
-- using the timestamp of the first element seen for that session.  To detect
-- session timeouts, a monotonic event time clock is maintained using the
-- timestamps seen in the inputs and a timer with a tick duration specified by
-- @tick@.
--
-- @session key@ is a key that uniquely identifies the session for the given
-- element, @timestamp@ characterizes the time when the input element was
-- generated, this is an absolute time measured from some @Epoch@. @session
-- close@ is a boolean indicating whether this element marks the closing of the
-- session. When an input element with @session close@ set to @True@ is seen
-- the session is purged immediately.
--
-- All the input elements belonging to a session are collected using the fold
-- @f@.  The session key and the fold result are emitted in the output stream
-- when the session is purged either via the session close event or via the
-- session lifetime timeout.
--
-- @since 0.7.0
{-# INLINABLE classifySessionsBy #-}
classifySessionsBy
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ timer tick in seconds
    -> Double         -- ^ session timeout
    -> Bool           -- ^ reset the timeout when an event is received
    -> Fold m a b     -- ^ Fold to be applied to session events
    -> t m (k, a, Bool, AbsTime) -- ^ session key, timestamp, close event, data
    -> t m (k, b)
classifySessionsBy tick timeout reset (Fold step initial extract) str =
    S.concatMap (\(Tuple4' _ _ _ s) -> s) $ S.scanlM' sstep szero stream

    where

    timeoutMs = toRelTime (round (timeout * 1000) :: MilliSecond64)
    tickMs = toRelTime (round (tick * 1000) :: MilliSecond64)
    szero = Tuple4' (toAbsTime (0 :: MilliSecond64)) H.empty Map.empty S.nil

    -- Got a new stream input element
    sstep (Tuple4' evTime hp mp _) (Just (key, a, closing, ts)) =
        -- XXX we should use a heap in pinned memory to scale it to a large
        -- size
        --
        -- deleting a key from the heap is expensive, so we never delete a
        -- key, we just purge it from the Map and it gets purged from the
        -- heap on timeout. We just need an extra lookup in the Map when
        -- the key is purged from the heap, that should not be expensive.
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
        let accumulate v = do
                Tuple' _ old <- maybe (initial >>= return . Tuple' ts) return v
                new <- step old a
                return $ Tuple' ts new
        in if closing
           then do
                let (r, mp') = Map.updateLookupWithKey (\_ _ -> Nothing) key mp
                Tuple' _ acc <- accumulate r
                res <- extract acc
                return $ Tuple4' evTime hp mp' (S.value (key, res))
           else do
                    let r = Map.lookup key mp
                    acc <- accumulate r
                    let mp' = Map.insert key acc mp
                    let hp' =
                            case r of
                                Nothing ->
                                    let expiry = addToAbsTime ts timeoutMs
                                    in H.insert (Entry expiry key) hp
                                Just _ -> hp
                    -- Event time is maintained as monotonically increasing
                    -- time. If we have lagged behind any of the timestamps
                    -- seen then we increase it to match the latest time seen
                    -- in the timestamps. We also increase it on timer ticks.
                    return $ Tuple4' (max evTime ts) hp' mp' S.nil

    -- Got a timer tick event
    -- XXX can we yield the entries without accumulating them?
    sstep (Tuple4' evTime heap sessions _) Nothing = do
        (hp', mp', out) <- go heap sessions S.nil
        return $ Tuple4' curTime hp' mp' out

        where

        curTime = addToAbsTime evTime tickMs
        go hp mp out = do
            let hres = H.uncons hp
            case hres of
                Just (Entry ts key, hp') -> do
                    let duration = diffAbsTime curTime ts
                    if duration >= timeoutMs
                    then do
                        let (r, mp') = Map.updateLookupWithKey
                                            (\_ _ -> Nothing) key mp
                        case r of
                            Nothing -> go hp' mp' out
                            Just (Tuple' latestTS acc) -> do
                                let dur = diffAbsTime curTime latestTS
                                if dur >= timeoutMs || not reset
                                then do
                                    sess <- extract acc
                                    go hp' mp' ((key, sess) `S.cons` out)
                                else
                                    -- reset the session timeout
                                    let expiry = addToAbsTime latestTS timeoutMs
                                        hp'' = H.insert (Entry expiry key) hp'
                                        mp'' = Map.insert key (Tuple' latestTS acc) mp'
                                    in go hp'' mp'' out
                    else return (hp, mp, out)
                Nothing -> return (hp, mp, out)

    -- merge timer events in the stream
    stream = S.map Just str `parallel` S.repeatM timer
    timer = do
        liftIO $ threadDelay (round $ tick * 1000000)
        return Nothing

-- | Like 'classifySessionsOf' but the session is kept alive if an event is
-- received within the session window. The session times out and gets closed
-- only if no event is received within the specified session window size.
--
-- @since 0.7.0
{-# INLINABLE classifyKeepAliveSessions #-}
classifyKeepAliveSessions
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ session inactive timeout
    -> Fold m a b     -- ^ Fold to be applied to session payload data
    -> t m (k, a, Bool, AbsTime) -- ^ session key, data, close flag, timestamp
    -> t m (k, b)
classifyKeepAliveSessions timeout = classifySessionsBy 1 timeout True

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

-- | Split the stream into fixed size time windows of specified interval in
-- seconds. Within each such window, fold the elements in buckets identified by
-- the keys. A particular bucket fold can be terminated early if a closing flag
-- is encountered in an element for that key. Once a fold is terminated the key
-- and value for that bucket are emitted in the output stream.
--
-- Session @timestamp@ in the input stream is an absolute time from some epoch,
-- characterizing the time when the input element was generated.  To detect
-- session window end, a monotonic event time clock is maintained synced with
-- the timestamps with a clock resolution of 1 second.
--
-- @since 0.7.0
{-# INLINABLE classifySessionsOf #-}
classifySessionsOf
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ time window size
    -> Fold m a b     -- ^ Fold to be applied to window events
    -> t m (k, a, Bool, AbsTime) -- ^ window key, data, close flag, timestamp
    -> t m (k, b)
classifySessionsOf interval = classifySessionsBy 1 interval False
-}
