{-# OPTIONS_GHC -Wno-orphans #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Prelude
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module is designed to be imported qualified:
--
-- @
-- import qualified Streamly.Prelude as S
-- @
--
-- Functions with the suffix @M@ are general functions that work on monadic
-- arguments. The corresponding functions without the suffix @M@ work on pure
-- arguments and can in general be derived from their monadic versions but are
-- provided for convenience and for consistency with other pure APIs in the
-- @base@ package.
--
-- In many cases, short definitions of the combinators are provided in the
-- documentation for illustration. The actual implementation may differ for
-- performance reasons.
--
-- Functions having a 'MonadAsync' constraint work concurrently when used with
-- appropriate stream type combinator. Please be careful to not use 'parallely'
-- with infinite streams.
--
-- Deconstruction and folds accept a 'SerialT' type instead of a polymorphic
-- type to ensure that streams always have a concrete monomorphic type by
-- default, reducing type errors. In case you want to use any other type of
-- stream you can use one of the type combinators provided to convert the
-- stream type.

module Streamly.Prelude
    (
    -- * Stream Types
    -- $streamtypes

    -- ** Serial Streams
    -- $serial
      SerialT
    , WSerialT

    -- ** Speculative Streams
    -- $ahead
    , AheadT

    -- ** Asynchronous Streams
    -- $async
    , AsyncT
    , WAsyncT
    , ParallelT
    , mkAsync

    -- ** Zipping Streams
    -- $zipping
    , ZipSerialM
    , ZipAsyncM

    -- * IO Streams
    , Serial
    , WSerial
    , Ahead
    , Async
    , WAsync
    , Parallel
    , ZipSerial
    , ZipAsync

    -- * Type Synonyms
    , MonadAsync

    -- * Stream Type Adapters
    -- $adapters
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

    -- * Construction
    -- ** Primitives
    -- | Primitives to construct a stream from pure values or monadic actions.
    -- All other stream construction and generation combinators described later
    -- can be expressed in terms of these primitives. However, the special
    -- versions provided in this module can be much more efficient in most
    -- cases. Users can create custom combinators using these primitives.

    , nil
    , cons
    , (.:)

    , consM
    , (|:)

    -- ** From Values
    -- | Generate a monadic stream from a seed value or values.
    , value
    , yieldM
    , repeat
    , repeatM
    , replicate
    , replicateM

    -- Note: Using enumeration functions e.g. 'Prelude.enumFromThen' turns out
    -- to be slightly faster than the idioms like @[from, then..]@.
    --
    -- ** Enumeration
    -- | We can use the 'Enum' type class to enumerate a type producing a list
    -- and then convert it to a stream:
    --
    -- @
    -- 'fromList' $ 'Prelude.enumFromThen' from then
    -- @
    --
    -- However, this is not particularly efficient.
    -- The 'Enumerable' type class provides corresponding functions that
    -- generate a stream instead of a list, efficiently.

    , Enumerable (..)
    , enumerate
    , enumerateTo

    -- ** From Generators
    -- | Generate a monadic stream from a seed value and a generator function.
    , unfoldr
    , unfoldrM
    , unfold
    , iterate
    , iterateM
    , fromIndices
    , fromIndicesM

    -- ** From Containers
    -- | Convert an input structure, container or source into a stream. All of
    -- these can be expressed in terms of primitives.
    , fromList
    , fromListM
    , fromFoldable
    , fromFoldableM

    -- * Elimination

    -- ** Deconstruction
    -- | It is easy to express all the folds in terms of the 'uncons' primitive,
    -- however the specific implementations provided later are generally more
    -- efficient.
    --
    , uncons
    , tail
    , init

    -- ** Folding
-- | In imperative terms a fold can be considered as a loop over the stream
-- that reduces the stream to a single value.
-- Left and right folds both use a fold function @f@ and an identity element
-- @z@ (@zero@) to deconstruct a recursive data structure and reconstruct a
-- new data structure. The new structure may be a recursive construction (a
-- container) or a non-recursive single value reduction of the original
-- structure.
--
-- Both right and left folds are mathematical duals of each other, they are
-- functionally equivalent.  Operationally, a left fold on a left associated
-- structure behaves exactly in the same way as a right fold on a right
-- associated structure. Similarly, a left fold on a right associated structure
-- behaves in the same way as a right fold on a left associated structure.
-- However, the behavior of a right fold on a right associated structure is
-- operationally different (even though functionally equivalent) than a left
-- fold on the same structure.
--
-- On right associated structures like Haskell @cons@ lists or Streamly
-- streams, a lazy right fold is naturally suitable for lazy recursive
-- reconstruction of a new structure, while a strict left fold is naturally
-- suitable for efficient reduction. In right folds control is in the hand of
-- the @puller@ whereas in left folds the control is in the hand of the
-- @pusher@.
--
-- The behavior of right and left folds are described in detail in the
-- individual fold's documentation.  To illustrate the two folds for right
-- associated @cons@ lists:
--
-- > foldr :: (a -> b -> b) -> b -> [a] -> b
-- > foldr f z [] = z
-- > foldr f z (x:xs) = x `f` foldr f z xs
-- >
-- > foldl :: (b -> a -> b) -> b -> [a] -> b
-- > foldl f z [] = z
-- > foldl f z (x:xs) = foldl f (z `f` x) xs
--
-- @foldr@ is conceptually equivalent to:
--
-- > foldr f z [] = z
-- > foldr f z [x] = f x z
-- > foldr f z xs = foldr f (foldr f z (tail xs)) [head xs]
--
-- @foldl@ is conceptually equivalent to:
--
-- > foldl f z [] = z
-- > foldl f z [x] = f z x
-- > foldl f z xs = foldl f (foldl f z (init xs)) [last xs]
--
-- Left and right folds are duals of each other.
--
-- @
-- foldr f z xs = foldl (flip f) z (reverse xs)
-- foldl f z xs = foldr (flip f) z (reverse xs)
-- @
--
-- More generally:
--
-- @
-- foldr f z xs = foldl g id xs z where g k x = k . f x
-- foldl f z xs = foldr g id xs z where g x k = k . flip f x
-- @
--

-- As a general rule, foldr cannot have state and foldl cannot have control.

-- NOTE: Folds are inherently serial as each step needs to use the result of
-- the previous step. However, it is possible to fold parts of the stream in
-- parallel and then combine the results using a monoid.

    -- ** Right Folds
    -- $rightfolds
    , foldrM
    , foldr

    -- ** Left Folds
    -- $leftfolds
    , foldl'
    , foldl1'
    , foldlM'

    -- ** Composable Left Folds
    -- $runningfolds

    , fold

    -- ** Full Folds
    -- | Folds that are guaranteed to evaluate the whole stream.

    -- -- ** To Summary (Full Folds)
    -- -- | Folds that summarize the stream to a single value.
    , drain
    , last
    , length
    , sum
    , product

    -- -- ** To Summary (Maybe) (Full Folds)
    -- -- | Folds that summarize a non-empty stream to a 'Just' value and return
    -- 'Nothing' for an empty stream.
    , maximumBy
    , maximum
    , minimumBy
    , minimum
    , the

    -- ** Lazy Folds
    --
    -- | Folds that generate a lazy structure. Note that the generated
    -- structure may not be lazy if the underlying monad is strict.

    -- -- ** To Containers (Full Folds)
    -- -- | Convert or divert a stream into an output structure, container or
    -- sink.
    , toList

    -- ** Partial Folds
    -- | Folds that may terminate before evaluating the whole stream. These
    -- folds strictly evaluate the stream until the result is determined.

    -- -- ** To Elements (Partial Folds)
    , drainN
    , drainWhile

    -- -- | Folds that extract selected elements of a stream or their properties.
    , (!!)
    , head
    , findM
    , find
    , lookup
    , findIndex
    , elemIndex

    -- -- ** To Boolean (Partial Folds)
    -- -- | Folds that summarize the stream to a boolean value.
    , null
    , elem
    , notElem
    , all
    , any
    , and
    , or

    -- ** Multi-Stream folds
    , eqBy
    , cmpBy
    , isPrefixOf
    , isSubsequenceOf

    -- trimming sequences
    , stripPrefix

    -- * Parallel Function Application
    -- $application
    , (|$)
    , (|&)
    , (|$.)
    , (|&.)

    -- * Transformation

    -- ** Mapping
    -- | In imperative terms a map operation can be considered as a loop over
    -- the stream that transforms the stream into another stream by performing
    -- an operation on each element of the stream.
    --
    -- 'map' is the least powerful transformation operation with strictest
    -- guarantees.  A map, (1) is a stateless loop which means that no state is
    -- allowed to be carried from one iteration to another, therefore,
    -- operations on different elements are guaranteed to not affect each
    -- other, (2) is a strictly one-to-one transformation of stream elements
    -- which means it guarantees that no elements can be added or removed from
    -- the stream, it can merely transform them.
    , map
    , sequence
    , mapM

    -- ** Special Maps
    , mapM_
    , trace
    , tap

    -- ** Scanning
    --
    -- | A scan is more powerful than map. While a 'map' is a stateless loop, a
    -- @scan@ is a stateful loop which means that a state can be shared across
    -- all the loop iterations, therefore, future iterations can be impacted by
    -- the state changes made by the past iterations. A scan yields the state
    -- of the loop after each iteration. Like a map, a @postscan@ or @prescan@
    -- does not add or remove elements in the stream, it just transforms them.
    -- However, a @scan@ adds one extra element to the stream.
    --
    -- A left associative scan, also known as a prefix sum, can be thought of
    -- as a stream transformation consisting of left folds of all prefixes of a
    -- stream.  Another way of thinking about it is that it streams all the
    -- intermediate values of the accumulator while applying a left fold on the
    -- input stream.  A right associative scan, on the other hand, can be
    -- thought of as a stream consisting of right folds of all the suffixes of
    -- a stream.
    --
    -- The following equations hold for lists:
    --
    -- > scanl f z xs == map (foldl f z) $ inits xs
    -- > scanr f z xs == map (foldr f z) $ tails xs
    --
    -- @
    -- > scanl (+) 0 [1,2,3,4]
    -- 0                 = 0
    -- 0 + 1             = 1
    -- 0 + 1 + 2         = 3
    -- 0 + 1 + 2 + 3     = 6
    -- 0 + 1 + 2 + 3 + 4 = 10
    --
    -- > scanr (+) 0 [1,2,3,4]
    -- 1 + 2 + 3 + 4 + 0 = 10
    --     2 + 3 + 4 + 0 = 9
    --         3 + 4 + 0 = 7
    --             4 + 0 = 4
    --                 0 = 0
    -- @
    --
    -- Left and right scans are duals:
    --
    -- > scanr f z xs ==  reverse $ scanl (flip f) z (reverse xs)
    -- > scanl f z xs ==  reverse $ scanr (flip f) z (reverse xs)
    --
    -- A scan is a stateful map i.e. a combination of map and fold:
    --
    -- > map f xs =           tail $ scanl (\_ x -> f x) z xs
    -- > map f xs = reverse $ head $ scanr (\_ x -> f x) z xs
    --
    -- > foldl f z xs = last $ scanl f z xs
    -- > foldr f z xs = head $ scanr f z xs

    -- ** Left scans
    , scanl'
    , scanlM'
    , postscanl'
    , postscanlM'
    , scanl1'
    , scanl1M'

    -- ** Scan Using Fold
    , scan
    , postscan

    -- ** Filtering
    -- | Remove some elements from the stream based on a predicate. In
    -- imperative terms a filter over a stream corresponds to a loop with a
    -- @continue@ clause for the cases when the predicate fails.

    , filter
    , filterM

    -- ** Mapping Filters
    -- | Mapping along with filtering

    , mapMaybe
    , mapMaybeM

    -- ** Deleting Elements
    -- | Deleting elements is a special case of de-interleaving streams.
    , deleteBy
    , uniq

    -- ** Inserting Elements
    -- | Inserting elements is a special case of interleaving/merging streams.

    , insertBy
    , intersperseM
    , intersperse

    -- ** Indexing
    -- | Indexing can be considered as a special type of zipping where we zip a
    -- stream with an index stream.
    , indexed
    , indexedR

    -- ** Reordering Elements
    , reverse

    -- ** Trimming
    -- | Take or remove elements from one or both ends of a stream.
    , take
    , takeWhile
    , takeWhileM
    , drop
    , dropWhile
    , dropWhileM

    -- -- ** Breaking

    , chunksOf
    , intervalsOf

    -- ** Searching
    -- | Finding the presence or location of an element, a sequence of elements
    -- or another stream within a stream.

    -- -- ** Searching Elements
    , findIndices
    , elemIndices

    -- ** Splitting
    -- | In general we can express splitting in terms of parser combinators.
    -- These are some common use functions for convenience and efficiency.
    -- While parsers can fail these functions are designed to transform a
    -- stream without failure with a predefined behavior for all cases.
    --
    -- In general, there are three possible ways of combining stream segments
    -- with a separator. The separator could be prefixed to each segment,
    -- suffixed to each segment, or it could be infixed between segments.
    -- 'intersperse' and 'intercalate' operations are examples of infixed
    -- combining whereas 'unlines' is an example of suffixed combining. When we
    -- split a stream with separators we can split in three different ways,
    -- each being an opposite of the three ways of combining.
    --
    -- Splitting may keep the separator or drop it. Depending on how we split,
    -- the separator may be kept attached to the stream segments in prefix or
    -- suffix position or as a separate element in infix position. Combinators
    -- like 'splitOn' that use @On@ in their names drop the separator and
    -- combinators that use 'With' in their names keep the separator. When a
    -- segment is missing it is considered as empty, therefore, we never
    -- encounter an error in parsing.

    -- -- ** Splitting By Elements
    , splitOn
    , splitOnSuffix

    , splitWithSuffix
    , wordsBy -- strip, compact and split

    -- ** Grouping
    -- | Splitting a stream by combining multiple contiguous elements into
    -- groups using some criterion.
    , groups
    , groupsBy
    , groupsByRolling

    -- * Combining Streams
    -- | New streams can be constructed by appending, merging or zipping
    -- existing streams.

    -- ** Appending
    -- | Streams form a 'Semigroup' and a 'Monoid' under the append
    -- operation. Appending can be considered as a generalization of the `cons`
    -- operation to consing a stream to a stream.
    --
    -- @
    --
    -- -------Stream m a------|-------Stream m a------|=>----Stream m a---
    --
    -- @
    --
    -- @
    -- >> S.toList $ S.fromList [1,2] \<> S.fromList [3,4]
    -- [1,2,3,4]
    -- >> S.toList $ fold $ [S.fromList [1,2], S.fromList [3,4]]
    -- [1,2,3,4]
    -- @
    , serial

    -- ** Interleaving
    , wSerial

    -- ** Scheduling
    , ahead
    , async
    , wAsync
    , parallel

    -- ** Merging
    -- | Streams form a commutative semigroup under the merge
    -- operation. Merging can be considered as a generalization of inserting an
    -- element in a stream to interleaving a stream with another stream.
    --
    -- @
    --
    -- -------Stream m a------|
    --                        |=>----Stream m a---
    -- -------Stream m a------|
    -- @
    --

    -- , merge
    , mergeBy
    , mergeByM
    , mergeAsyncBy
    , mergeAsyncByM

    -- ** Zipping
    -- |
    -- @
    --
    -- -------Stream m a------|
    --                        |=>----Stream m c---
    -- -------Stream m b------|
    -- @
    --
    , zipWith
    , zipWithM
    , zipAsyncWith
    , zipAsyncWithM

    -- ** Folding Streams of Streams
    -- | Stream operations like map and filter represent loop processing in
    -- imperative programming terms. Similarly, the imperative concept of
    -- nested loops are represented by streams of streams. The 'concatMap'
    -- operation represents nested looping.
    -- A 'concatMap' operation loops over the input stream and then for each
    -- element of the input stream generates another stream and then loops over
    -- that inner stream as well producing effects and generating a single
    -- output stream.
    -- The 'Monad' instances of different stream types provide a more
    -- convenient way of writing nested loops. Note that the monad bind
    -- operation is just @flip concatMap@.
    --
    -- One dimension loops are just a special case of nested loops.  For
    -- example, 'concatMap' can degenerate to a simple map operation:
    --
    -- > map f m = S.concatMap (\x -> S.value (f x)) m
    --
    -- Similarly, 'concatMap' can perform filtering by mapping an element to a
    -- 'nil' stream:
    --
    -- > filter p m = S.concatMap (\x -> if p x then S.value x else S.nil) m
    --

    , concatMapWith
    , concatMap
    , concatMapM
    , concatUnfold

    -- ** Folding Containers of Streams
    -- | These are variants of standard 'Foldable' fold functions that use a
    -- polymorphic stream sum operation (e.g. 'async' or 'wSerial') to fold a
    -- finite container of streams. Note that these are just special cases of
    -- the more general 'concatMapWith' operation.
    --
    , concatFoldableWith
    , concatMapFoldableWith
    , concatForFoldableWith

    -- * Exceptions
    -- | Most of these combinators inhibit stream fusion, therefore, when
    -- possible, they should be called in an outer loop to mitigate the cost.
    -- For example, instead of calling them on a stream of chars call them on a
    -- stream of arrays before flattening it to a stream of chars.
    --
    , before
    , after
    , bracket
    , onException
    , finally
    , handle

    -- * Concurrency Control
    -- $concurrency
    , maxThreads
    , maxBuffer

    -- * Rate Limiting
    , Rate (..)
    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate

    -- * Deprecated
    , once
    , yield
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

import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, repeat, replicate, concatMap, span)

import Streamly.Internal.Data.Stream.IsStream

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Data.Fold as Fold

-- $streamtypes
-- The basic stream type is 'Serial', it represents a sequence of IO actions,
-- and is a 'Monad'.  The type 'SerialT' is a monad transformer that can
-- represent a sequence of actions in an arbitrary monad. The type 'Serial' is
-- in fact a synonym for @SerialT IO@.  There are a few more types similar to
-- 'SerialT', all of them represent a stream and differ only in the
-- 'Semigroup', 'Applicative' and 'Monad' compositions of the stream. 'Serial'
-- and 'WSerial' types compose serially whereas 'Async' and 'WAsync'
-- types compose concurrently. All these types can be freely inter-converted
-- using type combinators without any cost. You can freely switch to any type
-- of composition at any point in the program.  When no type annotation or
-- explicit stream type combinators are used, the default stream type is
-- inferred as 'Serial'.

-- $serial
--
-- When a stream consumer demands an element from a serial stream constructed
-- as @a \`consM` b \`consM` ... nil@, the action @a@ at the head of the stream
-- sequence is executed and the result is supplied to the consumer. When the
-- next element is demanded, the action @b@ is executed and its result is
-- supplied.  Thus, the effects are performed and results are consumed strictly
-- in a serial order.  Serial streams can be considered as /spatially ordered/
-- streams as the order of execution and consumption is the same as the spatial
-- order in which the actions are composed by the programmer.
--
-- Serial streams enforce the side effects as well as the results of the
-- actions to be in the same order in which the actions are added to the
-- stream.  Therefore, the semigroup operation for serial streams is not
-- commutative:
--
-- @
-- a <> b is not the same as b <> a
-- @
--
-- There are two serial stream types 'SerialT' and 'WSerialT'. The stream
-- evaluation of both the variants works in the same way as described above,
-- they differ only in the 'Semigroup' and 'Monad' implementaitons.

-- $ahead
--
-- When a stream consumer demands an element from a speculative stream
-- constructed as @a \`consM` b \`consM` ... nil@, the action @a@ at the head
-- of the stream is executed and the output of the action is supplied to the
-- consumer. However, in addition to the action at the head multiple actions
-- following it may also be executed concurrently and the results buffered.
-- When the next element is demanded it may be served from the buffer and we
-- may execute the next action in the sequence to keep the buffer adequately
-- filled.  Thus, the actions are executed concurrently but results consumed in
-- serial order just like serial streams.  `consM` can be used to fold an
-- infinite lazy container of effects, as the number of concurrent executions
-- is limited.
--
-- Similar to 'consM', the monadic stream generation (e.g. replicateM) and
-- transformation operations (e.g. mapM) on speculative streams can execute
-- multiple effects concurrently in a speculative manner.
--
-- How many effects can be executed concurrently and how many results can be
-- buffered are controlled by 'maxThreads' and 'maxBuffer' combinators
-- respectively.  The actual number of concurrent threads is adjusted according
-- to the rate at which the consumer is consuming the stream. It may even
-- execute actions serially in a single thread if that is enough to match the
-- consumer's speed.
--
-- Speculative streams enforce ordering of the results of actions in the stream
-- but the side effects are only partially ordered.  Therefore, the semigroup
-- operation for speculative streams is not commutative from the pure outputs
-- perspective but commutative from side effects perspective.

-- $async
--
-- /Scheduling and execution:/ In an asynchronous stream @a \`consM` b \`consM`
-- c ...@, the actions @a@, @b@, and @c@ are executed concurrently with the
-- consumer of the stream.  The actions are /scheduled/ for execution in the
-- same order as they are specified in the stream. Multiple scheduled actions
-- may be /executed/ concurrently in parallel threads of execution.  The
-- actions may be executed out of order and they may complete at arbitrary
-- times.  Therefore, the /effects/ of the actions may be observed out of
-- order.
--
-- /Buffering:/ The /results/ from multiple threads of execution are queued in
-- a buffer as soon as they become available. The consumer of the stream is
-- served from this buffer.  Therefore, the consumer may observe the results to
-- be out of order.  In other words, an asynchronous stream is an unordered
-- stream i.e.  order does not matter.
--
-- /Concurrency control:/ Threads are suspended if the `maxBuffer` limit is
-- reached, and resumed when the consumer makes space in the buffer.  The
-- maximum number of concurrent threads depends on `maxThreads`. Number of
-- threads is increased or decreased based on the speed of the consumer.
--
-- /Generation operations:/ Concurrent stream generation operations e.g.
-- 'Streamly.Prelude.replicateM' when used in async style schedule and execute
-- the stream generating actions in the manner described above. The generation
-- actions run concurrently, effects and results of the actions as observed by
-- the consumer of the stream may be out of order.
--
-- /Transformation operations:/ Concurrent stream transformation operations
-- e.g.  'Streamly.Prelude.mapM', when used in async style, schedule and
-- execute transformation actions in the manner described above. Transformation
-- actions run concurrently, effects and results of the actions may be
-- observed by the consumer out of order.
--
-- /Variants:/ There are two asynchronous stream types 'AsyncT' and 'WAsyncT'.
-- They are identical with respect to single stream evaluation behavior.  Their
-- behaviors differ in how they combine multiple streams using 'Semigroup' or
-- 'Monad' composition. Since the order of elements does not matter in
-- asynchronous streams the 'Semigroup' operation is effectively commutative.

-- $zipping
--
-- 'ZipSerialM' and 'ZipAsyncM', provide 'Applicative' instances for zipping the
-- corresponding elements of two streams together. Note that these types are
-- not monads.

-- $rightfolds
--
-- Let's take a closer look at the @foldr@ definition for lists, as given3
-- earlier:
--
-- @
-- foldr f z (x:xs) = x \`f` foldr f z xs
-- @
--
-- @foldr@ invokes the fold step function @f@ as @f x (foldr f z xs)@. At each
-- invocation of @f@, @foldr@ gives us the next element in the input container
-- @x@ and a recursive expression @foldr f z xs@ representing the yet unbuilt
-- (lazy thunk) part of the output.
--
-- When @f x xs@ is lazy in @xs@ it can consume the input one element at a time
-- in FIFO order to build a lazy output expression. For example,
--
-- > f x remaining = show x : remaining
--
-- @take 2 $ foldr f [] (1:2:undefined)@ would consume the input lazily on
-- demand, consuming only first two elements and resulting in ["1", "2"]. @f@
-- can terminate recursion by not evaluating the @remaining@ part:
--
-- > f 2 remaining = show 2 : []
-- > f x remaining = show x : remaining
--
-- @f@ would terminate recursion whenever it sees element @2@ in the input.
-- Therefore, @take 2 $ foldr f [] (1:2:undefined)@ would work without any
-- problem.
--
-- On the other hand, if @f a b@ is strict in @b@ it would end up consuming the
-- whole input right away and expanding the recursive expression @b@ (i.e.
-- @foldr f z xs@) fully before it yields an output expression, resulting in
-- the following /right associated expression/:
--
-- @
-- foldr f z xs == x1 \`f` (x2 \`f` ...(xn \`f` z))
-- @
--
-- For example,
--
-- > f x remaining = x + remaining
--
-- With this definition, @foldr f 0 [1..1000]@, would recurse completely until
-- it reaches the terminating case @... `f` (1000 `f` 0)@, and then
-- start reducing the whole expression from right to left, therefore, consuming
-- the input elements in LIFO order. Thus, such an evaluation would require
-- memory proportional to the size of input. Try out @foldr (+) 0 (map (\\x ->
-- trace (show x) x) [1..10])@.
--
-- Notice the order of the arguments to the step function @f a b@. It follows
-- the order of @a@ and @b@ in the right associative recursive expression
-- generated by expanding @a \`f` b@.
--
-- A right fold is a pull fold, the step function is the puller, it can pull
-- more data from the input container by using its second argument in the
-- output expression or terminate pulling by not using it. As a corollary:
--
-- 1. a step function which is lazy in its second argument (usually functions
-- or constructors that build a lazy structure e.g. @(:)@) can pull lazily on
-- demand.
-- 2. a step function strict in its second argument (usually reducers e.g.
-- (+)) would end up pulling all of its input and buffer it in memory before
-- potentially reducing it.
--
-- A right fold is suitable for lazy reconstructions e.g.  transformation,
-- mapping, filtering of /right associated input structures/ (e.g. cons lists).
-- Whereas a left fold is suitable for reductions (e.g. summing a stream of
-- numbers) of right associated structures. Note that these roles will reverse
-- for left associated structures (e.g. snoc lists). Most of our observations
-- here assume right associated structures, lists being the canonical example.
--
-- 1. A lazy FIFO style pull using a right fold allows pulling a potentially
-- /infinite/ input stream lazily, perform transformations on it, and
-- reconstruct a new structure without having to buffer the whole structure. In
-- contrast, a left fold would buffer the entire structure before the
-- reconstructed structure can be consumed.
-- 2. Even if buffering the entire input structure is ok, we need to keep in
-- mind that a right fold reconstructs structures in a FIFO style, whereas a
-- left fold reconstructs in a LIFO style, thereby reversing the order of
-- elements..
-- 3. A right fold has termination control and therefore can terminate early
-- without going through the entire input, a left fold cannot terminate
-- without consuming all of its input.  For example, a right fold
-- implementation of 'or' can terminate as soon as it finds the first 'True'
-- element, whereas a left fold would necessarily go through the entire input
-- irrespective of that.
-- 4. Reduction (e.g. using (+) on a stream of numbers) using a right fold
-- occurs in a LIFO style, which means that the entire input gets buffered
-- before reduction starts. Whereas with a strict left fold reductions occur
-- incrementally in FIFO style. Therefore, a strict left fold is more suitable
-- for reductions.
--

-- $leftfolds
--
-- Note that the observations below about the behavior of a left fold assume
-- that we are working on a right associated structure like cons lists and
-- streamly streams. If we are working on a left associated structure (e.g.
-- snoc lists) the roles of right and left folds would reverse.
--
-- Let's take a closer look at the @foldl@ definition for lists given above:
--
-- @
-- foldl f z (x:xs) = foldl f (z \`f` x) xs
-- @
--
-- @foldl@ calls itself recursively, in each call it invokes @f@ as @f z x@
-- providing it with the result accumulated till now @z@ (the state) and the
-- next element from the input container. First call to @f@ is supplied with
-- the initial value of the accumulator @z@ and each subsequent call uses the
-- output of the previous call to @f z x@.
--
-- >> foldl' (+) 0 [1,2,3]
-- > 6
--
-- The recursive call at the head of the output expression is bound to be
-- evaluated until recursion terminates, therefore, a left fold always
-- /consumes the whole input container/. The following would result in an
-- error, even though the fold is not using the values at all:
--
-- >> foldl' (\_ _ -> 0) 0 (1:undefined)
-- > *** Exception: Prelude.undefined
--
-- As @foldl@ recurses, it builds the left associated expression shown below.
-- Notice, the order of the arguments to the step function @f b a@. It follows
-- the left associative recursive expression generated by expanding @b \`f` a@.
--
-- @
-- foldl f z xs == (((z \`f` x1) \`f` x2) ...) \`f` xn
-- @
--
--
-- The strict left fold @foldl'@ forces the reduction of its argument @z \`f`
-- x@ before using it, therefore it never builds the whole expression in
-- memory.  Thus, @z \`f` x1@ would get reduced to @z1@ and then @z1 \`f` x2@
-- would get reduced to @z2@ and so on, incrementally reducing the expression
-- from left to right as it recurses, consuming the input in FIFO order.  Try
-- out @foldl' (+) 0 (map (\\x -> trace (show x) x) [1..10])@ to see how it
-- works. For example:
--
-- @
-- > S.foldl' (+) 0 $ S.fromList [1,2,3,4]
-- 10
-- @
--
-- @
-- 0 + 1 = 1
-- 1 + 2 = 3
-- 3 + 3 = 6
-- 6 + 4 = 10
-- @
--
-- However, @foldl'@ evaluates the accumulator only to WHNF. It may further
-- help if the step function uses a strict data structure as accumulator to
-- improve performance and to keep the expression fully reduced at all times
-- during the fold.
--
-- A left fold can also build a new structure instead of reducing one if a
-- constructor is used as a fold step. However, it may not be very useful
-- because it will consume the whole input and construct the new structure in
-- memory before we can consume it. Thus the whole structure gets buffered in
-- memory. When the list constructor is used it would build a new list in
-- reverse (LIFO) order:
--
-- @
-- > S.foldl' (flip (:)) [] $ S.fromList [1,2,3,4]
-- [4,3,2,1]
-- @
--
-- A left fold is a push fold. The producer pushes its contents to the step
-- function of the fold. The step function therefore has no control to stop the
-- input, it can only discard it if it does not need it. We can also consider a
-- left fold as a state machine where the state is store in the accumulator,
-- the state can be modified based on new inputs that are pushed to the fold.
--
-- In general, a strict left fold is a reducing fold, whereas a right fold is a
-- constructing fold. A strict left fold reduces in a FIFO order whereas it
-- constructs in a LIFO order, and vice-versa for the right fold. See the
-- documentation of 'foldrM' for a discussion on where a left or right fold is
-- suitable.
--
-- To perform a left fold lazily without having to consume all the input one
-- can use @scanl@ to stream the intermediate results of the fold and consume
-- the resulting stream lazily.
--

-- $runningfolds
--
-- "Streamly.Data.Fold" module defines composable left folds which can be combined
-- together in many interesting ways. Those folds can be run using 'fold'.
-- The following two ways of folding are equivalent in functionality and
-- performance,
--
-- >>> Stream.fold Fold.sum (Stream.enumerateFromTo 1 100)
-- 5050
-- >>> Stream.sum (Stream.enumerateFromTo 1 100)
-- 5050
--
-- However, left folds cannot terminate early even if it does not need to
-- consume more input to determine the result.  Therefore, the performance is
-- equivalent only for full folds like 'sum' and 'length'. For partial folds
-- like 'head' or 'any' the the folds defined in this module may be much more
-- efficient because they are implemented as right folds that terminate as soon
-- as we get the result. Note that when a full fold is composed with a partial
-- fold in parallel the performance is not impacted as we anyway have to
-- consume the whole stream due to the full fold.
--
-- >>> Stream.head (1 `Stream.cons` undefined)
-- Just 1
-- >>> Stream.fold Fold.head (1 `Stream.cons` undefined)
-- Just 1
--
-- However, we can wrap the fold in a scan to convert it into a lazy stream of
-- fold steps. We can then terminate the stream whenever we want.  For example,
--
-- >>> Stream.toList $ Stream.take 1 $ Stream.scan Fold.head (1 `Stream.cons` undefined)
-- [Nothing]
--
-- The following example extracts the input stream up to a point where the
-- running average of elements is no more than 10:
--
-- >>> import Data.Maybe (fromJust)
-- >>> let avg = Fold.teeWith (/) Fold.sum (fmap fromIntegral Fold.length)
-- >>> :{
--  Stream.toList
--   $ Stream.map (fromJust . fst)
--   $ Stream.takeWhile (\(_,x) -> x <= 10)
--   $ Stream.postscan (Fold.tee Fold.last avg) (Stream.enumerateFromTo 1.0 100.0)
-- :}
-- [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0]
--
-- $application
--
-- Stream processing functions can be composed in a chain using function
-- application with or without the '$' operator, or with reverse function
-- application operator '&'. Streamly provides concurrent versions of these
-- operators applying stream processing functions such that each stage of the
-- stream can run in parallel. The operators start with a @|@; we can read '|$'
-- as "@parallel dollar@" to remember that @|@ comes before '$'.
--
-- Imports for the code snippets below:
--
-- @
--  import qualified Streamly.Prelude as S
--  import Control.Concurrent
-- @

-- $concurrency
--
-- These combinators can be used at any point in a stream composition to set
-- parameters to control the concurrency of the /argument stream/.  A control
-- parameter set at any point remains effective for any concurrent combinators
-- used in the argument stream until it is reset by using the combinator again.
-- These control parameters have no effect on non-concurrent combinators in the
-- stream, or on non-concurrent streams.
--
-- /Pitfall:/ Remember that 'maxBuffer' in the following example applies to
-- 'mapM' and any other combinators that may follow it, and it does not apply
-- to the combinators before it:
--
-- @
--  ...
--  $ S.maxBuffer 10
--  $ S.mapM ...
--  ...
-- @
--
-- If we use '&' instead of '$' the situation will reverse, in the following
-- example, 'maxBuffer' does not apply to 'mapM', it applies to combinators
-- that come before it, because those are the arguments to 'maxBuffer':
--
-- @
--  ...
--  & S.maxBuffer 10
--  & S.mapM ...
--  ...
-- @

-- $adapters
--
-- You may want to use different stream composition styles at different points
-- in your program. Stream types can be freely converted or adapted from one
-- type to another.  The 'IsStream' type class facilitates type conversion of
-- one stream type to another. It is not used directly, instead the type
-- combinators provided below are used for conversions.
--
-- To adapt from one monomorphic type (e.g. 'AsyncT') to another monomorphic
-- type (e.g. 'SerialT') use the 'adapt' combinator. To give a polymorphic code
-- a specific interpretation or to adapt a specific type to a polymorphic type
-- use the type specific combinators e.g. 'asyncly' or 'wSerially'. You
-- cannot adapt polymorphic code to polymorphic code, as the compiler would not know
-- which specific type you are converting from or to. If you see a an
-- @ambiguous type variable@ error then most likely you are using 'adapt'
-- unnecessarily on polymorphic code.
