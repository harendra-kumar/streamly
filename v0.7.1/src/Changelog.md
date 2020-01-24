## 0.7.1

### Bug Fixes

* Fix a bug that caused `findIndices` to return wrong indices in some
  cases.
* Fix a bug in `tap`, `chunksOf` that caused memory consumption to
  increase in some cases.
* Fix a space leak in concurrent streams (`async`, `wAsync`, and `ahead`) that
  caused memory consumption to increase with the number of elements in the
  stream, especially when built with `-threaded` and used with `-N` RTS option.
  The issue occurs only in cases when a worker thread happens to be used
  continuously for a long time.
* Fix scheduling of WAsyncT stream style to be in round-robin fashion.
* Now builds with `containers` package version < 0.5.8.
* Now builds with `network` package version >= 3.0.0.0 && < 3.1.0.0.

### Behavior change

* Combinators in `Streamly.Network.Inet.TCP` no longer use TCP `NoDelay` and
  `ReuseAddr` socket options by default. These options can now be specified
  using appropriate combinators.

### Performance

* Now uses `fusion-plugin` package for predictable stream fusion optimizations
* Significant improvement in performance of concurrent stream operations.
* Improved space and time performance of `Foldable` instance.

## 0.7.0

### Breaking changes

* Change the signature of `foldrM` to ensure that it is lazy
* Change the signature of `iterateM` to ensure that it is lazy.
* `scanx` would now require an additional `Monad m` constraint.

### Behavior change

* Earlier `ParallelT` was unaffected by `maxBuffer` directive, now `maxBuffer`
  can limit the buffer of a `ParallelT` stream as well. When the buffer becomes
  full, the producer threads block.
* `ParallelT` streams no longer have an unlimited buffer by default. Now the
  buffer for parallel streams is limited to 1500 by default, the same as other
  concurrent stream types.

### Deprecations

* In `Streamly.Prelude`:
    * `runStream` has been replaced by `drain`
    * `runN` has been replaced by `drainN`
    * `runWhile` has been replaced by `drainWhile`
    * `fromHandle` has been deprecated. Please use
      `Streamly.FileSystem.Handle.read`, `Streamly.Data.Unicode.Stream.decodeUtf8` and
      `splitOnSuffix` with `Streamly.Data.Fold.toList` to split the
       stream to a stream of `String` separated by a newline.
    * `toHandle` has been deprecated. Please use `intersperse` and `concatUnfold` to
      add newlines to a stream, `Streamly.Data.Unicode.Stream.encodeUtf8` for encoding and
      `Streamly.FileSystem.Handle.write` for writing to a file handle.
    * Deprecate `scanx`, `foldx`, `foldxM`, `foldr1`
    * Remove deprecated APIs `foldl`, `foldlM`
    * Replace deprecated API `scan` with a new signature, to scan using Fold.

* In `Streamly` module:
    * `runStream` has been deprecated, please use `Streamly.Prelude.drain`

* Remove deprecated module `Streamly.Time` (moved to Streamly.Internal.Data.Time)
* Remove module `Streamly.Internal` (functionality moved to the Internal hierarchy)

### Bug Fixes

* Fix a bug that caused `uniq` function to yield the same element twice.
* Fix a bug that caused "thread blocked indefinitely in an MVar operation"
  exception in a parallel stream.
* Fix unbounded memory usage (leak) in `parallel` combinator. The bug manifests
  when large streams are combined using `parallel`.

### Major Enhancements

This release contains a lot of new features and major enhancements.  For more
details on the new features described below please see the haddock docs of the
modules on hackage.

#### Exception Handling

See `Streamly.Prelude` for new exception handling combinators like `before`,
`after`, `bracket`, `onException`, `finally`, `handle` etc.

#### Composable Folds

`Streamly.Data.Fold` module provides composable folds (stream consumers). Folds
allow splitting, grouping, partitioning, unzipping and nesting a stream onto
multiple folds without breaking the stream. Combinators are provided for
temporal and spatial window based fold operations, for example, to support
folding and aggregating data for timeout or inactivity based sessions.

#### Composable Unfolds

`Streamly.Data.Unfold` module provides composable stream generators. Unfolds allow
high performance merging/flattening/combining of stream generators.

#### Streaming File IO

`Streamly.FileSystem.Handle` provides handle based streaming file IO
operations.

#### Streaming Network IO

* `Streamly.Network.Socket` provides socket based streaming network IO
operations.

* `Streamly.Network.Inet.TCP` provides combinators to build Inet/TCP
clients and servers.

#### Concurrent concatMap

The new `concatMapWith` in `Streamly.Prelude` combinator performs a
`concatMap` using a supplied merge/concat strategy. This is a very
powerful combinator as you can, for example, concat streams
concurrently using this.

### Other Enhancements

* Add the following new features/modules:
  * _Unicode Strings_: `Streamly.Data.Unicode.Stream` module provides
    encoding/decoding of character streams and other character stream
    operations.
  * _Arrays_: `Streamly.Memory.Array` module provides arrays for efficient
    in-memory buffering and efficient interfacing with IO.

* Add the following to `Streamly.Prelude`:
    * `unfold`, `fold`, `scan` and `postscan`
    * `concatUnfold` to concat a stream after unfolding each element
    * `intervalsOf` and `chunksOf`
    * `splitOn`, `splitOnSuffix`, `splitWithSuffix`, and `wordsBy`
    * `groups`, `groupsBy` and `groupsByRolling`
    * `postscanl'` and `postscanlM'`
    * `intersperse` intersperse an element in between consecutive elements in
      stream
    * `trace` combinator maps a monadic function on a stream just for side
      effects
    * `tap` redirects a copy of the stream to a `Fold`

## 0.6.1

### Bug Fixes

* Fix a bug that caused `maxThreads` directive to be ignored when rate control
  was not used.

### Enhancements

* Add GHCJS support
* Remove dependency on "clock" package

## 0.6.0

### Breaking changes

* `Monad` constraint may be needed on some of the existing APIs (`findIndices`
  and `elemIndices`).

### Enhancements

* Add the following functions to Streamly.Prelude:
    * Generation: `replicate`, `fromIndices`, `fromIndicesM`
    * Enumeration: `Enumerable` type class, `enumerateFrom`, `enumerateFromTo`,
      `enumerateFromThen`, `enumerateFromThenTo`, `enumerate`, `enumerateTo`
    * Running: `runN`, `runWhile`
    * Folds: `(!!)`, `maximumBy`, `minimumBy`, `the`
    * Scans: `scanl1'`, `scanl1M'
    * Filters: `uniq`, `insertBy`, `deleteBy`, `findM`
    * Multi-stream: `eqBy`, `cmpBy`, `mergeBy`, `mergeByM`, `mergeAsyncBy`,
      `mergeAsyncByM`, `isPrefixOf`, `isSubsequenceOf`, `stripPrefix`,
      `concatMap`, `concatMapM`, `indexed`, `indexedR`
* Following instances were added for `SerialT m`, `WSerialT m` and
  `ZipSerialM m`:
  * When `m` ~ `Identity`: IsList, Eq, Ord, Show, Read, IsString, NFData,
    NFData1, Traversable
  * When `m` is `Foldable`: Foldable
* Performance improvements
* Add benchmarks to measure composed and iterated operations

## 0.5.2

### Bug Fixes

* Cleanup any pending threads when an exception occurs.
* Fixed a livelock in ahead style streams. The problem manifests sometimes when
  multiple streams are merged together in ahead style and one of them is a nil
  stream.
* As per expected concurrency semantics each forked concurrent task must run
  with the monadic state captured at the fork point.  This release fixes a bug,
  which, in some cases caused an incorrect monadic state to be used for a
  concurrent action, leading to unexpected behavior when concurrent streams are
  used in a stateful monad e.g. `StateT`. Particularly, this bug cannot affect
  `ReaderT`.

## 0.5.1

* Performance improvements, especially space consumption, for concurrent
  streams

## 0.5.0

### Bug Fixes

* Leftover threads are now cleaned up as soon as the consumer is garbage
  collected.
* Fix a bug in concurrent function application that in certain cases would
  unnecessarily share the concurrency state resulting in incorrect output
  stream.
* Fix passing of state across `parallel`, `async`, `wAsync`, `ahead`, `serial`,
  `wSerial` combinators. Without this fix combinators that rely on state
  passing e.g.  `maxThreads` and `maxBuffer` won't work across these
  combinators.

### Enhancements

* Added rate limiting combinators `rate`, `avgRate`, `minRate`, `maxRate` and
  `constRate` to control the yield rate of a stream.
* Add `foldl1'`, `foldr1`, `intersperseM`, `find`, `lookup`, `and`, `or`,
  `findIndices`, `findIndex`, `elemIndices`, `elemIndex`, `init` to Prelude

### Deprecations

* The `Streamly.Time` module is now deprecated, its functionality is subsumed
  by the new rate limiting combinators.

## 0.4.1

### Bug Fixes

* foldxM was not fully strict, fixed.

## 0.4.0

### Breaking changes

* Signatures of `zipWithM` and `zipAsyncWithM` have changed
* Some functions in prelude now require an additional `Monad` constraint on
  the underlying type of the stream.

### Deprecations

* `once` has been deprecated and renamed to `yieldM`

### Enhancements

* Add concurrency control primitives `maxThreads` and `maxBuffer`.
* Concurrency of a stream with bounded concurrency when used with `take` is now
  limited by the number of elements demanded by `take`.
* Significant performance improvements utilizing stream fusion optimizations.
* Add `yield` to construct a singleton stream from a pure value
* Add `repeat` to generate an infinite stream by repeating a pure value
* Add `fromList` and `fromListM` to generate streams from lists, faster than
  `fromFoldable` and `fromFoldableM`
* Add `map` as a synonym of fmap
* Add `scanlM'`, the monadic version of scanl'
* Add `takeWhileM` and `dropWhileM`
* Add `filterM`

## 0.3.0

### Breaking changes

* Some prelude functions, to whom concurrency capability has been added, will
  now require a `MonadAsync` constraint.

### Bug Fixes

* Fixed a race due to which, in a rare case, we might block indefinitely on
  an MVar due to a lost wakeup.
* Fixed an issue in adaptive concurrency. The issue caused us to stop creating
  more worker threads in some cases due to a race. This bug would not cause any
  functional issue but may reduce concurrency in some cases.

### Enhancements
* Added a concurrent lookahead stream type `Ahead`
* Added `fromFoldableM` API that creates a stream from a container of monadic
  actions
* Monadic stream generation functions `consM`, `|:`, `unfoldrM`, `replicateM`,
  `repeatM`, `iterateM` and `fromFoldableM` can now generate streams
  concurrently when used with concurrent stream types.
* Monad transformation functions `mapM` and `sequence` can now map actions
  concurrently when used at appropriate stream types.
* Added concurrent function application operators to run stages of a
  stream processing function application pipeline concurrently.
* Added `mapMaybe` and `mapMaybeM`.

## 0.2.1

### Bug Fixes
* Fixed a bug that caused some transformation ops to return incorrect results
  when used with concurrent streams. The affected ops are `take`, `filter`,
  `takeWhile`, `drop`, `dropWhile`, and `reverse`.

## 0.2.0

### Breaking changes
* Changed the semantics of the Semigroup instance for `InterleavedT`, `AsyncT`
  and `ParallelT`. The new semantics are as follows:
  * For `InterleavedT`, `<>` operation interleaves two streams
  * For `AsyncT`, `<>` now concurrently merges two streams in a left biased
    manner using demand based concurrency.
  * For `ParallelT`, the `<>` operation now concurrently meges the two streams
    in a fairly parallel manner.

  To adapt to the new changes, replace `<>` with `serial` wherever it is used
  for stream types other than `StreamT`.

* Remove the `Alternative` instance.  To adapt to this change replace any usage
  of `<|>` with `parallel` and `empty` with `nil`.
* Stream type now defaults to the `SerialT` type unless explicitly specified
  using a type combinator or a monomorphic type.  This change reduces puzzling
  type errors for beginners. It includes the following two changes:
  * Change the type of all stream elimination functions to use `SerialT`
    instead of a polymorphic type. This makes sure that the stream type is
    always fixed at all exits.
  * Change the type combinators (e.g. `parallely`) to only fix the argument
    stream type and the output stream type remains polymorphic.

  Stream types may have to be changed or type combinators may have to be added
  or removed to adapt to this change.
* Change the type of `foldrM` to make it consistent with `foldrM` in base.
* `async` is renamed to `mkAsync` and `async` is now a new API with a different
  meaning.
* `ZipAsync` is renamed to `ZipAsyncM` and `ZipAsync` is now ZipAsyncM
  specialized to the IO Monad.
* Remove the `MonadError` instance as it was not working correctly for
  parallel compositions. Use `MonadThrow` instead for error propagation.
* Remove Num/Fractional/Floating instances as they are not very useful. Use
  `fmap` and `liftA2` instead.

### Deprecations
* Deprecate and rename the following symbols:
    * `Streaming` to `IsStream`
    * `runStreaming` to `runStream`
    * `StreamT` to `SerialT`
    * `InterleavedT` to `WSerialT`
    * `ZipStream` to `ZipSerialM`
    * `ZipAsync` to `ZipAsyncM`
    * `interleaving` to `wSerially`
    * `zipping` to `zipSerially`
    * `zippingAsync` to `zipAsyncly`
    * `<=>` to `wSerial`
    * `<|` to `async`
    * `each` to `fromFoldable`
    * `scan` to `scanx`
    * `foldl` to `foldx`
    * `foldlM` to `foldxM`
* Deprecate the following symbols for future removal:
    * `runStreamT`
    * `runInterleavedT`
    * `runAsyncT`
    * `runParallelT`
    * `runZipStream`
    * `runZipAsync`

### Enhancements
* Add the following functions:
    * `consM` and `|:` operator to construct streams from monadic actions
    * `once` to create a singleton stream from a monadic action
    * `repeatM` to construct a stream by repeating a monadic action
    * `scanl'` strict left scan
    * `foldl'` strict left fold
    * `foldlM'` strict left fold with a monadic fold function
    * `serial` run two streams serially one after the other
    * `async` run two streams asynchronously
    * `parallel` run two streams in parallel (replaces `<|>`)
    * `WAsyncT` stream type for BFS version of `AsyncT` composition
* Add simpler stream types that are specialized to the IO monad
* Put a bound (1500) on the output buffer used for asynchronous tasks
* Put a limit (1500) on the number of threads used for Async and WAsync types

## 0.1.2

### Enhancements
* Add `iterate`, `iterateM` stream operations

### Bug Fixes
* Fixed a bug that caused unexpected behavior when `pure` was used to inject
  values in Applicative composition of `ZipStream` and `ZipAsync` types.

## 0.1.1

### Enhancements
* Make `cons` right associative and provide an operator form `.:` for it
* Add `null`, `tail`, `reverse`, `replicateM`, `scan` stream operations
* Improve performance of some stream operations (`foldl`, `dropWhile`)

### Bug Fixes
* Fix the `product` operation. Earlier, it always returned 0 due to a bug
* Fix the `last` operation, which returned `Nothing` for singleton streams

## 0.1.0

* Initial release
