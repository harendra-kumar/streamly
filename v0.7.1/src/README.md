# Streamly

[![Hackage](https://img.shields.io/hackage/v/streamly.svg?style=flat)](https://hackage.haskell.org/package/streamly)
[![Gitter chat](https://badges.gitter.im/composewell/gitter.svg)](https://gitter.im/composewell/streamly)
[![Travis](https://travis-ci.com/composewell/streamly.svg?branch=master)](https://travis-ci.com/composewell/streamly)
[![Appveyor](https://ci.appveyor.com/api/projects/status/ajxg0c79raou9ned?svg=true)](https://ci.appveyor.com/project/harendra-kumar/streamly)
[![CircleCI](https://circleci.com/gh/composewell/streamly/tree/master.svg?style=svg)](https://circleci.com/gh/composewell/streamly/tree/master)
[![Coverage Status](https://coveralls.io/repos/composewell/streamly/badge.svg?branch=master&service=github)](https://coveralls.io/github/composewell/streamly?branch=master)

## Learning Materials

* Documentation: [Quick](#streaming-concurrently) | [Tutorial](https://hackage.haskell.org/package/streamly/docs/Streamly-Tutorial.html) | [Reference (Hackage)](https://hackage.haskell.org/package/streamly) | [Reference (Latest)](https://composewell.github.io/streamly) | [Guides](docs)
* Installing: [Installing](./INSTALL.md) | [Building for optimal performance](docs/Build.md)
* Examples: [streamly](examples) | [streamly-examples](https://github.com/composewell/streamly-examples)
* Benchmarks: [Streaming](https://github.com/composewell/streaming-benchmarks) | [Concurrency](https://github.com/composewell/concurrency-benchmarks)
* Talks: [Functional Conf 2019 Video](https://www.youtube.com/watch?v=uzsqgdMMgtk) | [Functional Conf 2019 Slides](https://www.slideshare.net/HarendraKumar10/streamly-concurrent-data-flow-programming)

## Streaming Concurrently

Haskell lists express pure computations using composable stream operations like
`:`, `unfold`, `map`, `filter`, `zip` and `fold`.  Streamly is exactly like
lists except that it can express sequences of pure as well as monadic
computations aka streams. More importantly, it can express monadic sequences
with concurrent execution semantics without introducing any additional APIs.

Streamly expresses concurrency using standard, well known abstractions.
Concurrency semantics are defined for list operations, semigroup, applicative
and monadic compositions. Programmer does not need to know any low level
notions of concurrency like threads, locking or synchronization.  Concurrent
and non-concurrent programs are fundamentally the same.  A chosen segment of
the program can be made concurrent by annotating it with an appropriate
combinator.  We can choose a combinator for lookahead style or asynchronous
concurrency.  Concurrency is automatically scaled up or down based on the
demand from the consumer application, we can finally say goodbye to managing
thread pools and associated sizing issues.  The result is truly fearless
and declarative monadic concurrency.

## Where to use streamly?

Streamly is a general purpose programming framework.  It can be used equally
efficiently from a simple `Hello World!` program to a massively concurrent
application. The answer to the question, "where to use streamly?" - would be
similar to the answer to - "Where to use Haskell lists or the IO monad?".

Streamly simplifies streaming and makes it as intuitive as plain lists. Unlike
other streaming libraries, no fancy types are required.  Streamly is simply a
generalization of Haskell lists to monadic streaming optionally with concurrent
composition. The basic stream type in streamly `SerialT m a` can be considered
as a list type `[a]` parameterized by the monad `m`. For example, `SerialT IO
a` is a moral equivalent of `[a]` in the IO monad. `SerialT Identity a`, is
equivalent to pure lists.  Streams are constructed very much like lists, except
that they use `nil` and `cons` instead of `[]` and `:`.  Unlike lists, streams
can be constructed from monadic effects, not just pure elements.  Streams are
processed just like lists, with list like combinators, except that they are
monadic and work in a streaming fashion. In other words streamly just completes
what lists lack, you do not need to learn anything new. Please see [streamly vs
lists](docs/streamly-vs-lists.md) for a detailed comparison.

Not surprisingly, the monad instance of streamly is a list transformer, with
concurrency capability.

## Why data flow programming?

If you need some convincing for using streaming or data flow programming
paradigm itself then try to answer this question - why do we use lists in
Haskell? It boils down to why we use functional programming in the first place.
Haskell is successful in enforcing the functional data flow paradigm for pure
computations using lists, but not for monadic computations. In the absence of a
standard and easy to use data flow programming paradigm for monadic
computations, and the IO monad providing an escape hatch to an imperative
model, we just love to fall into the imperative trap, and start asking the same
fundamental question again - why do we have to use the streaming data model?

## Comparative Performance

High performance and simplicity are the two primary goals of streamly.
`Streamly` employs two different stream representations (CPS and direct style)
and interconverts between the two to get the best of both worlds on different
operations. It uses both foldr/build (for CPS style) and stream fusion (for
direct style) techniques to fuse operations. In terms of performance,
Streamly's goal is to compete with equivalent C programs. Streamly redefines
"blazing fast" for streaming libraries, it competes with lists and `vector`.
Other streaming libraries like "streaming", "pipes" and "conduit" are orders of
magnitude slower on most microbenchmarks.  See [streaming
benchmarks](https://github.com/composewell/streaming-benchmarks) for detailed
comparison.

The following chart shows a comparison of those streamly and list operations
where performance of the two differs by more than 10%. Positive y-axis displays
how many times worse is a list operation compared to the same streamly
operation, negative y-axis shows where streamly is worse compared to lists.

![Streamly vs Lists (time) comparison](charts-0/streamly-vs-list-time.svg)

Streamly uses lock-free synchronization for concurrent operations. It employs
auto-scaling of the degree of concurrency based on demand. For CPU bound tasks
it tries to keep the threads close to the number of CPUs available whereas for
IO bound tasks more threads can be utilized. Parallelism can be utilized with
little overhead even if the task size is very small.  See [concurrency
benchmarks](https://github.com/composewell/concurrency-benchmarks) for detailed
performance results and a comparison with the `async` package.

## Installing and using

Please see [INSTALL.md](./INSTALL.md) for instructions on how to use streamly
with your Haskell build tool or package manager. You may want to go through it
before jumping to run the examples below.

The module `Streamly` provides just the core stream types, type casting and
concurrency control combinators.  Stream construction, transformation, folding,
merging, zipping combinators are found in `Streamly.Prelude`.

## Streaming Pipelines

The following snippet provides a simple stream composition example that reads
numbers from stdin, prints the squares of even numbers and exits if an even
number more than 9 is entered.

``` haskell
import Streamly
import qualified Streamly.Prelude as S
import Data.Function ((&))

main = S.drain $
       S.repeatM getLine
     & fmap read
     & S.filter even
     & S.takeWhile (<= 9)
     & fmap (\x -> x * x)
     & S.mapM print
```

Unlike `pipes` or `conduit` and like `vector` and `streaming`, `streamly`
composes stream data instead of stream processors (functions).  A stream is
just like a list and is explicitly passed around to functions that process the
stream.  Therefore, no special operator is needed to join stages in a streaming
pipeline, just the standard function application (`$`) or reverse function
application (`&`) operator is enough.

## Concurrent Stream Generation

`consM` or its operator form `|:` can be used to construct a stream from
monadic actions. A stream constructed with `consM` can run the monadic actions
in the stream concurrently when used with appropriate stream type combinator
(e.g. `asyncly`, `aheadly` or `parallely`).

The following code finishes in 3 seconds (6 seconds when serial), note the
order of elements in the resulting output, the outputs are consumed as soon as
each action is finished (asyncly):

``` haskell
> let p n = threadDelay (n * 1000000) >> return n
> S.toList $ asyncly $ p 3 |: p 2 |: p 1 |: S.nil
[1,2,3]
```

Use `aheadly` if you want speculative concurrency i.e. execute the actions in
the stream concurrently but consume the results in the specified order:

``` haskell
> S.toList $ aheadly $ p 3 |: p 2 |: p 1 |: S.nil
[3,2,1]
```

Monadic stream generation functions e.g. `unfoldrM`, `replicateM`, `repeatM`,
`iterateM` and `fromFoldableM` etc. can work concurrently.

The following finishes in 10 seconds (100 seconds when serial):

``` haskell
S.drain $ asyncly $ S.replicateM 10 $ p 10
```

## Concurrency Auto Scaling

Concurrency is auto-scaled i.e. more actions are executed concurrently if the
consumer is consuming the stream at a higher speed. How many tasks are executed
concurrently can be controlled by `maxThreads` and how many results are
buffered ahead of consumption can be controlled by `maxBuffer`. See the
documentation in the `Streamly` module.

## Concurrent Streaming Pipelines

Use `|&` or `|$` to apply stream processing functions concurrently. The
following example prints a "hello" every second; if you use `&` instead of
`|&` you will see that the delay doubles to 2 seconds instead because of serial
application.

``` haskell
main = S.drain $
      S.repeatM (threadDelay 1000000 >> return "hello")
   |& S.mapM (\x -> threadDelay 1000000 >> putStrLn x)
```

## Mapping Concurrently

We can use `mapM` or `sequence` functions concurrently on a stream.

``` haskell
> let p n = threadDelay (n * 1000000) >> return n
> S.drain $ aheadly $ S.mapM (\x -> p 1 >> print x) (serially $ repeatM (p 1))
```

## Serial and Concurrent Merging

Semigroup and Monoid instances can be used to fold streams serially or
concurrently. In the following example we compose ten actions in the
stream, each with a delay of 1 to 10 seconds, respectively. Since all the
actions are concurrent we see one output printed every second:

``` haskell
import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent (threadDelay)

main = S.toList $ parallely $ foldMap delay [1..10]
 where delay n = S.yieldM $ threadDelay (n * 1000000) >> print n
```

Streams can be combined together in many ways. We provide some examples
below, see the tutorial for more ways. We use the following `delay`
function in the examples to demonstrate the concurrency aspects:

``` haskell
import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent

delay n = S.yieldM $ do
    threadDelay (n * 1000000)
    tid <- myThreadId
    putStrLn (show tid ++ ": Delay " ++ show n)
```
### Serial

``` haskell
main = S.drain $ delay 3 <> delay 2 <> delay 1
```
```
ThreadId 36: Delay 3
ThreadId 36: Delay 2
ThreadId 36: Delay 1
```

### Parallel

``` haskell
main = S.drain . parallely $ delay 3 <> delay 2 <> delay 1
```
```
ThreadId 42: Delay 1
ThreadId 41: Delay 2
ThreadId 40: Delay 3
```

## Nested Loops (aka List Transformer)

The monad instance composes like a list monad.

``` haskell
import Streamly
import qualified Streamly.Prelude as S

loops = do
    x <- S.fromFoldable [1,2]
    y <- S.fromFoldable [3,4]
    S.yieldM $ putStrLn $ show (x, y)

main = S.drain loops
```
```
(1,3)
(1,4)
(2,3)
(2,4)
```

## Concurrent Nested Loops

To run the above code with speculative concurrency i.e. each iteration in the
loop can run concurrently but the results are presented to the consumer of the
output in the same order as serial execution:

``` haskell
main = S.drain $ aheadly $ loops
```

Different stream types execute the loop iterations in different ways. For
example, `wSerially` interleaves the loop iterations. There are several
concurrent stream styles to execute the loop iterations concurrently in
different ways, see the `Streamly.Tutorial` module for a detailed treatment.

## Magical Concurrency

Streams can perform semigroup (<>) and monadic bind (>>=) operations
concurrently using combinators like `asyncly`, `parallelly`. For example,
to concurrently generate squares of a stream of numbers and then concurrently
sum the square roots of all combinations of two streams:

``` haskell
import Streamly
import qualified Streamly.Prelude as S

main = do
    s <- S.sum $ asyncly $ do
        -- Each square is performed concurrently, (<>) is concurrent
        x2 <- foldMap (\x -> return $ x * x) [1..100]
        y2 <- foldMap (\y -> return $ y * y) [1..100]
        -- Each addition is performed concurrently, monadic bind is concurrent
        return $ sqrt (x2 + y2)
    print s
```

The concurrency facilities provided by streamly can be compared with
[OpenMP](https://en.wikipedia.org/wiki/OpenMP) and
[Cilk](https://en.wikipedia.org/wiki/Cilk) but with a more declarative
expression.

## Example: Listing Directories Recursively/Concurrently

The following code snippet lists a directory tree recursively, reading multiple
directories concurrently:

```haskell
import Control.Monad.IO.Class (liftIO)
import Path.IO (listDir, getCurrentDir) -- from path-io package
import Streamly (AsyncT, adapt)
import qualified Streamly.Prelude as S

listDirRecursive :: AsyncT IO ()
listDirRecursive = getCurrentDir >>= readdir >>= liftIO . mapM_ putStrLn
  where
    readdir dir = do
      (dirs, files) <- listDir dir
      S.yield (map show dirs ++ map show files) <> foldMap readdir dirs

main :: IO ()
main = S.drain $ adapt $ listDirRecursive
```

`AsyncT` is a stream monad transformer. If you are familiar with a list
transformer, it is nothing but `ListT` with concurrency semantics. For example,
the semigroup operation `<>` is concurrent. This makes `foldMap` concurrent
too. You can replace `AsyncT` with `SerialT` and the above code will become
serial, exactly equivalent to a `ListT`.

## Rate Limiting

For bounded concurrent streams, stream yield rate can be specified. For
example, to print hello once every second you can simply write this:

``` haskell
import Streamly
import Streamly.Prelude as S

main = S.drain $ asyncly $ avgRate 1 $ S.repeatM $ putStrLn "hello"
```

For some practical uses of rate control, see
[AcidRain.hs](https://github.com/composewell/streamly/tree/master/examples/AcidRain.hs)
and
[CirclingSquare.hs](https://github.com/composewell/streamly/tree/master/examples/CirclingSquare.hs)
.
Concurrency of the stream is automatically controlled to match the specified
rate. Rate control works precisely even at throughputs as high as millions of
yields per second. For more sophisticated rate control see the haddock
documentation.

## Arrays

The `Streamly.Memory.Array` module provides immutable arrays.  Arrays are the
computing duals of streams. Streams are good at sequential access and immutable
transformations of in-transit data whereas arrays are good at random access and
in-place transformations of buffered data. Unlike streams which are potentially
infinite, arrays are necessarily finite. Arrays can be used as an efficient
interface between streams and external storage systems like memory, files and
network. Streams and arrays complete each other to provide a general purpose
computing system. The design of streamly as a general purpose computing
framework is centered around these two fundamental aspects of computing and
storage.

`Streamly.Memory.Array` uses pinned memory outside GC and therefore avoid any
GC overhead for the storage in arrays. Streamly allows efficient
transformations over arrays using streams. It uses arrays to transfer data to
and from the operating system and to store data in memory.

## Folds

Folds are consumers of streams.  `Streamly.Data.Fold` module provides a `Fold`
type that represents a `foldl'`.  Such folds can be efficiently composed
allowing the compiler to perform stream fusion and therefore implement high
performance combinators for consuming streams. A stream can be distributed to
multiple folds, or it can be partitioned across multiple folds, or
demultiplexed over multiple folds, or unzipped to two folds. We can also use
folds to fold segments of stream generating a stream of the folded results.

If you are familiar with the `foldl` library, these are the same composable
left folds but simpler and better integrated with streamly, and with many more
powerful ways of composing and applying them.

## Unfolds

Unfolds are duals of folds. Folds help us compose consumers of streams
efficiently and unfolds help us compose producers of streams efficiently.
`Streamly.Data.Unfold` provides an `Unfold` type that represents an `unfoldr`
or a stream generator. Such generators can be combined together efficiently
allowing the compiler to perform stream fusion and implement high performance
stream merging combinators.

## File IO

The following code snippets implement some common Unix command line utilities
using streamly.  You can compile these with `ghc -O2 -fspec-constr-recursive=16
-fmax-worker-args=16` and compare the performance with regular GNU coreutils
available on your system.  Though many of these are not most optimal solutions
to keep them short and elegant. Source file
[HandleIO.hs](https://github.com/composewell/streamly/tree/master/examples/HandleIO.hs)
in the examples directory includes these examples.

``` haskell
module Main where

import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Memory.Array as A
import qualified Streamly.FileSystem.Handle as FH
import qualified System.IO as FH

import Data.Char (ord)
import System.Environment (getArgs)
import System.IO (openFile, IOMode(..), stdout)

withArg f = do
    (name : _) <- getArgs
    src <- openFile name ReadMode
    f src

withArg2 f = do
    (sname : dname : _) <- getArgs
    src <- openFile sname ReadMode
    dst <- openFile dname WriteMode
    f src dst
```

### cat

``` haskell
cat = S.fold (FH.writeChunks stdout) . S.unfold FH.readChunks
main = withArg cat
```

### cp

``` haskell
cp src dst = S.fold (FH.writeChunks dst) $ S.unfold FH.readChunks src
main = withArg2 cp
```

### wc -l

``` haskell
wcl = S.length . S.splitOn (== 10) FL.drain . S.unfold FH.read
main = withArg wcl >>= print
```

### Average Line Length

``` haskell
avgll =
      S.fold avg
    . S.splitOn (== 10) FL.length
    . S.unfold FH.read

    where avg      = (/) <$> toDouble FL.sum <*> toDouble FL.length
          toDouble = fmap (fromIntegral :: Int -> Double)

main = withArg avgll >>= print
```

### Line Length Histogram

`classify` is not released yet, and is available in
`Streamly.Internal.Data.Fold`

``` haskell
llhisto =
      S.fold (FL.classify FL.length)
    . S.map bucket
    . S.splitOn (== 10) FL.length
    . S.unfold FH.read

    where
    bucket n = let i = n `mod` 10 in if i > 9 then (9,n) else (i,n)

main = withArg llhisto >>= print
```

## Socket IO

Its easy to build concurrent client and server programs using streamly.
`Streamly.Network.*` modules provide easy combinators to build network servers
and client programs using streamly. See
[FromFileClient.hs](https://github.com/composewell/streamly/tree/master/examples/FromFileClient.hs),
[EchoServer.hs](https://github.com/composewell/streamly/tree/master/examples/EchoServer.hs),
[FileSinkServer.hs](https://github.com/composewell/streamly/tree/master/examples/FileSinkServer.hs)
in the examples directory.

## Exceptions

Exceptions can be thrown at any point using the `MonadThrow` instance. Standard
exception handling combinators like `bracket`, `finally`, `handle`,
`onException` are provided in `Streamly.Prelude` module.

In presence of concurrency, synchronous exceptions work just the way they are
supposed to work in non-concurrent code. When concurrent streams
are combined together, exceptions from the constituent streams are propagated
to the consumer stream. When an exception occurs in any of the constituent
streams other concurrent streams are promptly terminated.

There is no notion of explicit threads in streamly, therefore, no
asynchronous exceptions to deal with. You can just ignore the zillions of
blogs, talks, caveats about async exceptions. Async exceptions just don't
exist.  Please don't use things like `myThreadId` and `throwTo` just for fun!

## Reactive Programming (FRP)

Streamly is a foundation for first class reactive programming as well by virtue
of integrating concurrency and streaming. See
[AcidRain.hs](https://github.com/composewell/streamly/tree/master/examples/AcidRain.hs)
for a console based FRP game example and
[CirclingSquare.hs](https://github.com/composewell/streamly/tree/master/examples/CirclingSquare.hs)
for an SDL based animation example.

## Conclusion

Streamly, short for streaming concurrently, provides monadic streams, with a
simple API, almost identical to standard lists, and an in-built
support for concurrency.  By using stream-style combinators on stream
composition, streams can be generated, merged, chained, mapped, zipped, and
consumed concurrently – providing a generalized high level programming
framework unifying streaming and concurrency. Controlled concurrency allows
even infinite streams to be evaluated concurrently.  Concurrency is auto scaled
based on feedback from the stream consumer.  The programmer does not have to be
aware of threads, locking or synchronization to write scalable concurrent
programs.

Streamly is a programmer first library, designed to be useful and friendly to
programmers for solving practical problems in a simple and concise manner. Some
key points in favor of streamly are:

  * _Simplicity_: Simple list like streaming API, if you know how to use lists
    then you know how to use streamly. This library is built with simplicity
    and ease of use as a design goal.
  * _Concurrency_: Simple, powerful, and scalable concurrency.  Concurrency is
    built-in, and not intrusive, concurrent programs are written exactly the
    same way as non-concurrent ones.
  * _Generality_: Unifies functionality provided by several disparate packages
    (streaming, concurrency, list transformer, logic programming, reactive
    programming) in a concise API.
  * _Performance_: Streamly is designed for high performance. It employs stream
    fusion optimizations for best possible performance. Serial peformance is
    equivalent to the venerable `vector` library in most cases and even better
    in some cases.  Concurrent performance is unbeatable.  See
    [streaming-benchmarks](https://github.com/composewell/streaming-benchmarks)
    for a comparison of popular streaming libraries on micro-benchmarks.

The basic streaming functionality of streamly is equivalent to that provided by
streaming libraries like
[vector](https://hackage.haskell.org/package/vector),
[streaming](https://hackage.haskell.org/package/streaming),
[pipes](https://hackage.haskell.org/package/pipes), and
[conduit](https://hackage.haskell.org/package/conduit).
In addition to providing streaming functionality, streamly subsumes
the functionality of list transformer libraries like `pipes` or
[list-t](https://hackage.haskell.org/package/list-t), and also the logic
programming library [logict](https://hackage.haskell.org/package/logict). On
the concurrency side, it subsumes the functionality of the
[async](https://hackage.haskell.org/package/async) package, and provides even
higher level concurrent composition. Because it supports
streaming with concurrency we can write FRP applications similar in concept to
[Yampa](https://hackage.haskell.org/package/Yampa) or
[reflex](https://hackage.haskell.org/package/reflex).

See the `Comparison with existing packages` section at the end of the
[tutorial](https://hackage.haskell.org/package/streamly/docs/Streamly-Tutorial.html).

## Support

Please feel free to ask questions on the
[streamly gitter channel](https://gitter.im/composewell/streamly).
If you require professional support, consulting, training or timely
enhancements to the library please contact
[support@composewell.com](mailto:support@composewell.com).

## Credits

The following authors/libraries have influenced or inspired this library in a
significant way:

  * Roman Leshchinskiy (vector)
  * Gabriel Gonzalez (foldl)
  * Alberto G. Corona (transient)

See the `credits` directory for full list of contributors, credits and licenses.

## Contributing

The code is available under BSD-3 license
[on github](https://github.com/composewell/streamly). Join the [gitter
chat](https://gitter.im/composewell/streamly) channel for discussions.  Please
ask any questions on the gitter channel or [contact the maintainer
directly](mailto:streamly@composewell.com). All contributions are welcome!
