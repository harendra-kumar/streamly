# Compilation Options

## Recommended Options

Benchmarks show that GHC 8.8 has significantly better performance than GHC 8.6
in many cases.

Use the following GHC options:

```
  -O2 
  -fdicts-strict 
  -fspec-constr-recursive=16 
  -fmax-worker-args=16
```

## Using Fusion Plugin

In many cases `fusion-plugin` can improve performance by better stream
fusion. However, in some cases performance may also regress. Please note
that the `fusion-plugin` package works only for GHC >= 8.6.

* Install the
[fusion-plugin](https://hackage.haskell.org/package/fusion-plugin)
package or add it to the `build-depends` section of your program in the
cabal file.

* Use the following GHC option in addition to the options listed in the
  previous section:

```
  -fplugin=Fusion.Plugin 
```

## Minimal

At the very least `-O -fdicts-strict` compilation options are
required. If these options are not used, the program may exhibit memory
hog.  For example, the following program, if compiled without an
optimization option, is known to hog memory:

```
main = S.drain $ S.concatMap S.fromList $ S.repeat []
```

## Explanation

* `-fdicts-strict` is needed to avoid [a GHC
issue](https://gitlab.haskell.org/ghc/ghc/issues/17745) leading to
memory leak in some cases.

* `-fspec-constr-recursive` is needed for better stream fusion by enabling
the `SpecConstr` optimization in more cases.

* `-fmax-worker-args` is needed for better stream fusion by enabling the
`SpecConstr` optimization in some important cases.

* `-fplugin=Fusion.Plugin` enables predictable stream fusion
optimization in certain cases by helping the compiler inline internal
bindings and therefore enabling case-of-case optimization. In some
cases, especially in some fileio benchmarks, it can make a difference of
5-10x better performance.

# Multi-core Parallelism

Concurrency without a threaded runtime may be a bit more efficient. Do not use
threaded runtime unless you really need multi-core parallelism. To get
multi-core parallelism use the following GHC options:

  `-threaded -with-rtsopts "-N"`

# Compiler Versions

Use GHC 8.8 for best performance.

GHC 8.2.2 may hog memory and hang when building certain application using
streamly (particularly the benchmark programs in the streamly package).
Therefore we recommend avoiding using the GHC version 8.2.x.

# Performance Optimizations

If performance is below expectations:

* Look for inlining functions in the fast path
* Strictness annotations on data, specially the data used as accumulator in
  folds and scans, can help in improving performance.
* Strictness annotations on function arguments can help the compiler unbox
  constructors in certain cases, improving performance.
* Sometimes using `-XStrict` extension can help improve performance, if so you
  may be missing some strictness annotations.
* Use tail recursion for streaming data or for large loops
