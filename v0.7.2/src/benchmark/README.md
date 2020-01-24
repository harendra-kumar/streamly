## Running Benchmarks

`bench.sh` script at the root of the repo is the top level driver for running
benchmarks. It runs the requested benchmarks and then creates a report from the
results using the `bench-show` package. Try `bench.sh --help` for available
options to run it.

## Quick start

Run these commands from the root of the repo.

To run the default benchmarks:

```
$ ./bench.sh
```

To run all benchmarks:

```
$ ./bench.sh --benchmarks all
```

To run `linear` and `linear-async` benchmarks:

```
$ ./bench.sh --benchmarks "linear linear-async"
```

To run only the base benchmark and only the benchmarks prefixed with
`StreamD` in that (anything after a `--` is passed to gauge):

```
$ ./bench.sh --benchmarks base -- StreamD
```

## Comparing benchmarks

To compare two sets of results, first run the benchmarks at the baseline
commit:

```
$ ./bench.sh
```

And then run with the `--append` option at the commit that you want to compare
with the baseline. It will show the comparison with the baseline:

```
$ ./bench.sh --append
```

Append just adds the next set of results in the same results file. You can keep
appending more results and all of them will be compared with the baseline.

You can use `--compare` to compare the previous commit with the head commit:

```
$ ./bench.sh --compare
```

To compare the head commit with some other base commit:

```
$ ./bench.sh --compare --base d918833
```

To compare two arbitrary commits:

```
$ ./bench.sh --compare --base d918833 --candidate 38aa5f2
```

Note that the above may not always work because the script and the benchmarks
themselves might have changed across the commits. The `--append` method is more
reliable to compare.

## Available Benchmarks

The benchmark names that you can use when running `bench.sh`:

* `base`: a benchmark that measures the raw operations of the basic streams
  `StreamD` and `StreamK`.

* `linear`: measures the non-monadic operations of serial streams
* `linear-async`: measures the non-monadic operations of concurrent streams
* `linear-rate`: measures the rate limiting operations
* `nested`: measures the monadic operations of all streams
* `all`: runs all of the above benchmarks

## Reporting without measuring

You can use the `--no-measure` option to report the already measured results in
the benchmarks results file. A results file may collect an arbitrary number of
results by running with `--append` multiple times. Each benchmark has its own
results file, for example the `linear` benchmark has the results file at
`charts/linear/results.csv`.

You can also manually edit the file to remove a set of results if you like or
to append results from previously saved results or from some other results
file. After editing you can run `bench.sh` with the `--no-measure` option to
see the reports corresponding to the results.
