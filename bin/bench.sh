#!/usr/bin/env bash

SCRIPT_DIR=$(cd `dirname $0`; pwd)

RUNNING_BENCHMARKS=y
source $SCRIPT_DIR/build-lib.sh

print_help () {
  echo "Usage: $0 "
  echo "       [--benchmarks <"bench1 bench2 ..." | help>]"
  echo "       [--fields <"field1 field2 ..." | help>]"
  echo "       [--graphs]"
  echo "       [--no-measure]"
  echo "       [--append]"
  echo "       [--long]"
  echo "       [--slow]"
  echo "       [--quick]"
  echo "       [--raw]"
  echo "       [--dev-build]"
  echo "       [--with-compiler <compiler exe name>]"
  echo "       [--cabal-build-options <options>]"
  echo "       [--rtsopts <opts>]"
  echo "       [--compare] [--base <commit>] [--candidate <commit>]"
  echo "       -- <gauge options or benchmarks>"
  echo
  echo "--benchmarks: benchmarks to run, use 'help' for list of benchmarks"
  echo "--fields: measurement fields to report, use 'help' for a list"
  echo "--graphs: Generate graphical reports"
  echo "--no-measure: Don't run benchmarks, run reports from previous results"
  echo "--append: Don't overwrite previous results, append for comparison"
  echo "--long: Use much longer stream size for infinite stream benchmarks"
  echo "--slow: Slightly more accurate results at the expense of speed"
  echo "--quick: Faster results, useful for longer benchmarks"
  echo "--raw: Run the benchmarks but don't report them. This is useful when"
  echo "       you only want to work with the csv files generated."
  echo "--cabal-build-options: Pass any cabal build options to be used for build"
  echo "       e.g. --cabal-build-options \"--flag dev\""
  echo
  echo "When specific space complexity group is chosen then (and only then) "
  echo "RTS memory restrictions are used accordingly. For example, "
  echo "bench.sh --benchmarks Data.Parser -- Data.Parser/o-1-space "
  echo "restricts Heap/Stack space for O(1) characterstics"
  echo
  echo "When using --compare, by default comparative chart of HEAD^ vs HEAD"
  echo "commit is generated, in the 'charts' directory."
  echo "Use --base and --candidate to select the commits to compare."
  echo
  echo "Any arguments after a '--' are passed directly to gauge"
  exit
}

#-----------------------------------------------------------------------------
# Reporting utility functions
#-----------------------------------------------------------------------------

list_comparisons ()  {
  echo "Comparison groups:"
  for i in $COMPARISONS
  do
    echo -n "$i ["
    eval "echo -n \$$i"
    echo "]"
  done
  echo
}

# chart is expensive to build and usually not required to be rebuilt,
# use master branch as fallback
cabal_which_report() {
  local path=$(cabal_which streamly-benchmarks x $1)
  if test -z "$path"
  then
    echo "Cannot find $1 executable, trying in dist-newstyle" 1>&2
    local path1=$(cabal_which_builddir dist-newstyle streamly-benchmarks x $1)
    if test -z "$path1"
    then
      local path2="./bin/$1"
      echo "Cannot find $1 executable, trying $path2" 1>&2
      if test -e "$path2"
      then
        echo $path2
      fi
    else
        echo $path1
    fi
  else
    echo $path
  fi
}

find_report_prog() {
    local prog_name="chart"
    hash -r
    local prog_path=$(cabal_which_report $prog_name)
    if test -x "$prog_path"
    then
      echo $prog_path
    else
      return 1
    fi
}

build_report_prog() {
    local prog_name="chart"
    local prog_path=$(cabal_which_report $prog_name)

    hash -r
    if test ! -x "$prog_path" -a "$BUILD_ONCE" = "0"
    then
      echo "Building bench-show executables"
      BUILD_ONCE=1
      $CABAL_EXECUTABLE v2-build --flags dev chart \
        || die "bench-show build failed"

    elif test ! -x "$prog_path"
    then
      return 1
    fi
    return 0
}

build_report_progs() {
  if test "$RAW" = "0"
  then
      build_report_prog || exit 1
      local prog
      prog=$(find_report_prog) || \
          die "Cannot find bench-show executable"
      echo "Using bench-show executable [$prog]"
  fi
}

# We run the benchmarks in isolation in a separate process so that different
# benchmarks do not interfere with other. To enable that we need to pass the
# benchmark exe path to gauge as an argument. Unfortunately it cannot find its
# own path currently.

# The path is dependent on the architecture and cabal version.

bench_output_file() {
    local bench_name=$1
    echo "charts/$bench_name/results.csv"
}

# --min-duration 0 means exactly one iteration per sample. We use a million
# iterations in the benchmarking code explicitly and do not use the iterations
# done by the benchmarking tool.
#
# Benchmarking tool by default discards the first iteration to remove
# aberrations due to initial evaluations etc. We do not discard it because we
# are anyway doing iterations in the benchmarking code and many of them so that
# any constant factor gets amortized and anyway it is a cost that we pay in
# real life.
#
# We can pass --min-samples value from the command line as second argument
# after the benchmark name in case we want to use more than one sample.

# $1: bench name
# $2: bench executable
target_exe_extra_args () {
  local bench_name=$1
  local bench_prog=$2

  local output_file=$(bench_output_file $bench_name)
  mkdir -p `dirname $output_file`

  local QUICK_OPTS="--quick --min-duration 0"
  local SPEED_OPTIONS
  if test "$LONG" -eq 0
  then
    if test "$SLOW" -eq 0
    then
        if test "$QUICK_MODE" -eq 0
        then
          # default mode, not super quick, not slow
          SPEED_OPTIONS="$QUICK_OPTS --min-samples 10 --time-limit 1"
        else
          # super quick but less accurate
          # When the time-limit is too low and the benchmark is tiny,
          # then if the number of iterations is very small the GC stats
          # may remain 0.  So keep the time-limit at a minimum of 10 ms
          # to collect significant stats. The problem was observed in
          # the Prelude.Serial/reverse' benchmark.
          SPEED_OPTIONS="$QUICK_OPTS --time-limit 0.01 --include-first-iter"
        fi
    else
      # Slow but more accurate mode
      SPEED_OPTIONS="--min-duration 0"
    fi
  else
      # large stream size, always super quick
      GAUGE_ARGS="$GAUGE_ARGS $bench_name/o-1-space"
      SPEED_OPTIONS="--stream-size 10000000 $QUICK_OPTS --include-first-iter"
  fi

  echo "$SPEED_OPTIONS \
    --csvraw=$output_file \
    -v 2 \
    --measure-with "$SCRIPT_DIR/bench-exec-one.sh" \
    $GAUGE_ARGS"
}

run_benches_comparing() {
    local bench_list=$1

    if test -z "$CANDIDATE"
    then
      CANDIDATE=$(git rev-parse HEAD)
    fi
    if test -z "$BASE"
    then
      # XXX Should be where the current branch is forked from master
      BASE="$CANDIDATE^"
    fi
    echo "Comparing baseline commit [$BASE] with candidate [$CANDIDATE]"
    echo "Checking out base commit [$BASE] for benchmarking"
    git checkout "$BASE" || die "Checkout of base commit [$BASE] failed"

    $BUILD_BENCH || die "build failed"
    run_targets streamly-benchmarks b "$bench_list" target_exe_extra_args

    echo "Checking out candidate commit [$CANDIDATE] for benchmarking"
    git checkout "$CANDIDATE" || \
        die "Checkout of candidate [$CANDIDATE] commit failed"

    $BUILD_BENCH || die "build failed"
    run_targets streamly-benchmarks b "$bench_list" target_exe_extra_args
    # XXX reset back to the original commit
}

backup_output_file() {
  local bench_name=$1
  local output_file=$(bench_output_file $bench_name)

  if test -e $output_file -a "$APPEND" != 1
  then
      mv -f -v $output_file ${output_file}.prev
  fi
}

run_measurements() {
  local bench_list=$1

  for i in $bench_list
  do
      backup_output_file $i
  done

  if test "$COMPARE" = "0"
  then
    run_targets streamly-benchmarks b "$bench_list" target_exe_extra_args
  else
    run_benches_comparing "$bench_list"
  fi
}

run_reports() {
    local prog
    prog=$(find_report_prog) || \
      die "Cannot find bench-graph executable"
    echo

    for i in $1
    do
        echo "Generating reports for ${i}..."
        $prog $(test "$GRAPH" = 1 && echo "--graphs") \
              --benchmark $i --fields "$FIELDS"
    done
}

#-----------------------------------------------------------------------------
# Execution starts here
#-----------------------------------------------------------------------------

cd $SCRIPT_DIR/..

USE_GIT_CABAL=1
set_common_vars

DEFAULT_FIELDS="allocated bytescopied cputime"
ALL_FIELDS="$FIELDS time cycles utime stime minflt majflt nvcsw nivcsw"
FIELDS=$DEFAULT_FIELDS

COMPARE=0
BASE=
CANDIDATE=

APPEND=0
LONG=0
RAW=0
GRAPH=0
MEASURE=1

GAUGE_ARGS=
BUILD_ONCE=0

CABAL_BUILD_OPTIONS="--flag fusion-plugin "

#-----------------------------------------------------------------------------
# Read command line
#-----------------------------------------------------------------------------

while test -n "$1"
do
  case $1 in
    -h|--help|help) print_help ;;
    # options with arguments
    --benchmarks) shift; TARGETS=$1; shift ;;
    --targets) shift; TARGETS=$1; shift ;;
    --fields) shift; FIELDS=$1; shift ;;
    --base) shift; BASE=$1; shift ;;
    --candidate) shift; CANDIDATE=$1; shift ;;
    --with-compiler) shift; CABAL_WITH_COMPILER=$1; shift ;;
    --cabal-build-flags) shift; CABAL_BUILD_OPTIONS+=$1; shift ;;
    --cabal-build-options) shift; CABAL_BUILD_OPTIONS+=$1; shift ;;
    --rtsopts) shift; RTS_OPTIONS=$1; shift ;;
    # flags
    --slow) SLOW=1; shift ;;
    --quick) QUICK_MODE=1; shift ;;
    --compare) COMPARE=1; shift ;;
    --raw) RAW=1; shift ;;
    --append) APPEND=1; shift ;;
    --long) LONG=1; shift ;;
    --graphs) GRAPH=1; shift ;;
    --no-measure) MEASURE=0; shift ;;
    --dev-build) RUNNING_DEVBUILD=1; shift ;;
    --) shift; break ;;
    -*|--*) echo "Unknown flags: $*"; echo; print_help ;;
    *) break ;;
  esac
done
GAUGE_ARGS=$*

set_derived_vars

#-----------------------------------------------------------------------------
# Determine targets
#-----------------------------------------------------------------------------

only_real_benchmarks () {
  for i in $TARGETS
  do
    local SKIP=0
    for j in $COMPARISONS
    do
      if test $i == $j
      then
        SKIP=1
      fi
    done
    if test "$SKIP" -eq 0
    then
      echo -n "$i "
    fi
  done
}

# Requires RUNNING_DEVBUILD var
source $SCRIPT_DIR/targets.sh

if test "$(has_item "$TARGETS" help)" = "help"
then
  list_target_groups
  list_comparisons
  list_targets
  exit
fi

if test "$(has_item "$FIELDS" help)" = "help"
then
  echo "Supported fields: $ALL_FIELDS"
  echo "Default fields: $DEFAULT_FIELDS"
  exit
fi

if test "$LONG" -ne 0
then
  if test -n "$TARGETS"
  then
    echo "Cannot specify benchmarks [$TARGETS] with --long"
    exit
  fi
  TARGETS=$infinite_grp
fi

DEFAULT_TARGETS="$(all_grp)"
TARGETS=$(set_targets)

TARGETS_ORIG=$TARGETS
TARGETS=$(only_real_benchmarks)

echo "Using benchmark suites [$TARGETS]"

#-----------------------------------------------------------------------------
# Build reporting utility
#-----------------------------------------------------------------------------

# We need to build the report progs first at the current (latest) commit before
# checking out any other commit for benchmarking.
build_report_progs

#-----------------------------------------------------------------------------
# Build and run targets
#-----------------------------------------------------------------------------

BUILD_BENCH="$CABAL_EXECUTABLE v2-build $CABAL_BUILD_OPTIONS --enable-benchmarks"
if test "$MEASURE" = "1"
then
  run_build "$BUILD_BENCH" streamly-benchmarks bench "$TARGETS"
  export QUICK_MODE
  export RTS_OPTIONS
  run_measurements "$TARGETS"
fi

#-----------------------------------------------------------------------------
# Run reports
#-----------------------------------------------------------------------------

COMPARISON_REPORTS=""
for i in $COMPARISONS
do
  if test "$(has_item "$TARGETS_ORIG" $i)" = $i
  then
    COMPARISON_REPORTS="$COMPARISON_REPORTS $i"
    mkdir -p "charts/$i"
    constituents=$(eval "echo -n \$${i}")
    dest_file="charts/$i/results.csv"
    : > $dest_file
    for j in $constituents
    do
      cat "charts/$j/results.csv" >> $dest_file
    done
  fi
done

if test "$RAW" = "0"
then
  run_reports "$TARGETS"
  run_reports "$COMPARISON_REPORTS"
fi
