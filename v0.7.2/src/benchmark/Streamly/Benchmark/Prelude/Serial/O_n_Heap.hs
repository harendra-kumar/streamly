-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Gauge

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    size <- limitStreamSize value
    size `seq` runMode (mode cfg) cfg benches (allBenchmarks size)

    where

    -- Operations using O(1) stack space and O(n) heap space.
    -- Tail recursive left folds
    allBenchmarks size =
        concat
            [ o_n_heap_serial_foldl size
            , o_n_heap_serial_buffering size
            ]
