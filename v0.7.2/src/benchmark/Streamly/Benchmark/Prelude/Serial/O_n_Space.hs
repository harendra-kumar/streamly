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

    allBenchmarks size =
        concat
            [ o_n_space_serial_toList size -- < 2MB
            , o_n_space_serial_outerProductStreams size
            , o_n_space_wSerial_outerProductStreams size
            , o_n_space_serial_traversable size -- < 2MB
            , o_n_space_serial_foldr size
            ]
