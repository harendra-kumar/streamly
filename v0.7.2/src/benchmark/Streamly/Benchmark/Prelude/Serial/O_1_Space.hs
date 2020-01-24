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
    value `seq` runMode (mode cfg) cfg benches (allBenchmarks value)

    where

    allBenchmarks value =
        concat
            [ serial value
            , wSerial value
            , zipSerial value
            ]

    serial value =
        concat
            [ o_1_space_serial_pure value
            , o_1_space_serial_foldable value
            , o_1_space_serial_generation value
            , o_1_space_serial_elimination value
            , o_1_space_serial_foldMultiStream value
            , o_1_space_serial_pipes value
            , o_1_space_serial_pipesX4 value
            , o_1_space_serial_transformer value
            , o_1_space_serial_transformation value
            , o_1_space_serial_transformationX4 value
            , o_1_space_serial_filtering value
            , o_1_space_serial_filteringX4 value
            , o_1_space_serial_joining value
            , o_1_space_serial_concatFoldable value
            , o_1_space_serial_concatSerial value
            , o_1_space_serial_outerProductStreams value
            , o_1_space_serial_mixed value
            , o_1_space_serial_mixedX4 value
            ]

    wSerial value =
        concat
            [ o_1_space_wSerial_transformation value
            , o_1_space_wSerial_concatMap value
            , o_1_space_wSerial_outerProduct value
            ]

    zipSerial value = concat [o_1_space_zipSerial_transformation value]
