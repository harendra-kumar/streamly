-- |
-- Module      : Streamly.Benchmark.Data.Fold
-- Copyright   : (c) 2018 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.DeepSeq (NFData(..))

import System.Random (randomRIO)

import Gauge

import Prelude hiding (concat)

import Streamly.Benchmark.Common
import Streamly.Benchmark.Data.NestedUnfoldOps

{-# INLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-------------------------------------------------------------------------------
-- Stream folds
-------------------------------------------------------------------------------

o_1_space_serial_outerProductUnfolds :: Int -> [Benchmark]
o_1_space_serial_outerProductUnfolds value =
    [ bgroup
          "serially"
          [ bgroup
                "outer-product-unfolds"
                [ benchIO "toNull" $ toNull value
                , benchIO "toNull3" $ toNull3 value
                , benchIO "concat" $ concat value
                , benchIO "filterAllOut" $ filterAllOut value
                , benchIO "filterAllIn" $ filterAllIn value
                , benchIO "filterSome" $ filterSome value
                , benchIO "breakAfterSome" $ breakAfterSome value
                ]
          ]
    ]


o_n_space_serial_outerProductUnfolds :: Int -> [Benchmark]
o_n_space_serial_outerProductUnfolds value =
    [ bgroup
          "serially"
          [ bgroup
                "outer-product-unfolds"
                [ benchIO "toList" $ toList value
                , benchIO "toListSome" $ toListSome value
                ]
          ]
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
  (value, cfg, benches) <- parseCLIOpts defaultStreamSize
  value `seq` runMode (mode cfg) cfg benches (allBenchmarks value)
  where
    allBenchmarks value =
      [ bgroup
          "o-1-space"
          [bgroup "unfold" (o_1_space_serial_outerProductUnfolds value)]
      , bgroup
          "o-n-space"
          [bgroup "unfold" (o_n_space_serial_outerProductUnfolds value)]
      ]
