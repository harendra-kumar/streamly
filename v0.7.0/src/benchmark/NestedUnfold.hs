-- |
-- Module      : NestedUnfold
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Control.DeepSeq (NFData)
import System.Random (randomRIO)

import qualified NestedUnfoldOps as Ops

import Gauge

benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

main :: IO ()
main =
  defaultMain
    [ bgroup "unfold"
      [ benchIO "toNull"         $ Ops.toNull
      , benchIO "toNull3"        $ Ops.toNull3
      , benchIO "concat"         $ Ops.concat
      , benchIO "toList"         $ Ops.toList
      , benchIO "toListSome"     $ Ops.toListSome
      , benchIO "filterAllOut"   $ Ops.filterAllOut
      , benchIO "filterAllIn"    $ Ops.filterAllIn
      , benchIO "filterSome"     $ Ops.filterSome
      , benchIO "breakAfterSome" $ Ops.breakAfterSome
      ]
    ]
