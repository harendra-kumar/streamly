-- |
-- Module      : NestedUnfold
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Control.DeepSeq (NFData)
import System.Random (randomRIO)

import Common (parseCLIOpts)

import qualified NestedUnfoldOps as Ops

import Gauge

benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

defaultStreamSize :: Int
defaultStreamSize = 100000

main :: IO ()
main = do
  (linearCount, cfg, benches) <- parseCLIOpts defaultStreamSize
  linearCount `seq` runMode (mode cfg) cfg benches
    [ bgroup "unfold"
      [ benchIO "toNull"         $ Ops.toNull linearCount
      , benchIO "toNull3"        $ Ops.toNull3 linearCount
      , benchIO "concat"         $ Ops.concat linearCount
      -- , benchIO "toList"         $ Ops.toList
      , benchIO "toListSome"     $ Ops.toListSome linearCount
      , benchIO "filterAllOut"   $ Ops.filterAllOut linearCount
      , benchIO "filterAllIn"    $ Ops.filterAllIn linearCount
      , benchIO "filterSome"     $ Ops.filterSome linearCount
      , benchIO "breakAfterSome" $ Ops.breakAfterSome linearCount
      ]
    ]
