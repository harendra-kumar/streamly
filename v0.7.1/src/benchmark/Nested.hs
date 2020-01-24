-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Control.DeepSeq (NFData)
import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)

import Common (parseCLIOpts)

import Streamly
import Gauge

import qualified NestedOps as Ops

benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

_benchId :: (NFData b) => String -> (Int -> Identity b) -> Benchmark
_benchId name f = bench name $ nf (\g -> runIdentity (g 1))  f

defaultStreamSize :: Int
defaultStreamSize = 100000

main :: IO ()
main = do
  -- XXX Fix indentation
  (linearCount, cfg, benches) <- parseCLIOpts defaultStreamSize
  linearCount `seq` runMode (mode cfg) cfg benches
    [ bgroup "serially"
      [ benchIO "toNullAp"       $ Ops.toNullAp linearCount       serially
      , benchIO "toNull"         $ Ops.toNull linearCount         serially
      , benchIO "toNull3"        $ Ops.toNull3 linearCount        serially
      -- , benchIO "toList"         $ Ops.toList linearCount         serially
      -- XXX takes too much stack space
      , benchIO "toListSome"     $ Ops.toListSome linearCount     serially
      , benchIO "filterAllOut"   $ Ops.filterAllOut linearCount   serially
      , benchIO "filterAllIn"    $ Ops.filterAllIn linearCount    serially
      , benchIO "filterSome"     $ Ops.filterSome linearCount     serially
      , benchIO "breakAfterSome" $ Ops.breakAfterSome linearCount serially
      ]

    , bgroup "wSerially"
      [ benchIO "toNullAp"       $ Ops.toNullAp linearCount       wSerially
      , benchIO "toNull"         $ Ops.toNull linearCount         wSerially
      , benchIO "toNull3"        $ Ops.toNull3 linearCount        wSerially
      -- , benchIO "toList"         $ Ops.toList linearCount         wSerially
      , benchIO "toListSome"     $ Ops.toListSome  linearCount    wSerially
      , benchIO "filterAllOut"   $ Ops.filterAllOut linearCount   wSerially
      , benchIO "filterAllIn"    $ Ops.filterAllIn linearCount    wSerially
      , benchIO "filterSome"     $ Ops.filterSome linearCount     wSerially
      , benchIO "breakAfterSome" $ Ops.breakAfterSome linearCount wSerially
      ]

    , bgroup "zipSerially"
      [ benchIO "toNullAp"       $ Ops.toNullAp linearCount       zipSerially
      ]
    ]
