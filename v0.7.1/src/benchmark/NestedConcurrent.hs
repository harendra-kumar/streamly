-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Control.DeepSeq (NFData)
import Control.Monad (when)
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
  let finiteCount = min linearCount defaultStreamSize
  when (finiteCount /= linearCount) $
    putStrLn $ "Limiting stream size to "
               ++ show defaultStreamSize
               ++ " for finite stream operations only"

  finiteCount `seq` linearCount `seq` runMode (mode cfg) cfg benches
    [
      bgroup "aheadly"
      [ benchIO "toNullAp"       $ Ops.toNullAp linearCount       aheadly
      , benchIO "toNull"         $ Ops.toNull linearCount         aheadly
      , benchIO "toNull3"        $ Ops.toNull3 linearCount        aheadly
      -- , benchIO "toList"         $ Ops.toList linearCount         aheadly
      -- XXX consumes too much stack space
      , benchIO "toListSome"     $ Ops.toListSome linearCount     aheadly
      , benchIO "filterAllOut"   $ Ops.filterAllOut linearCount   aheadly
      , benchIO "filterAllIn"    $ Ops.filterAllIn linearCount    aheadly
      , benchIO "filterSome"     $ Ops.filterSome linearCount     aheadly
      , benchIO "breakAfterSome" $ Ops.breakAfterSome linearCount aheadly
      ]

    , bgroup "asyncly"
      [ benchIO "toNullAp"       $ Ops.toNullAp linearCount       asyncly
      , benchIO "toNull"         $ Ops.toNull linearCount         asyncly
      , benchIO "toNull3"        $ Ops.toNull3 linearCount        asyncly
      -- , benchIO "toList"         $ Ops.toList linearCount         asyncly
      , benchIO "toListSome"     $ Ops.toListSome  linearCount    asyncly
      , benchIO "filterAllOut"   $ Ops.filterAllOut linearCount   asyncly
      , benchIO "filterAllIn"    $ Ops.filterAllIn linearCount    asyncly
      , benchIO "filterSome"     $ Ops.filterSome linearCount     asyncly
      , benchIO "breakAfterSome" $ Ops.breakAfterSome linearCount asyncly
      ]

    , bgroup "zipAsyncly"
      [ benchIO "toNullAp"       $ Ops.toNullAp linearCount       zipAsyncly
      ]

    -- Operations that are not scalable to infinite streams
    , bgroup "finite"
      [ bgroup "wAsyncly"
        [ benchIO "toNullAp"       $ Ops.toNullAp finiteCount       wAsyncly
        , benchIO "toNull"         $ Ops.toNull finiteCount         wAsyncly
        , benchIO "toNull3"        $ Ops.toNull3 finiteCount        wAsyncly
        -- , benchIO "toList"         $ Ops.toList finiteCount         wAsyncly
        , benchIO "toListSome"     $ Ops.toListSome finiteCount     wAsyncly
        , benchIO "filterAllOut"   $ Ops.filterAllOut finiteCount   wAsyncly
        -- , benchIO "filterAllIn"    $ Ops.filterAllIn finiteCount    wAsyncly
        , benchIO "filterSome"     $ Ops.filterSome finiteCount     wAsyncly
        , benchIO "breakAfterSome" $ Ops.breakAfterSome finiteCount wAsyncly
        ]
      ]
    ]
