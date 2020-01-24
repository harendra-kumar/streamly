{-# LANGUAGE CPP #-}
-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Control.DeepSeq (NFData)
-- import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)

import Common (parseCLIOpts)

import Streamly
import Gauge

import qualified Streamly.Benchmark.Prelude as Ops

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.
--
-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIO #-}
benchIO :: (IsStream t, NFData b) => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIO value name f = bench name $ nfIO $ randomRIO (1,1) >>= f . Ops.source value

-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchSrcIO #-}
benchSrcIO
    :: (t IO a -> SerialT IO a)
    -> String
    -> (Int -> t IO a)
    -> Benchmark
benchSrcIO t name f
    = bench name $ nfIO $ randomRIO (1,1) >>= Ops.toNull t . f

{-# INLINE benchMonadicSrcIO #-}
benchMonadicSrcIO :: String -> (Int -> IO ()) -> Benchmark
benchMonadicSrcIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

{-
_benchId :: NFData b => String -> (Ops.Stream m Int -> Identity b) -> Benchmark
_benchId name f = bench name $ nf (runIdentity . f) (Ops.source 10)
-}

defaultStreamSize :: Int
defaultStreamSize = 100000

main :: IO ()
main = do
  -- XXX Fix indentation
  (value, cfg, benches) <- parseCLIOpts defaultStreamSize
  let value2 = round $ sqrt $ (fromIntegral value :: Double)
  value2 `seq` value `seq` runMode (mode cfg) cfg benches
    [ bgroup "asyncly"
        [ benchSrcIO asyncly "unfoldr" (Ops.sourceUnfoldr value)
        , benchSrcIO asyncly "unfoldrM" (Ops.sourceUnfoldrM value)
        , benchSrcIO asyncly "fromFoldable" (Ops.sourceFromFoldable value)
        , benchSrcIO asyncly "fromFoldableM" (Ops.sourceFromFoldableM value)
        , benchSrcIO asyncly "foldMapWith" (Ops.sourceFoldMapWith value)
        , benchSrcIO asyncly "foldMapWithM" (Ops.sourceFoldMapWithM value)
        , benchSrcIO asyncly "foldMapM" (Ops.sourceFoldMapM value)
        , benchIO value "map"    $ Ops.map' asyncly 1
        , benchIO value "fmap"   $ Ops.fmap' asyncly 1
        , benchIO value "mapM"   $ Ops.mapM asyncly 1
        , benchSrcIO asyncly "unfoldrM maxThreads 1"
            (maxThreads 1 . Ops.sourceUnfoldrM value)
        , benchSrcIO asyncly "unfoldrM maxBuffer 1 (x/10 ops)"
            (maxBuffer 1 . Ops.sourceUnfoldrMN (value `div` 10))
        , benchMonadicSrcIO "concatMapWith (2,x/2)"
            (Ops.concatStreamsWith async 2 (value `div` 2))
        , benchMonadicSrcIO "concatMapWith (sqrt x,sqrt x)"
            (Ops.concatStreamsWith async value2 value2)
        , benchMonadicSrcIO "concatMapWith (sqrt x * 2,sqrt x / 2)"
            (Ops.concatStreamsWith async (value2 * 2) (value2 `div` 2))
        ]
      , bgroup "wAsyncly"
        [ benchSrcIO wAsyncly "unfoldr" (Ops.sourceUnfoldr value)
        , benchSrcIO wAsyncly "unfoldrM" (Ops.sourceUnfoldrM value)
        , benchSrcIO wAsyncly "fromFoldable" (Ops.sourceFromFoldable value)
        , benchSrcIO wAsyncly "fromFoldableM" (Ops.sourceFromFoldableM value)
        , benchSrcIO wAsyncly "foldMapWith" (Ops.sourceFoldMapWith value)
        , benchSrcIO wAsyncly "foldMapWithM" (Ops.sourceFoldMapWithM value)
        , benchSrcIO wAsyncly "foldMapM" (Ops.sourceFoldMapM value)
        , benchIO value "map"    $ Ops.map' wAsyncly 1
        , benchIO value "fmap"   $ Ops.fmap' wAsyncly 1
        , benchIO value "mapM"   $ Ops.mapM wAsyncly 1
        , benchSrcIO wAsyncly "unfoldrM maxThreads 1"
            (maxThreads 1 . Ops.sourceUnfoldrM value)
        , benchSrcIO wAsyncly "unfoldrM maxBuffer 1 (x/10 ops)"
            (maxBuffer 1 . Ops.sourceUnfoldrMN (value `div` 10))
        -- When we merge streams using wAsync the size of the queue increases
        -- slowly because of the binary composition adding just one more item
        -- to the work queue only after every scheduling pass through the
        -- work queue.
        --
        -- We should see the memory consumption increasing slowly if these
        -- benchmarks are left to run on infinite number of streams of infinite
        -- sizes.
        , benchMonadicSrcIO "concatMapWith (2,x/2)"
            (Ops.concatStreamsWith wAsync 2 (value `div` 2))
        , benchMonadicSrcIO "concatMapWith (sqrt x,sqrt x)"
            (Ops.concatStreamsWith wAsync value2 value2)
        , benchMonadicSrcIO "concatMapWith (sqrt x * 2,sqrt x / 2)"
            (Ops.concatStreamsWith wAsync (value2 * 2) (value2 `div` 2))
        ]
      -- unfoldr and fromFoldable are always serial and therefore the same for
      -- all stream types.
      , bgroup "aheadly"
        [ benchSrcIO aheadly "unfoldr" (Ops.sourceUnfoldr value)
        , benchSrcIO aheadly "unfoldrM" (Ops.sourceUnfoldrM value)
        , benchSrcIO aheadly "fromFoldableM" (Ops.sourceFromFoldableM value)
        , benchSrcIO aheadly "foldMapWith" (Ops.sourceFoldMapWith value)
        , benchSrcIO aheadly "foldMapWithM" (Ops.sourceFoldMapWithM value)
        , benchSrcIO aheadly "foldMapM" (Ops.sourceFoldMapM value)
        , benchIO value "map"  $ Ops.map' aheadly 1
        , benchIO value "fmap" $ Ops.fmap' aheadly 1
        , benchIO value "mapM" $ Ops.mapM aheadly 1
        , benchSrcIO aheadly "unfoldrM maxThreads 1"
            (maxThreads 1 . Ops.sourceUnfoldrM value)
        , benchSrcIO aheadly "unfoldrM maxBuffer 1 (x/10 ops)"
            (maxBuffer 1 . Ops.sourceUnfoldrMN (value `div` 10))
        , benchSrcIO aheadly "fromFoldable" (Ops.sourceFromFoldable value)
        , benchMonadicSrcIO "concatMapWith (2,x/2)"
            (Ops.concatStreamsWith ahead 2 (value `div` 2))
        , benchMonadicSrcIO "concatMapWith (sqrt x,sqrt x)"
            (Ops.concatStreamsWith ahead value2 value2)
        , benchMonadicSrcIO "concatMapWith (sqrt x * 2,sqrt x / 2)"
            (Ops.concatStreamsWith ahead (value2 * 2) (value2 `div` 2))
        ]
      , bgroup "zip"
        [ benchSrcIO serially "zipAsync (2,x/2)" (Ops.zipAsync (value `div` 2))
        , benchSrcIO serially "zipAsyncM (2,x/2)"
            (Ops.zipAsyncM (value `div` 2))
        , benchSrcIO serially "zipAsyncAp (2,x/2)"
            (Ops.zipAsyncAp (value `div` 2))
        , benchIO value "fmap zipAsyncly"  $ Ops.fmap' zipAsyncly 1
        , benchSrcIO serially "mergeAsyncBy (2,x/2)"
            (Ops.mergeAsyncBy (value `div` 2))
        , benchSrcIO serially "mergeAsyncByM (2,x/2)"
            (Ops.mergeAsyncByM (value `div` 2))
        -- Parallel stages in a pipeline
        , benchIO value "parAppMap" Ops.parAppMap
        , benchIO value "parAppSum" Ops.parAppSum
        ]
      ]
