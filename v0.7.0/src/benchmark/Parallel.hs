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
import qualified NestedOps as Nested

{-# INLINE benchIONested #-}
benchIONested :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIONested name f = bench name $ nfIO $ randomRIO (1,1) >>= f

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
    :: (t IO Int -> SerialT IO Int)
    -> String
    -> (Int -> t IO Int)
    -> Benchmark
benchSrcIO t name f
    = bench name $ nfIO $ randomRIO (1,1) >>= Ops.toNull t . f

defaultStreamSize :: Int
defaultStreamSize = 100000

linear :: Int -> Int -> [Benchmark]
linear value value2 =
    [ -- unfoldr is pure and works serially irrespective of the stream type
      benchSrcIO parallely "unfoldr" (Ops.sourceUnfoldr value)
    , benchSrcIO parallely "unfoldrM" (Ops.sourceUnfoldrM value)
    , benchSrcIO parallely "fromFoldable" (Ops.sourceFromFoldable value)
    , benchSrcIO parallely "fromFoldableM" (Ops.sourceFromFoldableM value)
    , benchSrcIO parallely "foldMapWith" (Ops.sourceFoldMapWith value)
    , benchSrcIO parallely "foldMapWithM" (Ops.sourceFoldMapWithM value)
    , benchSrcIO parallely "foldMapM" (Ops.sourceFoldMapM value)
    -- map/fmap are pure and therefore no concurrency would be added on top
    -- of what the source stream (i.e. unfoldrM) provides.
    , benchIO value "map"  $ Ops.map' parallely 1
    , benchIO value "fmap" $ Ops.fmap' parallely 1
    , benchIO value "mapM" $ Ops.mapM parallely 1
    , benchIONested "concatMapWith (2,x/2)"
        (Ops.concatStreamsWith parallel 2 (value `div` 2))
    , benchIONested "concatMapWith (sqrt x,sqrt x)"
        (Ops.concatStreamsWith parallel value2 value2)
    , benchIONested "concatMapWith (sqrt x * 2,sqrt x / 2)"
        (Ops.concatStreamsWith parallel (value2 * 2) (value2 `div` 2))
    ]

nested :: Int -> [Benchmark]
nested value =
    [ benchIONested "toNullAp"       $ Nested.toNullAp value       parallely
    , benchIONested "toNull"         $ Nested.toNull value         parallely
    , benchIONested "toNull3"        $ Nested.toNull3 value        parallely
    -- , benchIO "toList"         $ Ops.toList value         parallely
    -- XXX fix thread blocked indefinitely in MVar
    -- , benchIO "toListSome"     $ Ops.toListSome value     parallely
    , benchIONested "filterAllOut"   $ Nested.filterAllOut value   parallely
    , benchIONested "filterAllIn"    $ Nested.filterAllIn value    parallely
    , benchIONested "filterSome"     $ Nested.filterSome value     parallely
    , benchIONested "breakAfterSome" $ Nested.breakAfterSome value parallely
    ]

main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    let value2 = round $ sqrt $ (fromIntegral value :: Double)
    value2 `seq` value `seq`
        runMode (mode cfg) cfg benches $
            [ bgroup "parallelly"
              [ bgroup "linear" $ linear value value2
              , bgroup "nested" $ nested value
              ]
            ]
