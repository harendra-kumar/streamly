-- |
-- Module      : Streamly.Benchmark.Data.Fold
-- Copyright   : (c) 2018 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.DeepSeq (NFData(..))
import Data.Monoid (Last(..))

import System.Random (randomRIO)
import Prelude (IO, Int, Double, String, (>), (<*>), (<$>), (+), ($),
                (<=), Monad(..), (==), Maybe(..), (.), fromIntegral,
                compare, (>=), concat, seq)

import qualified Streamly as S hiding (runStream)
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Pipe as Pipe

import qualified Streamly.Internal.Data.Sink as Sink

import qualified Streamly.Memory.Array as A
import qualified Streamly.Internal.Memory.Array as IA
import qualified Streamly.Internal.Data.Fold as IFL
import qualified Streamly.Internal.Prelude as IP

import Gauge
import Streamly hiding (runStream)
import Streamly.Benchmark.Common

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrM value n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE source #-}
source :: (S.MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
source = sourceUnfoldrM

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: (IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f = bench name $ nfIO $ randomRIO (1,1) >>= f . source value

-------------------------------------------------------------------------------
-- Stream folds
-------------------------------------------------------------------------------

o_1_space_serial_folds :: Int -> [Benchmark]
o_1_space_serial_folds value =
    [ bgroup
          "serially"
          [ bgroup
                "folds"
                [ benchIOSink value "drain" (S.fold FL.drain)
                , benchIOSink value "drainN" (S.fold (IFL.drainN value))
                , benchIOSink
                      value
                      "drainWhileTrue"
                      (S.fold (IFL.drainWhile $ (<=) (value + 1)))
                , benchIOSink
                      value
                      "drainWhileFalse"
                      (S.fold (IFL.drainWhile $ (>=) (value + 1)))
                , benchIOSink value "sink" (S.fold $ Sink.toFold Sink.drain)
                , benchIOSink value "last" (S.fold FL.last)
                , benchIOSink value "lastN.1" (S.fold (IA.lastN 1))
                , benchIOSink value "lastN.10" (S.fold (IA.lastN 10))
                , benchIOSink value "length" (S.fold FL.length)
                , benchIOSink value "sum" (S.fold FL.sum)
                , benchIOSink value "product" (S.fold FL.product)
                , benchIOSink value "maximumBy" (S.fold (FL.maximumBy compare))
                , benchIOSink value "maximum" (S.fold FL.maximum)
                , benchIOSink value "minimumBy" (S.fold (FL.minimumBy compare))
                , benchIOSink value "minimum" (S.fold FL.minimum)
                , benchIOSink
                      value
                      "mean"
                      (\s ->
                           S.fold
                               FL.mean
                               (S.map (fromIntegral :: Int -> Double) s))
                , benchIOSink
                      value
                      "variance"
                      (\s ->
                           S.fold
                               FL.variance
                               (S.map (fromIntegral :: Int -> Double) s))
                , benchIOSink
                      value
                      "stdDev"
                      (\s ->
                           S.fold
                               FL.stdDev
                               (S.map (fromIntegral :: Int -> Double) s))
                , benchIOSink
                      value
                      "mconcat"
                      (S.fold FL.mconcat . (S.map (Last . Just)))
                , benchIOSink
                      value
                      "foldMap"
                      (S.fold (FL.foldMap (Last . Just)))
                , benchIOSink value "index" (S.fold (FL.index (value + 1)))
                , benchIOSink value "head" (S.fold FL.head)
                , benchIOSink value "find" (S.fold (FL.find (== (value + 1))))
                , benchIOSink
                      value
                      "findIndex"
                      (S.fold (FL.findIndex (== (value + 1))))
                , benchIOSink
                      value
                      "elemIndex"
                      (S.fold (FL.elemIndex (value + 1)))
                , benchIOSink value "null" (S.fold FL.null)
                , benchIOSink value "elem" (S.fold (FL.elem (value + 1)))
                , benchIOSink value "notElem" (S.fold (FL.notElem (value + 1)))
                , benchIOSink value "all" (S.fold (FL.all (<= (value + 1))))
                , benchIOSink value "any" (S.fold (FL.any (> (value + 1))))
                , benchIOSink
                      value
                      "and"
                      (\s -> S.fold FL.and (S.map (<= (value + 1)) s))
                , benchIOSink
                      value
                      "or"
                      (\s -> S.fold FL.or (S.map (> (value + 1)) s))
                ]
          ]
    ]


o_1_space_serial_foldsTransforms :: Int -> [Benchmark]
o_1_space_serial_foldsTransforms value =
    [ bgroup
          "serially"
          [ bgroup
                "folds-transforms"
                [ benchIOSink value "drain" (S.fold FL.drain)
                , benchIOSink value "lmap" (S.fold (IFL.lmap (+ 1) FL.drain))
                , benchIOSink
                      value
                      "pipe-mapM"
                      (S.fold
                           (IFL.transform
                                (Pipe.mapM (\x -> return $ x + 1))
                                FL.drain))
                ]
          ]
    ]


o_1_space_serial_foldsCompositions :: Int -> [Benchmark]
o_1_space_serial_foldsCompositions value =
    [ bgroup
          "serially"
          [ bgroup
                "folds-compositions" -- Applicative
                [ benchIOSink
                      value
                      "all,any"
                      (S.fold
                           ((,) <$> FL.all (<= (value + 1)) <*>
                            FL.any (> (value + 1))))
                , benchIOSink
                      value
                      "sum,length"
                      (S.fold ((,) <$> FL.sum <*> FL.length))
                ]
          ]
    ]


o_n_heap_serial_folds :: Int -> [Benchmark]
o_n_heap_serial_folds value =
    [ bgroup
          "serially"
          [ bgroup
                "foldl"
          -- Left folds for building a structure are inherently non-streaming
          -- as the structure cannot be lazily consumed until fully built.
                [ benchIOSink value "toStream" (S.fold IP.toStream)
                , benchIOSink value "toStreamRev" (S.fold IP.toStreamRev)
                , benchIOSink value "toList" (S.fold FL.toList)
                , benchIOSink value "toListRevF" (S.fold IFL.toListRevF)
          -- Converting the stream to an array
                , benchIOSink value "lastN.Max" (S.fold (IA.lastN (value + 1)))
                , benchIOSink value "writeN" (S.fold (A.writeN value))
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
          [ bgroup "fold" $
            concat
              [ o_1_space_serial_folds value
              , o_1_space_serial_foldsTransforms value
              , o_1_space_serial_foldsCompositions value
              ]
          ]
      , bgroup
          "o-n-heap"
          [bgroup "fold" $ concat [o_n_heap_serial_folds value]]
      ]
