-- |
-- Module      : Main
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}

module Main (main) where

import Control.DeepSeq (NFData(..))
import System.Random (randomRIO)

import qualified Streamly.Benchmark.Data.Prim.ArrayOps as Ops
import qualified Streamly.Internal.Data.Prim.Array as A
import qualified Streamly.Prelude as S

import Gauge

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO a) -> (a -> b) -> Benchmark
benchIO name src f = bench name $ nfIO $
    randomRIO (1,1) >>= src >>= return . f

-- Drain a source that generates an array in the IO monad
{-# INLINE benchIOSrc #-}
benchIOSrc :: A.Prim a => String -> (Int -> IO (Ops.Stream a)) -> Benchmark
benchIOSrc name src = benchIO name src id

{-# INLINE benchPureSink #-}
benchPureSink :: NFData b => String -> (Ops.Stream Int -> b) -> Benchmark
benchPureSink name f = benchIO name Ops.sourceIntFromTo f

{-# INLINE benchIO' #-}
benchIO' :: NFData b => String -> (Int -> IO a) -> (a -> IO b) -> Benchmark
benchIO' name src f = bench name $ nfIO $
    randomRIO (1,1) >>= src >>= f

{-# INLINE benchIOSink #-}
benchIOSink :: NFData b => String -> (Ops.Stream Int -> IO b) -> Benchmark
benchIOSink name f = benchIO' name Ops.sourceIntFromTo f

{-
mkString :: String
mkString = "[1" ++ concat (replicate Ops.value ",1") ++ "]"
-}

main :: IO ()
main =
  defaultMain
    [ bgroup "Data.Prim.Array"
     [  bgroup "generation"
        [ benchIOSrc "writeN . intFromTo" Ops.sourceIntFromTo
        , benchIOSrc "write . intFromTo" Ops.sourceIntFromToFromStream
        , benchIOSrc "fromList . intFromTo" Ops.sourceIntFromToFromList
        , benchIOSrc "writeN . unfoldr" Ops.sourceUnfoldr
        , benchIOSrc "writeN . fromList" Ops.sourceFromList
        -- , benchPureSrc "writeN . IsList.fromList" Ops.sourceIsList
        -- , benchPureSrc "writeN . IsString.fromString" Ops.sourceIsString
        -- , mkString `deepseq` (bench "read" $ nf Ops.readInstance mkString)
        , benchPureSink "show" Ops.showInstance
        ]
      , bgroup "elimination"
        [ benchPureSink "id" id
        , benchPureSink "==" Ops.eqInstance
        , benchPureSink "/=" Ops.eqInstanceNotEq
        , benchPureSink "<" Ops.ordInstance
        , benchPureSink "min" Ops.ordInstanceMin
        -- length is used to check for foldr/build fusion
        -- , benchPureSink "length . IsList.toList" (length . GHC.toList)
        , benchIOSink "foldl'" Ops.pureFoldl'
        , benchIOSink "read" (S.drain . S.unfold A.read)
        , benchIOSink "toStreamRev" (S.drain . A.toStreamRev)
#if 0
        -- PrimArray does not have a Foldable instance because it requires a
        -- Prim constraint. Though it should be possible to make an instance in
        -- the same way as we do in Memory.Array.
        , benchPureSink "foldable/foldl'" Ops.foldableFoldl'
        , benchPureSink "foldable/sum" Ops.foldableSum
#endif
        ]
      , bgroup "transformation"
        [ benchIOSink "scanl'" (Ops.scanl' 1)
        , benchIOSink "scanl1'" (Ops.scanl1' 1)
        , benchIOSink "map" (Ops.map 1)
        ]
      , bgroup "transformationX4"
        [ benchIOSink "scanl'" (Ops.scanl' 4)
        , benchIOSink "scanl1'" (Ops.scanl1' 4)
        , benchIOSink "map" (Ops.map 4)
        ]
    ]
    ]
