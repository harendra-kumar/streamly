{-# LANGUAGE CPP #-}
-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Control.DeepSeq (NFData(..), deepseq)
import Foreign.Storable (Storable(..))
import System.Random (randomRIO)

import qualified GHC.Exts as GHC

import qualified ArrayOps as Ops
import qualified Streamly.Internal.Memory.Array as IA
import qualified Streamly.Memory.Array as A
import qualified Streamly.Prelude as S

import Gauge

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

{-# INLINE benchPure #-}
benchPure :: NFData b => String -> (Int -> a) -> (a -> b) -> Benchmark
benchPure name src f = bench name $ nfIO $
    randomRIO (1,1) >>= return . f . src

-- Drain a source that generates a pure array
{-# INLINE benchPureSrc #-}
benchPureSrc :: (NFData a, Storable a)
    => String -> (Int -> Ops.Stream a) -> Benchmark
benchPureSrc name src = benchPure name src id

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO a) -> (a -> b) -> Benchmark
benchIO name src f = bench name $ nfIO $
    randomRIO (1,1) >>= src >>= return . f

-- Drain a source that generates an array in the IO monad
{-# INLINE benchIOSrc #-}
benchIOSrc :: (NFData a, Storable a)
    => String -> (Int -> IO (Ops.Stream a)) -> Benchmark
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

mkString :: String
mkString = "[1" ++ concat (replicate Ops.value ",1") ++ "]"

main :: IO ()
main =
  defaultMain
    [ bgroup "array"
     [  bgroup "generation"
        [ benchIOSrc "writeN . intFromTo" Ops.sourceIntFromTo
        , benchIOSrc "write . intFromTo" Ops.sourceIntFromToFromStream
        , benchIOSrc "fromList . intFromTo" Ops.sourceIntFromToFromList
        , benchIOSrc "writeN . unfoldr" Ops.sourceUnfoldr
        , benchIOSrc "writeN . fromList" Ops.sourceFromList
        , benchPureSrc "writeN . IsList.fromList" Ops.sourceIsList
        , benchPureSrc "writeN . IsString.fromString" Ops.sourceIsString
        , mkString `deepseq` (bench "read" $ nf Ops.readInstance mkString)
        , benchPureSink "show" Ops.showInstance
        ]
      , bgroup "elimination"
        [ benchPureSink "id" id
        -- , benchPureSink "eqBy" Ops.eqBy
        , benchPureSink "==" Ops.eqInstance
        , benchPureSink "/=" Ops.eqInstanceNotEq
        {-
        , benchPureSink "cmpBy" Ops.cmpBy
        -}
        , benchPureSink "<" Ops.ordInstance
        , benchPureSink "min" Ops.ordInstanceMin
        -- length is used to check for foldr/build fusion
        , benchPureSink "length . IsList.toList" (length . GHC.toList)
        , benchIOSink "foldl'" Ops.pureFoldl'
        , benchIOSink "read" (S.drain . S.unfold A.read)
        , benchIOSink "toStreamRev" (S.drain . IA.toStreamRev)
#ifdef DEVBUILD
        , benchPureSink "foldable/foldl'" Ops.foldableFoldl'
        , benchPureSink "foldable/sum" Ops.foldableSum
        -- , benchPureSinkIO "traversable/mapM" Ops.traversableMapM
#endif
        ]

        {-
        [ benchPureSink "uncons" Ops.uncons
        , benchPureSink "toNull" $ Ops.toNull serially
        , benchPureSink "mapM_" Ops.mapM_

        , benchPureSink "init" Ops.init
        , benchPureSink "tail" Ops.tail
        , benchPureSink "nullHeadTail" Ops.nullHeadTail

        -- this is too low and causes all benchmarks reported in ns
        -- , benchPureSink "head" Ops.head
        , benchPureSink "last" Ops.last
        -- , benchPureSink "lookup" Ops.lookup
        , benchPureSink "find" Ops.find
        , benchPureSink "findIndex" Ops.findIndex
        , benchPureSink "elemIndex" Ops.elemIndex

        -- this is too low and causes all benchmarks reported in ns
        -- , benchPureSink "null" Ops.null
        , benchPureSink "elem" Ops.elem
        , benchPureSink "notElem" Ops.notElem
        , benchPureSink "all" Ops.all
        , benchPureSink "any" Ops.any
        , benchPureSink "and" Ops.and
        , benchPureSink "or" Ops.or

        , benchPureSink "length" Ops.length
        , benchPureSink "sum" Ops.sum
        , benchPureSink "product" Ops.product

        , benchPureSink "maximumBy" Ops.maximumBy
        , benchPureSink "maximum" Ops.maximum
        , benchPureSink "minimumBy" Ops.minimumBy
        , benchPureSink "minimum" Ops.minimum

        , benchPureSink "toList" Ops.toList
        , benchPureSink "toRevList" Ops.toRevList
        ]
        -}
      , bgroup "transformation"
        [ benchIOSink "scanl'" (Ops.scanl' 1)
        , benchIOSink "scanl1'" (Ops.scanl1' 1)
        , benchIOSink "map" (Ops.map 1)
        {-
        , benchPureSink "fmap" (Ops.fmap 1)
        , benchPureSink "mapM" (Ops.mapM serially 1)
        , benchPureSink "mapMaybe" (Ops.mapMaybe 1)
        , benchPureSink "mapMaybeM" (Ops.mapMaybeM 1)
        , bench "sequence" $ nfIO $ randomRIO (1,1000) >>= \n ->
            Ops.sequence serially (Ops.sourceUnfoldrMAction n)
        , benchPureSink "findIndices" (Ops.findIndices 1)
        , benchPureSink "elemIndices" (Ops.elemIndices 1)
        , benchPureSink "reverse" (Ops.reverse 1)
        , benchPureSink "foldrS" (Ops.foldrS 1)
        , benchPureSink "foldrSMap" (Ops.foldrSMap 1)
        , benchPureSink "foldrT" (Ops.foldrT 1)
        , benchPureSink "foldrTMap" (Ops.foldrTMap 1)
        -}
        ]
      , bgroup "transformationX4"
        [ benchIOSink "scanl'" (Ops.scanl' 4)
        , benchIOSink "scanl1'" (Ops.scanl1' 4)
        , benchIOSink "map" (Ops.map 4)
        {-
        , benchPureSink "fmap" (Ops.fmap 4)
        , benchPureSink "mapM" (Ops.mapM serially 4)
        , benchPureSink "mapMaybe" (Ops.mapMaybe 4)
        , benchPureSink "mapMaybeM" (Ops.mapMaybeM 4)
        -- , bench "sequence" $ nfIO $ randomRIO (1,1000) >>= \n ->
            -- Ops.sequence serially (Ops.sourceUnfoldrMAction n)
        , benchPureSink "findIndices" (Ops.findIndices 4)
        , benchPureSink "elemIndices" (Ops.elemIndices 4)
        -}
        ]
        {-
      , bgroup "filtering"
        [ benchPureSink "filter-even"     (Ops.filterEven 1)
        , benchPureSink "filter-all-out"  (Ops.filterAllOut 1)
        , benchPureSink "filter-all-in"   (Ops.filterAllIn 1)
        , benchPureSink "take-all"        (Ops.takeAll 1)
        , benchPureSink "takeWhile-true"  (Ops.takeWhileTrue 1)
        --, benchPureSink "takeWhileM-true" (Ops.takeWhileMTrue 1)
        , benchPureSink "drop-one"        (Ops.dropOne 1)
        , benchPureSink "drop-all"        (Ops.dropAll 1)
        , benchPureSink "dropWhile-true"  (Ops.dropWhileTrue 1)
        --, benchPureSink "dropWhileM-true" (Ops.dropWhileMTrue 1)
        , benchPureSink "dropWhile-false" (Ops.dropWhileFalse 1)
        , benchPureSink "deleteBy" (Ops.deleteBy 1)
        , benchPureSink "insertBy" (Ops.insertBy 1)
        ]
      , bgroup "filteringX4"
        [ benchPureSink "filter-even"     (Ops.filterEven 4)
        , benchPureSink "filter-all-out"  (Ops.filterAllOut 4)
        , benchPureSink "filter-all-in"   (Ops.filterAllIn 4)
        , benchPureSink "take-all"        (Ops.takeAll 4)
        , benchPureSink "takeWhile-true"  (Ops.takeWhileTrue 4)
        --, benchPureSink "takeWhileM-true" (Ops.takeWhileMTrue 4)
        , benchPureSink "drop-one"        (Ops.dropOne 4)
        , benchPureSink "drop-all"        (Ops.dropAll 4)
        , benchPureSink "dropWhile-true"  (Ops.dropWhileTrue 4)
        --, benchPureSink "dropWhileM-true" (Ops.dropWhileMTrue 4)
        , benchPureSink "dropWhile-false" (Ops.dropWhileFalse 4)
        , benchPureSink "deleteBy" (Ops.deleteBy 4)
        , benchPureSink "insertBy" (Ops.insertBy 4)
        ]
      , bgroup "multi-stream"
        [ benchPureSink "eqBy" Ops.eqBy
        , benchPureSink "cmpBy" Ops.cmpBy
        , benchPureSink "zip" Ops.zip
        , benchPureSink "zipM" Ops.zipM
        , benchPureSink "mergeBy" Ops.mergeBy
        , benchPureSink "isPrefixOf" Ops.isPrefixOf
        , benchPureSink "isSubsequenceOf" Ops.isSubsequenceOf
        , benchPureSink "stripPrefix" Ops.stripPrefix
        , benchPureSrc  serially "concatMap" Ops.concatMap
        ]
    -- scanl-map and foldl-map are equivalent to the scan and fold in the foldl
    -- library. If scan/fold followed by a map is efficient enough we may not
    -- need monolithic implementations of these.
    , bgroup "mixed"
      [ benchPureSink "scanl-map" (Ops.scanMap 1)
      , benchPureSink "foldl-map" Ops.foldl'ReduceMap
      , benchPureSink "sum-product-fold"  Ops.sumProductFold
      , benchPureSink "sum-product-scan"  Ops.sumProductScan
      ]
    , bgroup "mixedX4"
      [ benchPureSink "scan-map"    (Ops.scanMap 4)
      , benchPureSink "drop-map"    (Ops.dropMap 4)
      , benchPureSink "drop-scan"   (Ops.dropScan 4)
      , benchPureSink "take-drop"   (Ops.takeDrop 4)
      , benchPureSink "take-scan"   (Ops.takeScan 4)
      , benchPureSink "take-map"    (Ops.takeMap 4)
      , benchPureSink "filter-drop" (Ops.filterDrop 4)
      , benchPureSink "filter-take" (Ops.filterTake 4)
      , benchPureSink "filter-scan" (Ops.filterScan 4)
      , benchPureSink "filter-scanl1" (Ops.filterScanl1 4)
      , benchPureSink "filter-map"  (Ops.filterMap 4)
      ]
    , bgroup "iterated"
      [ benchPureSrc serially "mapM"           Ops.iterateMapM
      , benchPureSrc serially "scan(1/100)"    Ops.iterateScan
      , benchPureSrc serially "scanl1(1/100)"  Ops.iterateScanl1
      , benchPureSrc serially "filterEven"     Ops.iterateFilterEven
      , benchPureSrc serially "takeAll"        Ops.iterateTakeAll
      , benchPureSrc serially "dropOne"        Ops.iterateDropOne
      , benchPureSrc serially "dropWhileFalse" Ops.iterateDropWhileFalse
      , benchPureSrc serially "dropWhileTrue"  Ops.iterateDropWhileTrue
      ]
      -}
    ]
    ]
