-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP                       #-}

import Control.DeepSeq (NFData(..))
-- import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)

import Gauge
import qualified StreamDOps as D
import qualified StreamKOps as K
import qualified StreamDKOps as DK
import qualified Data.List as List

#if !MIN_VERSION_deepseq(1,4,3)
instance NFData Ordering where rnf = (`seq` ())
#endif

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.
{-# INLINE benchIO #-}
benchIO :: String -> (a IO Int -> IO ()) -> (Int -> a IO Int) -> Benchmark
benchIO name run f = bench name $ nfIO $ randomRIO (1,1) >>= run . f

{-# INLINE _benchIOSrcK #-}
_benchIOSrcK
    :: String
    -> (Int -> K.Stream IO Int)
    -> Benchmark
_benchIOSrcK name f = bench name $ nfIO $ randomRIO (1,1) >>= K.toNull . f

{-# INLINE _benchIOSrcD #-}
_benchIOSrcD
    :: String
    -> (Int -> D.Stream IO Int)
    -> Benchmark
_benchIOSrcD name f = bench name $ nfIO $ randomRIO (1,1) >>= D.toNull . f

benchFold :: NFData b
    => String -> (t IO Int -> IO b) -> (Int -> t IO Int) -> Benchmark
benchFold name f src = bench name $ nfIO $ randomRIO (1,1) >>= f . src

#ifdef DEVBUILD
-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchD #-}
benchD :: String -> (Int -> D.Stream IO Int) -> Benchmark
benchD name f = bench name $ nfIO $ randomRIO (1,1) >>= D.toNull . f

{-# INLINE benchK #-}
benchK :: String -> (Int -> K.Stream IO Int) -> Benchmark
benchK name f = bench name $ nfIO $ randomRIO (1,1) >>= K.toNull . f
#endif

{-
_benchId :: NFData b => String -> (Ops.Stream m Int -> Identity b) -> Benchmark
_benchId name f = bench name $ nf (runIdentity . f) (Ops.source 10)
-}

{-# INLINE benchPure #-}
benchPure :: String -> ([Int] -> [Int]) -> (Int -> [Int]) -> Benchmark
benchPure name run f = bench name $ nfIO $ randomRIO (1,1) >>= return . run . f

main :: IO ()
main =
  defaultMain
    [ bgroup "streamD"
      [ bgroup "generation"
        [ benchIO "unfoldr"      D.toNull D.sourceUnfoldr
        , benchIO "unfoldrM"     D.toNull D.sourceUnfoldrM
        , benchIO "intFromTo"    D.toNull D.sourceIntFromTo

        , benchIO "fromList" D.toNull D.sourceFromList
        -- , benchIO "fromFoldableM" D.sourceFromFoldableM
        ]
      , bgroup "elimination"
        [ benchIO "toNull"   D.toNull   D.sourceUnfoldrM
        , benchIO "mapM_"    D.mapM_    D.sourceUnfoldrM
        , benchIO "uncons"   D.uncons   D.sourceUnfoldrM
#ifdef DEVBUILD
        -- XXX these consume too much stack space, need to fix or segregate in
        -- another benchmark.
        , benchFold "tail"   D.tail     D.sourceUnfoldrM
        , benchIO "nullTail" D.nullTail D.sourceUnfoldrM
        , benchIO "headTail" D.headTail D.sourceUnfoldrM
        , benchFold "toList" D.toList   D.sourceUnfoldrM
#endif
        , benchFold "foldl'" D.foldl    D.sourceUnfoldrM
        , benchFold "last"   D.last     D.sourceUnfoldrM
        ]
      , bgroup "nested"
        [ benchIO "toNullAp" D.toNullApNested (D.sourceUnfoldrMN D.value2)
        , benchIO "toNull"   D.toNullNested   (D.sourceUnfoldrMN D.value2)
        , benchIO "toNull3"  D.toNullNested3  (D.sourceUnfoldrMN D.value3)
        , benchIO "filterAllIn"  D.filterAllInNested  (D.sourceUnfoldrMN K.value2)
        , benchIO "filterAllOut"  D.filterAllOutNested  (D.sourceUnfoldrMN K.value2)
        , benchIO "toNullApPure" D.toNullApNested (D.sourceUnfoldrN K.value2)
        , benchIO "toNullPure"   D.toNullNested   (D.sourceUnfoldrN K.value2)
        , benchIO "toNull3Pure"  D.toNullNested3  (D.sourceUnfoldrN K.value3)
        , benchIO "filterAllInPure"  D.filterAllInNested  (D.sourceUnfoldrN K.value2)
        , benchIO "filterAllOutPure"  D.filterAllOutNested  (D.sourceUnfoldrN K.value2)
        ]
      , bgroup "transformation"
        [ benchIO "scan"      (D.scan      1) D.sourceUnfoldrM
        , benchIO "map"       (D.map       1) D.sourceUnfoldrM
        , benchIO "fmap"      (D.fmap      1) D.sourceUnfoldrM
        , benchIO "mapM"      (D.mapM      1) D.sourceUnfoldrM
        , benchIO "mapMaybe"  (D.mapMaybe  1) D.sourceUnfoldrM
        , benchIO "mapMaybeM" (D.mapMaybeM 1) D.sourceUnfoldrM
        , benchIO "concatMapNxN" (D.concatMap 1) (D.sourceUnfoldrMN D.value2)
        , benchIO "concatMapRepl4xN" D.concatMapRepl4xN
            (D.sourceUnfoldrMN (D.value `div` 4))
        , benchIO "concatMapPureNxN" (D.concatMap 1) (D.sourceUnfoldrN D.value2)
        , benchIO "concatMapURepl4xN" D.concatMapURepl4xN
            (D.sourceUnfoldrMN (D.value `div` 4))
        , benchIO "intersperse" (D.intersperse 1) (D.sourceUnfoldrMN D.value2)
        , benchIO "interspersePure" (D.intersperse 1) (D.sourceUnfoldrN D.value2)
        -- , benchIO "foldrS"    (D.foldrS    1) D.sourceUnfoldrM
        -- This has horrible performance, never finishes
        -- , benchIO "foldlS"    (D.foldlS    1) D.sourceUnfoldrM
        ]
      , bgroup "transformationX4"
        [ benchIO "scan"      (D.scan      4) D.sourceUnfoldrM
        , benchIO "map"       (D.map       4) D.sourceUnfoldrM
        , benchIO "fmap"      (D.fmap      4) D.sourceUnfoldrM
        , benchIO "mapM"      (D.mapM      4) D.sourceUnfoldrM
        , benchIO "mapMaybe"  (D.mapMaybe  4) D.sourceUnfoldrM
        , benchIO "mapMaybeM" (D.mapMaybeM 4) D.sourceUnfoldrM
        -- , benchIO "concatMap" (D.concatMap 4) (D.sourceUnfoldrMN D.value16)
        , benchIO "intersperse" (D.intersperse 4) (D.sourceUnfoldrMN D.value16)
        ]
      , bgroup "filtering"
        [ benchIO "filter-even"     (D.filterEven     1) D.sourceUnfoldrM
        , benchIO "filter-all-out"  (D.filterAllOut   1) D.sourceUnfoldrM
        , benchIO "filter-all-in"   (D.filterAllIn    1) D.sourceUnfoldrM
        , benchIO "take-all"        (D.takeAll        1) D.sourceUnfoldrM
        , benchIO "takeWhile-true"  (D.takeWhileTrue  1) D.sourceUnfoldrM
        , benchIO "drop-one"        (D.dropOne        1) D.sourceUnfoldrM
        , benchIO "drop-all"        (D.dropAll        1) D.sourceUnfoldrM
        , benchIO "dropWhile-true"  (D.dropWhileTrue  1) D.sourceUnfoldrM
        , benchIO "dropWhile-false" (D.dropWhileFalse 1) D.sourceUnfoldrM
        ]
      , bgroup "filteringX4"
        [ benchIO "filter-even"     (D.filterEven     4) D.sourceUnfoldrM
        , benchIO "filter-all-out"  (D.filterAllOut   4) D.sourceUnfoldrM
        , benchIO "filter-all-in"   (D.filterAllIn    4) D.sourceUnfoldrM
        , benchIO "take-all"        (D.takeAll        4) D.sourceUnfoldrM
        , benchIO "takeWhile-true"  (D.takeWhileTrue  4) D.sourceUnfoldrM
        , benchIO "drop-one"        (D.dropOne        4) D.sourceUnfoldrM
        , benchIO "drop-all"        (D.dropAll        4) D.sourceUnfoldrM
        , benchIO "dropWhile-true"  (D.dropWhileTrue  4) D.sourceUnfoldrM
        , benchIO "dropWhile-false" (D.dropWhileFalse 4) D.sourceUnfoldrM
        ]
      , bgroup "zipping"
        [ benchFold "eqBy"  D.eqBy  D.sourceUnfoldrM
        , benchFold "cmpBy" D.cmpBy D.sourceUnfoldrM
        , benchIO   "zip"   D.zip   D.sourceUnfoldrM
        ]
      , bgroup "mixed"
        [ benchIO "scan-map"    (D.scanMap    1) D.sourceUnfoldrM
        , benchIO "drop-map"    (D.dropMap    1) D.sourceUnfoldrM
        , benchIO "drop-scan"   (D.dropScan   1) D.sourceUnfoldrM
        , benchIO "take-drop"   (D.takeDrop   1) D.sourceUnfoldrM
        , benchIO "take-scan"   (D.takeScan   1) D.sourceUnfoldrM
        , benchIO "take-map"    (D.takeMap    1) D.sourceUnfoldrM
        , benchIO "filter-drop" (D.filterDrop 1) D.sourceUnfoldrM
        , benchIO "filter-take" (D.filterTake 1) D.sourceUnfoldrM
        , benchIO "filter-scan" (D.filterScan 1) D.sourceUnfoldrM
        , benchIO "filter-map"  (D.filterMap  1) D.sourceUnfoldrM
        ]
      , bgroup "mixedX2"
        [ benchIO "scan-map"    (D.scanMap    2) D.sourceUnfoldrM
        , benchIO "drop-map"    (D.dropMap    2) D.sourceUnfoldrM
        , benchIO "drop-scan"   (D.dropScan   2) D.sourceUnfoldrM
        , benchIO "take-drop"   (D.takeDrop   2) D.sourceUnfoldrM
        , benchIO "take-scan"   (D.takeScan   2) D.sourceUnfoldrM
        , benchIO "take-map"    (D.takeMap    2) D.sourceUnfoldrM
        , benchIO "filter-drop" (D.filterDrop 2) D.sourceUnfoldrM
        , benchIO "filter-take" (D.filterTake 2) D.sourceUnfoldrM
        , benchIO "filter-scan" (D.filterScan 2) D.sourceUnfoldrM
        , benchIO "filter-map"  (D.filterMap  2) D.sourceUnfoldrM
        ]
      , bgroup "mixedX4"
        [ benchIO "scan-map"    (D.scanMap    4) D.sourceUnfoldrM
        , benchIO "drop-map"    (D.dropMap    4) D.sourceUnfoldrM
        , benchIO "drop-scan"   (D.dropScan   4) D.sourceUnfoldrM
        , benchIO "take-drop"   (D.takeDrop   4) D.sourceUnfoldrM
        , benchIO "take-scan"   (D.takeScan   4) D.sourceUnfoldrM
        , benchIO "take-map"    (D.takeMap    4) D.sourceUnfoldrM
        , benchIO "filter-drop" (D.filterDrop 4) D.sourceUnfoldrM
        , benchIO "filter-take" (D.filterTake 4) D.sourceUnfoldrM
        , benchIO "filter-scan" (D.filterScan 4) D.sourceUnfoldrM
        , benchIO "filter-map"  (D.filterMap  4) D.sourceUnfoldrM
        ]
#ifdef DEVBUILD
        -- XXX these consume too much stack space, need to fix or segregate in
        -- another benchmark.
      , bgroup "iterated"
        [ benchD "mapM"                 D.iterateMapM
        , benchD "scan(1/10)"           D.iterateScan
        , benchD "filterEven"           D.iterateFilterEven
        , benchD "takeAll"              D.iterateTakeAll
        , benchD "dropOne"              D.iterateDropOne
        , benchD "dropWhileFalse(1/10)" D.iterateDropWhileFalse
        , benchD "dropWhileTrue"        D.iterateDropWhileTrue
        , benchD "iterateM"             D.iterateM

        ]
#endif
      ]
    , bgroup "list"
      [ bgroup "elimination"
        [ benchPure "last" (\xs -> [List.last xs]) (K.sourceUnfoldrList K.value)
        ]
      , bgroup "nested"
        [ benchPure "toNullAp" K.toNullApNestedList (K.sourceUnfoldrList K.value2)
        , benchPure "toNull"   K.toNullNestedList (K.sourceUnfoldrList K.value2)
        , benchPure "toNull3"  K.toNullNestedList3 (K.sourceUnfoldrList K.value3)
        , benchPure "filterAllIn"  K.filterAllInNestedList (K.sourceUnfoldrList K.value2)
        , benchPure "filterAllOut"  K.filterAllOutNestedList (K.sourceUnfoldrList K.value2)
        ]
      ]
    , bgroup "streamK"
      [ bgroup "generation"
        [ benchIO "unfoldr"       K.toNull K.sourceUnfoldr
        , benchIO "unfoldrM"      K.toNull K.sourceUnfoldrM
        -- , benchIO "fromEnum"     K.toNull K.sourceFromEnum

        , benchIO "fromFoldable"  K.toNull K.sourceFromFoldable
        -- , benchIO "fromFoldableM" K.toNull K.sourceFromFoldableM

        -- appends
        , benchIO "foldMapWith"  K.toNull K.sourceFoldMapWith
        , benchIO "foldMapWithM" K.toNull K.sourceFoldMapWithM
        ]
      , bgroup "elimination"
        [ benchIO "toNull"   K.toNull   K.sourceUnfoldrM
        , benchIO "mapM_"    K.mapM_    K.sourceUnfoldrM
        , benchIO "uncons"   K.uncons   K.sourceUnfoldrM
        , benchFold "init"   K.init     K.sourceUnfoldrM
#ifdef DEVBUILD
        -- XXX these consume too much stack space, need to fix or segregate in
        -- another benchmark.
        , benchFold "tail"   K.tail     K.sourceUnfoldrM
        , benchIO "nullTail" K.nullTail K.sourceUnfoldrM
        , benchIO "headTail" K.headTail K.sourceUnfoldrM
        , benchFold "toList" K.toList   K.sourceUnfoldrM
#endif
        , benchFold "foldl'" K.foldl    K.sourceUnfoldrM
        , benchFold "last"   K.last     K.sourceUnfoldrM
        ]
      , bgroup "nested"
        [ benchIO "toNullAp" K.toNullApNested (K.sourceUnfoldrMN K.value2)
        , benchIO "toNull"   K.toNullNested   (K.sourceUnfoldrMN K.value2)
        , benchIO "toNull3"  K.toNullNested3  (K.sourceUnfoldrMN K.value3)
        , benchIO "filterAllIn"  K.filterAllInNested  (K.sourceUnfoldrMN K.value2)
        , benchIO "filterAllOut" K.filterAllOutNested (K.sourceUnfoldrMN K.value2)
        , benchIO "toNullApPure" K.toNullApNested (K.sourceUnfoldrN K.value2)
        , benchIO "toNullPure"   K.toNullNested   (K.sourceUnfoldrN K.value2)
        , benchIO "toNull3Pure"  K.toNullNested3  (K.sourceUnfoldrN K.value3)
        , benchIO "filterAllInPure"  K.filterAllInNested  (K.sourceUnfoldrN K.value2)
        , benchIO "filterAllOutPure" K.filterAllOutNested (K.sourceUnfoldrN K.value2)
        ]
      , bgroup "transformation"
        [ benchIO "scan"   (K.scan 1) K.sourceUnfoldrM
        , benchIO "map"    (K.map  1) K.sourceUnfoldrM
        , benchIO "fmap"   (K.fmap 1) K.sourceUnfoldrM
        , benchIO "mapM"   (K.mapM 1) K.sourceUnfoldrM
        , benchIO "mapMSerial"  (K.mapMSerial 1) K.sourceUnfoldrM
        -- , benchIOSrcK "concatMap" K.concatMap
        , benchIO "concatMapNxN" (K.concatMap 1) (K.sourceUnfoldrMN K.value2)
        , benchIO "concatMapPureNxN" (K.concatMap 1) (K.sourceUnfoldrN K.value2)
        , benchIO "concatMapRepl4xN" K.concatMapRepl4xN
            (K.sourceUnfoldrMN (K.value `div` 4))
        , benchIO "intersperse" (K.intersperse 1) (K.sourceUnfoldrMN K.value2)
        , benchIO "interspersePure" (K.intersperse 1) (K.sourceUnfoldrN K.value2)
#ifdef DEVBUILD
        -- XXX this consumes too much heap
        , benchIO "foldlS" (K.foldlS 1) K.sourceUnfoldrM
#endif
        ]
      , bgroup "transformationX4"
        [ benchIO "scan"   (K.scan 4) K.sourceUnfoldrM
        , benchIO "map"    (K.map  4) K.sourceUnfoldrM
        , benchIO "fmap"   (K.fmap 4) K.sourceUnfoldrM
        , benchIO "mapM"   (K.mapM 4) K.sourceUnfoldrM
        , benchIO "mapMSerial" (K.mapMSerial 4) K.sourceUnfoldrM
        -- , benchIO "concatMap" (K.concatMap 4) (K.sourceUnfoldrMN K.value16)
        , benchIO "intersperse" (K.intersperse 4) (K.sourceUnfoldrMN K.value16)
        ]
      , bgroup "filtering"
        [ benchIO "filter-even"     (K.filterEven     1) K.sourceUnfoldrM
        , benchIO "filter-all-out"  (K.filterAllOut   1) K.sourceUnfoldrM
        , benchIO "filter-all-in"   (K.filterAllIn    1) K.sourceUnfoldrM
        , benchIO "take-all"        (K.takeAll        1) K.sourceUnfoldrM
        , benchIO "takeWhile-true"  (K.takeWhileTrue  1) K.sourceUnfoldrM
        , benchIO "drop-one"        (K.dropOne        1) K.sourceUnfoldrM
        , benchIO "drop-all"        (K.dropAll        1) K.sourceUnfoldrM
        , benchIO "dropWhile-true"  (K.dropWhileTrue  1) K.sourceUnfoldrM
        , benchIO "dropWhile-false" (K.dropWhileFalse 1) K.sourceUnfoldrM
        ]
      , bgroup "filteringX4"
        [ benchIO "filter-even"     (K.filterEven     4) K.sourceUnfoldrM
        , benchIO "filter-all-out"  (K.filterAllOut   4) K.sourceUnfoldrM
        , benchIO "filter-all-in"   (K.filterAllIn    4) K.sourceUnfoldrM
        , benchIO "take-all"        (K.takeAll        4) K.sourceUnfoldrM
        , benchIO "takeWhile-true"  (K.takeWhileTrue  4) K.sourceUnfoldrM
        , benchIO "drop-one"        (K.dropOne        4) K.sourceUnfoldrM
        , benchIO "drop-all"        (K.dropAll        4) K.sourceUnfoldrM
        , benchIO "dropWhile-true"  (K.dropWhileTrue  4) K.sourceUnfoldrM
        , benchIO "dropWhile-false" (K.dropWhileFalse 4) K.sourceUnfoldrM
        ]
      , bgroup "zipping"
        [ benchIO "zip" K.zip K.sourceUnfoldrM
        ]
      , bgroup "mixed"
        [ benchIO "scan-map"    (K.scanMap    1) K.sourceUnfoldrM
        , benchIO "drop-map"    (K.dropMap    1) K.sourceUnfoldrM
        , benchIO "drop-scan"   (K.dropScan   1) K.sourceUnfoldrM
        , benchIO "take-drop"   (K.takeDrop   1) K.sourceUnfoldrM
        , benchIO "take-scan"   (K.takeScan   1) K.sourceUnfoldrM
        , benchIO "take-map"    (K.takeMap    1) K.sourceUnfoldrM
        , benchIO "filter-drop" (K.filterDrop 1) K.sourceUnfoldrM
        , benchIO "filter-take" (K.filterTake 1) K.sourceUnfoldrM
        , benchIO "filter-scan" (K.filterScan 1) K.sourceUnfoldrM
        , benchIO "filter-map"  (K.filterMap  1) K.sourceUnfoldrM
        ]
      , bgroup "mixedX2"
        [ benchIO "scan-map"    (K.scanMap    2) K.sourceUnfoldrM
        , benchIO "drop-map"    (K.dropMap    2) K.sourceUnfoldrM
        , benchIO "drop-scan"   (K.dropScan   2) K.sourceUnfoldrM
        , benchIO "take-drop"   (K.takeDrop   2) K.sourceUnfoldrM
        , benchIO "take-scan"   (K.takeScan   2) K.sourceUnfoldrM
        , benchIO "take-map"    (K.takeMap    2) K.sourceUnfoldrM
        , benchIO "filter-drop" (K.filterDrop 2) K.sourceUnfoldrM
        , benchIO "filter-take" (K.filterTake 2) K.sourceUnfoldrM
        , benchIO "filter-scan" (K.filterScan 2) K.sourceUnfoldrM
        , benchIO "filter-map"  (K.filterMap  2) K.sourceUnfoldrM
        ]
      , bgroup "mixedX4"
        [ benchIO "scan-map"    (K.scanMap    4) K.sourceUnfoldrM
        , benchIO "drop-map"    (K.dropMap    4) K.sourceUnfoldrM
        , benchIO "drop-scan"   (K.dropScan   4) K.sourceUnfoldrM
        , benchIO "take-drop"   (K.takeDrop   4) K.sourceUnfoldrM
        , benchIO "take-scan"   (K.takeScan   4) K.sourceUnfoldrM
        , benchIO "take-map"    (K.takeMap    4) K.sourceUnfoldrM
        , benchIO "filter-drop" (K.filterDrop 4) K.sourceUnfoldrM
        , benchIO "filter-take" (K.filterTake 4) K.sourceUnfoldrM
        , benchIO "filter-scan" (K.filterScan 4) K.sourceUnfoldrM
        , benchIO "filter-map"  (K.filterMap  4) K.sourceUnfoldrM
        ]
#ifdef DEVBUILD
        -- XXX these consume too much stack space, need to fix or segregate in
        -- another benchmark.
      , bgroup "iterated"
        [ benchK "mapM"                 K.iterateMapM
        , benchK "scan(1/10)"           K.iterateScan
        , benchK "filterEven"           K.iterateFilterEven
        , benchK "takeAll"              K.iterateTakeAll
        , benchK "dropOne"              K.iterateDropOne
        , benchK "dropWhileFalse(1/10)" K.iterateDropWhileFalse
        , benchK "dropWhileTrue"        K.iterateDropWhileTrue
        ]
#endif
      ]
    , bgroup "streamDK"
      [ bgroup "generation"
        [ benchIO "unfoldr"       DK.toNull DK.sourceUnfoldr
        , benchIO "unfoldrM"      DK.toNull DK.sourceUnfoldrM
        ]
      , bgroup "elimination"
        [ benchIO "toNull"   DK.toNull   DK.sourceUnfoldrM
        , benchIO "uncons"   DK.uncons   DK.sourceUnfoldrM
        ]
      ]
    ]
