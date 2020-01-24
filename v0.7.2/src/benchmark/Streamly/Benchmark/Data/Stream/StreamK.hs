-- |
-- Module      : Streamly.Benchmark.Data.Stream.StreamK
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.Benchmark.Data.Stream.StreamK
    (
      o_1_space
    , o_n_stack
    , o_n_heap
    , o_n_space
    , o_1_space_list
    )
where

import Control.Monad (when)
import Data.Maybe (isJust)
import Prelude
       (Monad, Int, (+), ($), (.), return, even, (>), (<=), div,
        subtract, undefined, Maybe(..), not, (>>=),
        maxBound, flip, (<$>), (<*>), round, (/), (**), (<), foldr, fmap)
import System.Random (randomRIO)
import qualified Prelude as P
import qualified Data.List as List

import qualified Streamly.Internal.Data.Stream.StreamK as S
import qualified Streamly.Internal.Data.Stream.Prelude as SP
import qualified Streamly.Internal.Data.SVar as S

import Streamly.Benchmark.Common (benchFold)
import Gauge (bench, nfIO, bgroup, Benchmark)

value, value2, value3, value16, maxValue :: Int
value = 100000
value2 = round (P.fromIntegral value**(1/2::P.Double)) -- double nested loop
value3 = round (P.fromIntegral value**(1/3::P.Double)) -- triple nested loop
value16 = round (P.fromIntegral value**(1/16::P.Double)) -- triple nested loop
maxValue = value

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

{-# INLINE toNull #-}
{-# INLINE uncons #-}
{-# INLINE nullTail #-}
{-# INLINE headTail #-}
{-# INLINE zip #-}
toNull, uncons, nullTail, headTail, zip
    :: Monad m
    => Stream m Int -> m ()

{-# INLINE toList #-}
toList :: Monad m => Stream m Int -> m [Int]
{-# INLINE foldl #-}
foldl :: Monad m => Stream m Int -> m Int
{-# INLINE last #-}
last :: Monad m => Stream m Int -> m (Maybe Int)

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream m a = S.Stream m a

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: Int -> Stream m Int
sourceUnfoldr n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrN #-}
sourceUnfoldrN :: Int -> Int -> Stream m Int
sourceUnfoldrN m n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + m
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: S.MonadAsync m => Int -> Stream m Int
sourceUnfoldrM n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldrMN #-}
sourceUnfoldrMN :: S.MonadAsync m => Int -> Int -> Stream m Int
sourceUnfoldrMN m n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + m
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceFromFoldable #-}
sourceFromFoldable :: Int -> Stream m Int
sourceFromFoldable n = S.fromFoldable [n..n+value]

{-# INLINE sourceFromFoldableM #-}
sourceFromFoldableM :: S.MonadAsync m => Int -> Stream m Int
sourceFromFoldableM n =
    Prelude.foldr S.consM S.nil (Prelude.fmap return [n..n+value])

{-# INLINE sourceFoldMapWith #-}
sourceFoldMapWith :: Int -> Stream m Int
sourceFoldMapWith n = SP.foldMapWith S.serial S.yield [n..n+value]

{-# INLINE sourceFoldMapWithM #-}
sourceFoldMapWithM :: Monad m => Int -> Stream m Int
sourceFoldMapWithM n = SP.foldMapWith S.serial (S.yieldM . return) [n..n+value]

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.drain
-- runStream = S.mapM_ (\_ -> return ())

{-# INLINE mapM_ #-}
mapM_ :: Monad m => Stream m a -> m ()
mapM_ = S.mapM_ (\_ -> return ())

toNull = runStream
uncons s = do
    r <- S.uncons s
    case r of
        Nothing -> return ()
        Just (_, t) -> uncons t

{-# INLINE init #-}
init :: (Monad m, S.IsStream t) => t m a -> m ()
init s = do
    t <- S.init s
    P.mapM_ S.drain t

{-# INLINE tail #-}
tail :: (Monad m, S.IsStream t) => t m a -> m ()
tail s = S.tail s >>= P.mapM_ tail

nullTail s = do
    r <- S.null s
    when (not r) $ S.tail s >>= P.mapM_ nullTail

headTail s = do
    h <- S.head s
    when (isJust h) $ S.tail s >>= P.mapM_ headTail

toList = S.toList
foldl  = S.foldl' (+) 0
last   = S.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Stream m a -> m ()
transform = runStream

{-# INLINE composeN #-}
composeN
    :: Monad m
    => Int -> (Stream m Int -> Stream m Int) -> Stream m Int -> m ()
composeN n f =
    case n of
        1 -> transform . f
        2 -> transform . f . f
        3 -> transform . f . f . f
        4 -> transform . f . f . f . f
        _ -> undefined

{-# INLINE scan #-}
{-# INLINE map #-}
{-# INLINE fmapK #-}
{-# INLINE filterEven #-}
{-# INLINE filterAllOut #-}
{-# INLINE filterAllIn #-}
{-# INLINE _takeOne #-}
{-# INLINE takeAll #-}
{-# INLINE takeWhileTrue #-}
{-# INLINE dropOne #-}
{-# INLINE dropAll #-}
{-# INLINE dropWhileTrue #-}
{-# INLINE dropWhileFalse #-}
{-# INLINE foldrS #-}
{-# INLINE foldlS #-}
{-# INLINE concatMap #-}
scan, map, fmapK, filterEven, filterAllOut,
    filterAllIn, _takeOne, takeAll, takeWhileTrue, dropAll, dropOne,
    dropWhileTrue, dropWhileFalse, foldrS, foldlS, concatMap
    :: Monad m
    => Int -> Stream m Int -> m ()

{-# INLINE mapM #-}
{-# INLINE mapMSerial #-}
{-# INLINE intersperse #-}
mapM, mapMSerial, intersperse
    :: S.MonadAsync m => Int -> Stream m Int -> m ()

scan           n = composeN n $ S.scanl' (+) 0
map            n = composeN n $ P.fmap (+1)
fmapK          n = composeN n $ P.fmap (+1)
mapM           n = composeN n $ S.mapM return
mapMSerial     n = composeN n $ S.mapMSerial return
filterEven     n = composeN n $ S.filter even
filterAllOut   n = composeN n $ S.filter (> maxValue)
filterAllIn    n = composeN n $ S.filter (<= maxValue)
_takeOne       n = composeN n $ S.take 1
takeAll        n = composeN n $ S.take maxValue
takeWhileTrue  n = composeN n $ S.takeWhile (<= maxValue)
dropOne        n = composeN n $ S.drop 1
dropAll        n = composeN n $ S.drop maxValue
dropWhileTrue  n = composeN n $ S.dropWhile (<= maxValue)
dropWhileFalse n = composeN n $ S.dropWhile (<= 1)
foldrS         n = composeN n $ S.foldrS S.cons S.nil
foldlS         n = composeN n $ S.foldlS (flip S.cons) S.nil
-- We use a (sqrt n) element stream as source and then concat the same stream
-- for each element to produce an n element stream.
concatMap      n = composeN n $ (\s -> S.concatMap (\_ -> s) s)
intersperse    n = composeN n $ S.intersperse maxValue

-------------------------------------------------------------------------------
-- Iteration
-------------------------------------------------------------------------------

iterStreamLen, maxIters :: Int
iterStreamLen = 10
maxIters = 10000

{-# INLINE iterateSource #-}
iterateSource
    :: S.MonadAsync m
    => (Stream m Int -> Stream m Int) -> Int -> Int -> Stream m Int
iterateSource g i n = f i (sourceUnfoldrMN iterStreamLen n)
    where
        f (0 :: Int) m = g m
        f x m = g (f (x P.- 1) m)

{-# INLINE iterateMapM #-}
{-# INLINE iterateScan #-}
{-# INLINE iterateFilterEven #-}
{-# INLINE iterateTakeAll #-}
{-# INLINE iterateDropOne #-}
{-# INLINE iterateDropWhileFalse #-}
{-# INLINE iterateDropWhileTrue #-}
iterateMapM, iterateScan, iterateFilterEven, iterateTakeAll, iterateDropOne,
    iterateDropWhileFalse, iterateDropWhileTrue
    :: S.MonadAsync m
    => Int -> Stream m Int

-- this is quadratic
iterateScan            = iterateSource (S.scanl' (+) 0) (maxIters `div` 10)
iterateDropWhileFalse  = iterateSource (S.dropWhile (> maxValue))
                                       (maxIters `div` 10)

iterateMapM            = iterateSource (S.mapM return) maxIters
iterateFilterEven      = iterateSource (S.filter even) maxIters
iterateTakeAll         = iterateSource (S.take maxValue) maxIters
iterateDropOne         = iterateSource (S.drop 1) maxIters
iterateDropWhileTrue   = iterateSource (S.dropWhile (<= maxValue)) maxIters

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

zip src       = transform $ S.zipWith (,) src src

{-# INLINE concatMapRepl4xN #-}
concatMapRepl4xN :: Monad m => Stream m Int -> m ()
concatMapRepl4xN src = transform $ (S.concatMap (S.replicate 4) src)

-------------------------------------------------------------------------------
-- Mixed Composition
-------------------------------------------------------------------------------

{-# INLINE scanMap #-}
{-# INLINE dropMap #-}
{-# INLINE dropScan #-}
{-# INLINE takeDrop #-}
{-# INLINE takeScan #-}
{-# INLINE takeMap #-}
{-# INLINE filterDrop #-}
{-# INLINE filterTake #-}
{-# INLINE filterScan #-}
{-# INLINE filterMap #-}
scanMap, dropMap, dropScan, takeDrop, takeScan, takeMap, filterDrop,
    filterTake, filterScan, filterMap
    :: Monad m => Int -> Stream m Int -> m ()

scanMap    n = composeN n $ S.map (subtract 1) . S.scanl' (+) 0
dropMap    n = composeN n $ S.map (subtract 1) . S.drop 1
dropScan   n = composeN n $ S.scanl' (+) 0 . S.drop 1
takeDrop   n = composeN n $ S.drop 1 . S.take maxValue
takeScan   n = composeN n $ S.scanl' (+) 0 . S.take maxValue
takeMap    n = composeN n $ S.map (subtract 1) . S.take maxValue
filterDrop n = composeN n $ S.drop 1 . S.filter (<= maxValue)
filterTake n = composeN n $ S.take maxValue . S.filter (<= maxValue)
filterScan n = composeN n $ S.scanl' (+) 0 . S.filter (<= maxBound)
filterMap  n = composeN n $ S.map (subtract 1) . S.filter (<= maxValue)

-------------------------------------------------------------------------------
-- Nested Composition
-------------------------------------------------------------------------------

{-# INLINE toNullApNested #-}
toNullApNested :: Monad m => Stream m Int -> m ()
toNullApNested s = runStream $ do
    (+) <$> s <*> s

{-# INLINE toNullNested #-}
toNullNested :: Monad m => Stream m Int -> m ()
toNullNested s = runStream $ do
    x <- s
    y <- s
    return $ x + y

{-# INLINE toNullNested3 #-}
toNullNested3 :: Monad m => Stream m Int -> m ()
toNullNested3 s = runStream $ do
    x <- s
    y <- s
    z <- s
    return $ x + y + z

{-# INLINE filterAllOutNested #-}
filterAllOutNested
    :: Monad m
    => Stream m Int -> m ()
filterAllOutNested str = runStream $ do
    x <- str
    y <- str
    let s = x + y
    if s < 0
    then return s
    else S.nil

{-# INLINE filterAllInNested #-}
filterAllInNested
    :: Monad m
    => Stream m Int -> m ()
filterAllInNested str = runStream $ do
    x <- str
    y <- str
    let s = x + y
    if s > 0
    then return s
    else S.nil

-------------------------------------------------------------------------------
-- Nested Composition Pure lists
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldrList #-}
sourceUnfoldrList :: Int -> Int -> [Int]
sourceUnfoldrList maxval n = List.unfoldr step n
    where
    step cnt =
        if cnt > n + maxval
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE toNullApNestedList #-}
toNullApNestedList :: [Int] -> [Int]
toNullApNestedList s = (+) <$> s <*> s

{-# INLINE toNullNestedList #-}
toNullNestedList :: [Int] -> [Int]
toNullNestedList s = do
    x <- s
    y <- s
    return $ x + y

{-# INLINE toNullNestedList3 #-}
toNullNestedList3 :: [Int] -> [Int]
toNullNestedList3 s = do
    x <- s
    y <- s
    z <- s
    return $ x + y + z

{-# INLINE filterAllOutNestedList #-}
filterAllOutNestedList :: [Int] -> [Int]
filterAllOutNestedList str = do
    x <- str
    y <- str
    let s = x + y
    if s < 0
    then return s
    else []

{-# INLINE filterAllInNestedList #-}
filterAllInNestedList :: [Int] -> [Int]
filterAllInNestedList str = do
    x <- str
    y <- str
    let s = x + y
    if s > 0
    then return s
    else []

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

o_1_space :: [Benchmark]
o_1_space =
    [ bgroup "streamK"
      [ bgroup "generation"
        [ benchFold "unfoldr"       toNull sourceUnfoldr
        , benchFold "unfoldrM"      toNull sourceUnfoldrM

        , benchFold "fromFoldable"  toNull sourceFromFoldable
        , benchFold "fromFoldableM" toNull sourceFromFoldableM

        -- appends
        , benchFold "foldMapWith"  toNull sourceFoldMapWith
        , benchFold "foldMapWithM" toNull sourceFoldMapWithM
        ]
      , bgroup "elimination"
        [ benchFold "toNull"   toNull   sourceUnfoldrM
        , benchFold "mapM_"    mapM_    sourceUnfoldrM
        , benchFold "uncons"   uncons   sourceUnfoldrM
        , benchFold "init"   init     sourceUnfoldrM
        , benchFold "foldl'" foldl    sourceUnfoldrM
        , benchFold "last"   last     sourceUnfoldrM
        ]
      , bgroup "nested"
        [ benchFold "toNullAp" toNullApNested (sourceUnfoldrMN value2)
        , benchFold "toNull"   toNullNested   (sourceUnfoldrMN value2)
        , benchFold "toNull3"  toNullNested3  (sourceUnfoldrMN value3)
        , benchFold "filterAllIn"  filterAllInNested  (sourceUnfoldrMN value2)
        , benchFold "filterAllOut" filterAllOutNested (sourceUnfoldrMN value2)
        , benchFold "toNullApPure" toNullApNested (sourceUnfoldrN value2)
        , benchFold "toNullPure"   toNullNested   (sourceUnfoldrN value2)
        , benchFold "toNull3Pure"  toNullNested3  (sourceUnfoldrN value3)
        , benchFold "filterAllInPure"  filterAllInNested  (sourceUnfoldrN value2)
        , benchFold "filterAllOutPure" filterAllOutNested (sourceUnfoldrN value2)
        ]
      , bgroup "transformation"
        [ benchFold "foldrS" (foldrS 1) sourceUnfoldrM
        , benchFold "scan"   (scan 1) sourceUnfoldrM
        , benchFold "map"    (map  1) sourceUnfoldrM
        , benchFold "fmap"   (fmapK 1) sourceUnfoldrM
        , benchFold "mapM"   (mapM 1) sourceUnfoldrM
        , benchFold "mapMSerial"  (mapMSerial 1) sourceUnfoldrM
        -- , benchFoldSrcK "concatMap" concatMap
        , benchFold "concatMapNxN" (concatMap 1) (sourceUnfoldrMN value2)
        , benchFold "concatMapPureNxN" (concatMap 1) (sourceUnfoldrN value2)
        , benchFold "concatMapRepl4xN" concatMapRepl4xN
            (sourceUnfoldrMN (value `div` 4))
        ]
      , bgroup "transformationX4"
        [ benchFold "scan"   (scan 4) sourceUnfoldrM
        , benchFold "map"    (map  4) sourceUnfoldrM
        , benchFold "fmap"   (fmapK 4) sourceUnfoldrM
        , benchFold "mapM"   (mapM 4) sourceUnfoldrM
        , benchFold "mapMSerial" (mapMSerial 4) sourceUnfoldrM
        -- XXX this is horribly slow
        -- , benchFold "concatMap" (concatMap 4) (sourceUnfoldrMN value16)
        ]
      , bgroup "filtering"
        [ benchFold "filter-even"     (filterEven     1) sourceUnfoldrM
        , benchFold "filter-all-out"  (filterAllOut   1) sourceUnfoldrM
        , benchFold "filter-all-in"   (filterAllIn    1) sourceUnfoldrM
        , benchFold "take-all"        (takeAll        1) sourceUnfoldrM
        , benchFold "takeWhile-true"  (takeWhileTrue  1) sourceUnfoldrM
        , benchFold "drop-one"        (dropOne        1) sourceUnfoldrM
        , benchFold "drop-all"        (dropAll        1) sourceUnfoldrM
        , benchFold "dropWhile-true"  (dropWhileTrue  1) sourceUnfoldrM
        , benchFold "dropWhile-false" (dropWhileFalse 1) sourceUnfoldrM
        ]
      , bgroup "filteringX4"
        [ benchFold "filter-even"     (filterEven     4) sourceUnfoldrM
        , benchFold "filter-all-out"  (filterAllOut   4) sourceUnfoldrM
        , benchFold "filter-all-in"   (filterAllIn    4) sourceUnfoldrM
        , benchFold "take-all"        (takeAll        4) sourceUnfoldrM
        , benchFold "takeWhile-true"  (takeWhileTrue  4) sourceUnfoldrM
        , benchFold "drop-one"        (dropOne        4) sourceUnfoldrM
        , benchFold "drop-all"        (dropAll        4) sourceUnfoldrM
        , benchFold "dropWhile-true"  (dropWhileTrue  4) sourceUnfoldrM
        , benchFold "dropWhile-false" (dropWhileFalse 4) sourceUnfoldrM
        ]
      , bgroup "zipping"
        [ benchFold "zip" zip sourceUnfoldrM
        ]
      , bgroup "mixed"
        [ benchFold "scan-map"    (scanMap    1) sourceUnfoldrM
        , benchFold "drop-map"    (dropMap    1) sourceUnfoldrM
        , benchFold "drop-scan"   (dropScan   1) sourceUnfoldrM
        , benchFold "take-drop"   (takeDrop   1) sourceUnfoldrM
        , benchFold "take-scan"   (takeScan   1) sourceUnfoldrM
        , benchFold "take-map"    (takeMap    1) sourceUnfoldrM
        , benchFold "filter-drop" (filterDrop 1) sourceUnfoldrM
        , benchFold "filter-take" (filterTake 1) sourceUnfoldrM
        , benchFold "filter-scan" (filterScan 1) sourceUnfoldrM
        , benchFold "filter-map"  (filterMap  1) sourceUnfoldrM
        ]
      , bgroup "mixedX2"
        [ benchFold "scan-map"    (scanMap    2) sourceUnfoldrM
        , benchFold "drop-map"    (dropMap    2) sourceUnfoldrM
        , benchFold "drop-scan"   (dropScan   2) sourceUnfoldrM
        , benchFold "take-drop"   (takeDrop   2) sourceUnfoldrM
        , benchFold "take-scan"   (takeScan   2) sourceUnfoldrM
        , benchFold "take-map"    (takeMap    2) sourceUnfoldrM
        , benchFold "filter-drop" (filterDrop 2) sourceUnfoldrM
        , benchFold "filter-take" (filterTake 2) sourceUnfoldrM
        , benchFold "filter-scan" (filterScan 2) sourceUnfoldrM
        , benchFold "filter-map"  (filterMap  2) sourceUnfoldrM
        ]
      , bgroup "mixedX4"
        [ benchFold "scan-map"    (scanMap    4) sourceUnfoldrM
        , benchFold "drop-map"    (dropMap    4) sourceUnfoldrM
        , benchFold "drop-scan"   (dropScan   4) sourceUnfoldrM
        , benchFold "take-drop"   (takeDrop   4) sourceUnfoldrM
        , benchFold "take-scan"   (takeScan   4) sourceUnfoldrM
        , benchFold "take-map"    (takeMap    4) sourceUnfoldrM
        , benchFold "filter-drop" (filterDrop 4) sourceUnfoldrM
        , benchFold "filter-take" (filterTake 4) sourceUnfoldrM
        , benchFold "filter-scan" (filterScan 4) sourceUnfoldrM
        , benchFold "filter-map"  (filterMap  4) sourceUnfoldrM
        ]
      ]
    ]

o_n_heap :: [Benchmark]
o_n_heap =
    [ bgroup "streamK"
      [ bgroup "transformation"
        [ benchFold "foldlS" (foldlS 1) sourceUnfoldrM
        ]
      ]
    ]

{-# INLINE benchK #-}
benchK :: P.String -> (Int -> Stream P.IO Int) -> Benchmark
benchK name f = bench name $ nfIO $ randomRIO (1,1) >>= toNull . f

o_n_stack :: [Benchmark]
o_n_stack =
    [ bgroup "streamK"
      [ bgroup "elimination"
        [ benchFold "tail"   tail     sourceUnfoldrM
        , benchFold "nullTail" nullTail sourceUnfoldrM
        , benchFold "headTail" headTail sourceUnfoldrM
        ]
      , bgroup "transformation"
        [
          -- XXX why do these need so much stack
          benchFold "intersperse" (intersperse 1) (sourceUnfoldrMN value2)
        , benchFold "interspersePure" (intersperse 1) (sourceUnfoldrN value2)
        ]
      , bgroup "transformationX4"
        [
          benchFold "intersperse" (intersperse 4) (sourceUnfoldrMN value16)
        ]
      , bgroup "iterated"
        [ benchK "mapM"                 iterateMapM
        , benchK "scan(1/10)"           iterateScan
        , benchK "filterEven"           iterateFilterEven
        , benchK "takeAll"              iterateTakeAll
        , benchK "dropOne"              iterateDropOne
        , benchK "dropWhileFalse(1/10)" iterateDropWhileFalse
        , benchK "dropWhileTrue"        iterateDropWhileTrue
        ]
      ]
   ]

o_n_space :: [Benchmark]
o_n_space =
    [ bgroup "streamK"
      [ bgroup "elimination"
        [ benchFold "toList" toList   sourceUnfoldrM
        ]
      ]
   ]

{-# INLINE benchList #-}
benchList :: P.String -> ([Int] -> [Int]) -> (Int -> [Int]) -> Benchmark
benchList name run f = bench name $ nfIO $ randomRIO (1,1) >>= return . run . f

o_1_space_list :: [Benchmark]
o_1_space_list =
    [ bgroup "list"
      [ bgroup "elimination"
        [ benchList "last" (\xs -> [List.last xs]) (sourceUnfoldrList value)
        ]
      , bgroup "nested"
        [ benchList "toNullAp" toNullApNestedList (sourceUnfoldrList value2)
        , benchList "toNull"   toNullNestedList (sourceUnfoldrList value2)
        , benchList "toNull3"  toNullNestedList3 (sourceUnfoldrList value3)
        , benchList "filterAllIn"  filterAllInNestedList (sourceUnfoldrList value2)
        , benchList "filterAllOut"  filterAllOutNestedList (sourceUnfoldrList value2)
        ]
      ]
    ]
