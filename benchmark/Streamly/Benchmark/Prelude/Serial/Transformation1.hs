-- |
-- Module      : Serial.Transformation1
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Serial.Transformation1 (benchmarks) where

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity)
import Data.IORef (newIORef, modifyIORef')
import System.Random (randomRIO)

#ifdef INSPECTION
import Test.Inspection
#endif

import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Stream.IsStream as Internal
import qualified Streamly.Internal.Data.Fold as FL
import qualified Prelude

import Gauge
import Streamly.Prelude (SerialT, serially, MonadAsync)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude
import Streamly.Internal.Data.Time.Units
import Prelude hiding (sequence, mapM, fmap)

-------------------------------------------------------------------------------
-- Pipelines (stream-to-stream transformations)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- one-to-one transformations
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Traversable Instance
-------------------------------------------------------------------------------

{-# INLINE traversableTraverse #-}
traversableTraverse :: SerialT Identity Int -> IO (SerialT Identity Int)
traversableTraverse = traverse return

{-# INLINE traversableSequenceA #-}
traversableSequenceA :: SerialT Identity Int -> IO (SerialT Identity Int)
traversableSequenceA = sequenceA . Prelude.fmap return

{-# INLINE traversableMapM #-}
traversableMapM :: SerialT Identity Int -> IO (SerialT Identity Int)
traversableMapM = Prelude.mapM return

{-# INLINE traversableSequence #-}
traversableSequence :: SerialT Identity Int -> IO (SerialT Identity Int)
traversableSequence = Prelude.sequence . Prelude.fmap return

{-# INLINE benchPureSinkIO #-}
benchPureSinkIO
    :: NFData b
    => Int -> String -> (SerialT Identity Int -> IO b) -> Benchmark
benchPureSinkIO value name f =
    bench name $ nfIO $ randomRIO (1, 1) >>= f . sourceUnfoldr value

o_n_space_traversable :: Int -> [Benchmark]
o_n_space_traversable value =
    -- Buffering operations using heap proportional to number of elements.
    [ bgroup "traversable"
        -- Traversable instance
        [ benchPureSinkIO value "traverse" traversableTraverse
        , benchPureSinkIO value "sequenceA" traversableSequenceA
        , benchPureSinkIO value "mapM" traversableMapM
        , benchPureSinkIO value "sequence" traversableSequence
        ]
    ]

-------------------------------------------------------------------------------
-- maps and scans
-------------------------------------------------------------------------------

{-# INLINE scanl' #-}
scanl' :: MonadIO m => Int -> SerialT m Int -> m ()
scanl' n = composeN n $ S.scanl' (+) 0

{-# INLINE scanlM' #-}
scanlM' :: MonadIO m => Int -> SerialT m Int -> m ()
scanlM' n = composeN n $ S.scanlM' (\b a -> return $ b + a) (return 0)

{-# INLINE scanl1' #-}
scanl1' :: MonadIO m => Int -> SerialT m Int -> m ()
scanl1' n = composeN n $ S.scanl1' (+)

{-# INLINE scanl1M' #-}
scanl1M' :: MonadIO m => Int -> SerialT m Int -> m ()
scanl1M' n = composeN n $ S.scanl1M' (\b a -> return $ b + a)

{-# INLINE scan #-}
scan :: MonadIO m => Int -> SerialT m Int -> m ()
scan n = composeN n $ S.scan FL.sum

{-# INLINE postscanl' #-}
postscanl' :: MonadIO m => Int -> SerialT m Int -> m ()
postscanl' n = composeN n $ S.postscanl' (+) 0

{-# INLINE postscanlM' #-}
postscanlM' :: MonadIO m => Int -> SerialT m Int -> m ()
postscanlM' n = composeN n $ S.postscanlM' (\b a -> return $ b + a) (return 0)

{-# INLINE postscan #-}
postscan :: MonadIO m => Int -> SerialT m Int -> m ()
postscan n = composeN n $ S.postscan FL.sum

{-# INLINE sequence #-}
sequence ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> t m (m Int)
    -> m ()
sequence t = S.drain . t . S.sequence

{-# INLINE tap #-}
tap :: MonadIO m => Int -> SerialT m Int -> m ()
tap n = composeN n $ S.tap FL.sum

{-# INLINE tapRate #-}
tapRate :: Int -> SerialT IO Int -> IO ()
tapRate n str = do
    cref <- newIORef 0
    composeN n (Internal.tapRate 1 (\c -> modifyIORef' cref (c +))) str

{-# INLINE pollCounts #-}
pollCounts :: Int -> SerialT IO Int -> IO ()
pollCounts n =
    composeN n (Internal.pollCounts (const True) f FL.drain)

    where

    f = Internal.rollingMap (-) . Internal.delayPost 1

{-# INLINE timestamped #-}
timestamped :: (S.MonadAsync m) => SerialT m Int -> m ()
timestamped = S.drain . Internal.timestamped

{-# INLINE foldrS #-}
foldrS :: MonadIO m => Int -> SerialT m Int -> m ()
foldrS n = composeN n $ Internal.foldrS S.cons S.nil

{-# INLINE foldrSMap #-}
foldrSMap :: MonadIO m => Int -> SerialT m Int -> m ()
foldrSMap n = composeN n $ Internal.foldrS (\x xs -> x + 1 `S.cons` xs) S.nil

{-# INLINE foldrT #-}
foldrT :: MonadIO m => Int -> SerialT m Int -> m ()
foldrT n = composeN n $ Internal.foldrT S.cons S.nil

{-# INLINE foldrTMap #-}
foldrTMap :: MonadIO m => Int -> SerialT m Int -> m ()
foldrTMap n = composeN n $ Internal.foldrT (\x xs -> x + 1 `S.cons` xs) S.nil


{-# INLINE trace #-}
trace :: MonadAsync m => Int -> SerialT m Int -> m ()
trace n = composeN n $ Internal.trace return

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup
        "mapping"
        [
        -- Right folds
          benchIOSink value "foldrS" (foldrS 1)
        , benchIOSink value "foldrSMap" (foldrSMap 1)
        , benchIOSink value "foldrT" (foldrT 1)
        , benchIOSink value "foldrTMap" (foldrTMap 1)

        -- Mapping
        , benchIOSink value "map" (mapN serially 1)
        , bench "sequence" $ nfIO $ randomRIO (1, 1000) >>= \n ->
              sequence serially (sourceUnfoldrAction value n)
        , benchIOSink value "mapM" (mapM serially 1)
        , benchIOSink value "tap" (tap 1)
        , benchIOSink value "tapRate 1 second" (tapRate 1)
        , benchIOSink value "pollCounts 1 second" (pollCounts 1)
        , benchIOSink value "timestamped" timestamped

        -- Scanning
        , benchIOSink value "scanl'" (scanl' 1)
        , benchIOSink value "scanl1'" (scanl1' 1)
        , benchIOSink value "scanlM'" (scanlM' 1)
        , benchIOSink value "scanl1M'" (scanl1M' 1)
        , benchIOSink value "postscanl'" (postscanl' 1)
        , benchIOSink value "postscanlM'" (postscanlM' 1)

        , benchIOSink value "scan" (scan 1)
        , benchIOSink value "postscan" (postscan 1)
        ]
    ]

o_1_space_mappingX4 :: Int -> [Benchmark]
o_1_space_mappingX4 value =
    [ bgroup "mappingX4"
        [ benchIOSink value "map" (mapN serially 4)
        , benchIOSink value "mapM" (mapM serially 4)
        , benchIOSink value "trace" (trace 4)

        , benchIOSink value "scanl'" (scanl' 4)
        , benchIOSink value "scanl1'" (scanl1' 4)
        , benchIOSink value "scanlM'" (scanlM' 4)
        , benchIOSink value "scanl1M'" (scanl1M' 4)
        , benchIOSink value "postscanl'" (postscanl' 4)
        , benchIOSink value "postscanlM'" (postscanlM' 4)

        ]
    ]

{-# INLINE sieveScan #-}
sieveScan :: Monad m => SerialT m Int -> SerialT m Int
sieveScan =
      S.mapMaybe snd
    . S.scanlM' (\(primes, _) n -> do
            return $
                let ps = takeWhile (\p -> p * p <= n) primes
                 in if all (\p -> n `mod` p /= 0) ps
                    then (primes ++ [n], Just n)
                    else (primes, Nothing)) (return ([2], Just 2))

o_n_space_mapping :: Int -> [Benchmark]
o_n_space_mapping value =
    [ bgroup "mapping"
        [ benchIO "naive prime sieve"
            (\n -> S.sum $ sieveScan $ S.enumerateFromTo 2 (value + n))
        ]
    ]

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

o_1_space_functor :: Int -> [Benchmark]
o_1_space_functor value =
    [ bgroup "Functor"
        [ benchIOSink value "fmap" (fmapN serially 1)
        , benchIOSink value "fmap x 4" (fmapN serially 4)
        ]
    ]

-------------------------------------------------------------------------------
-- Size reducing transformations (filtering)
-------------------------------------------------------------------------------

{-# INLINE filterEven #-}
filterEven :: MonadIO m => Int -> SerialT m Int -> m ()
filterEven n = composeN n $ S.filter even

{-# INLINE filterAllOut #-}
filterAllOut :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
filterAllOut value n = composeN n $ S.filter (> (value + 1))

{-# INLINE filterAllIn #-}
filterAllIn :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
filterAllIn value n = composeN n $ S.filter (<= (value + 1))

{-# INLINE filterMEven #-}
filterMEven :: MonadIO m => Int -> SerialT m Int -> m ()
filterMEven n = composeN n $ S.filterM (return . even)

{-# INLINE filterMAllOut #-}
filterMAllOut :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
filterMAllOut value n = composeN n $ S.filterM (\x -> return $ x > (value + 1))

{-# INLINE filterMAllIn #-}
filterMAllIn :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
filterMAllIn value n = composeN n $ S.filterM (\x -> return $ x <= (value + 1))

{-# INLINE _takeOne #-}
_takeOne :: MonadIO m => Int -> SerialT m Int -> m ()
_takeOne n = composeN n $ S.take 1

{-# INLINE takeAll #-}
takeAll :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
takeAll value n = composeN n $ S.take (value + 1)

{-# INLINE takeWhileTrue #-}
takeWhileTrue :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
takeWhileTrue value n = composeN n $ S.takeWhile (<= (value + 1))

{-# INLINE takeWhileMTrue #-}
takeWhileMTrue :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
takeWhileMTrue value n = composeN n $ S.takeWhileM (return . (<= (value + 1)))

{-# INLINE takeByTime #-}
takeByTime :: NanoSecond64 -> Int -> SerialT IO Int -> IO ()
takeByTime i n = composeN n (Internal.takeByTime i)

{-# INLINE dropOne #-}
dropOne :: MonadIO m => Int -> SerialT m Int -> m ()
dropOne n = composeN n $ S.drop 1

{-# INLINE dropAll #-}
dropAll :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
dropAll value n = composeN n $ S.drop (value + 1)

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
dropWhileTrue value n = composeN n $ S.dropWhile (<= (value + 1))

{-# INLINE dropWhileMTrue #-}
dropWhileMTrue :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
dropWhileMTrue value n = composeN n $ S.dropWhileM (return . (<= (value + 1)))

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
dropWhileFalse value n = composeN n $ S.dropWhile (> (value + 1))

-- XXX Decide on the time interval
{-# INLINE _intervalsOfSum #-}
_intervalsOfSum :: MonadAsync m => Double -> Int -> SerialT m Int -> m ()
_intervalsOfSum i n = composeN n (S.intervalsOf i FL.sum)

{-# INLINE dropByTime #-}
dropByTime :: NanoSecond64 -> Int -> SerialT IO Int -> IO ()
dropByTime i n = composeN n (Internal.dropByTime i)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropByTime
-- inspect $ 'dropByTime `hasNoType` ''D.Step
#endif

{-# INLINE findIndices #-}
findIndices :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
findIndices value n = composeN n $ S.findIndices (== (value + 1))

{-# INLINE elemIndices #-}
elemIndices :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
elemIndices value n = composeN n $ S.elemIndices (value + 1)

{-# INLINE deleteBy #-}
deleteBy :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
deleteBy value n = composeN n $ S.deleteBy (>=) (value + 1)

-- uniq . uniq == uniq, composeN 2 ~ composeN 1
{-# INLINE uniq #-}
uniq :: MonadIO m => Int -> SerialT m Int -> m ()
uniq n = composeN n S.uniq

{-# INLINE mapMaybe #-}
mapMaybe :: MonadIO m => Int -> SerialT m Int -> m ()
mapMaybe n =
    composeN n $
    S.mapMaybe
        (\x ->
             if odd x
             then Nothing
             else Just x)

{-# INLINE mapMaybeM #-}
mapMaybeM :: S.MonadAsync m => Int -> SerialT m Int -> m ()
mapMaybeM n =
    composeN n $
    S.mapMaybeM
        (\x ->
             if odd x
             then return Nothing
             else return $ Just x)

o_1_space_filtering :: Int -> [Benchmark]
o_1_space_filtering value =
    [ bgroup "filtering"
        [ benchIOSink value "filter-even" (filterEven 1)
        , benchIOSink value "filter-all-out" (filterAllOut value 1)
        , benchIOSink value "filter-all-in" (filterAllIn value 1)

        , benchIOSink value "filterM-even" (filterMEven 1)
        , benchIOSink value "filterM-all-out" (filterMAllOut value 1)
        , benchIOSink value "filterM-all-in" (filterMAllIn value 1)

        -- Trimming
        , benchIOSink value "take-all" (takeAll value 1)
        , benchIOSink
              value
              "takeByTime-all"
              (takeByTime (NanoSecond64 maxBound) 1)
        , benchIOSink value "takeWhile-true" (takeWhileTrue value 1)
     -- , benchIOSink value "takeWhileM-true" (_takeWhileMTrue value 1)
        , benchIOSink value "drop-one" (dropOne 1)
        , benchIOSink value "drop-all" (dropAll value 1)
        , benchIOSink
              value
              "dropByTime-all"
              (dropByTime (NanoSecond64 maxBound) 1)
        , benchIOSink value "dropWhile-true" (dropWhileTrue value 1)
     -- , benchIOSink value "dropWhileM-true" (_dropWhileMTrue value 1)
        , benchIOSink
              value
              "dropWhile-false"
              (dropWhileFalse value 1)
        , benchIOSink value "deleteBy" (deleteBy value 1)

        , benchIOSink value "uniq" (uniq 1)

        -- Map and filter
        , benchIOSink value "mapMaybe" (mapMaybe 1)
        , benchIOSink value "mapMaybeM" (mapMaybeM 1)

        -- Searching (stateful map and filter)
        , benchIOSink value "findIndices" (findIndices value 1)
        , benchIOSink value "elemIndices" (elemIndices value 1)
        ]
    ]

o_1_space_filteringX4 :: Int -> [Benchmark]
o_1_space_filteringX4 value =
    [ bgroup "filteringX4"
        [ benchIOSink value "filter-even" (filterEven 4)
        , benchIOSink value "filter-all-out" (filterAllOut value 4)
        , benchIOSink value "filter-all-in" (filterAllIn value 4)

        , benchIOSink value "filterM-even" (filterMEven 4)
        , benchIOSink value "filterM-all-out" (filterMAllOut value 4)
        , benchIOSink value "filterM-all-in" (filterMAllIn value 4)

        -- trimming
        , benchIOSink value "take-all" (takeAll value 4)
        , benchIOSink value "takeWhile-true" (takeWhileTrue value 4)
        , benchIOSink value "takeWhileM-true" (takeWhileMTrue value 4)
        , benchIOSink value "drop-one" (dropOne 4)
        , benchIOSink value "drop-all" (dropAll value 4)
        , benchIOSink value "dropWhile-true" (dropWhileTrue value 4)
        , benchIOSink value "dropWhileM-true" (dropWhileMTrue value 4)
        , benchIOSink
              value
              "dropWhile-false"
              (dropWhileFalse value 4)
        , benchIOSink value "deleteBy" (deleteBy value 4)

        , benchIOSink value "uniq" (uniq 4)

        -- map and filter
        , benchIOSink value "mapMaybe" (mapMaybe 4)
        , benchIOSink value "mapMaybeM" (mapMaybeM 4)

        -- searching
        , benchIOSink value "findIndices" (findIndices value 4)
        , benchIOSink value "elemIndices" (elemIndices value 4)
        ]
    ]

-------------------------------------------------------------------------------
-- Size increasing transformations (insertions)
-------------------------------------------------------------------------------

{-# INLINE intersperse #-}
intersperse :: S.MonadAsync m => Int -> Int -> SerialT m Int -> m ()
intersperse value n = composeN n $ S.intersperse (value + 1)

{-# INLINE intersperseM #-}
intersperseM :: S.MonadAsync m => Int -> Int -> SerialT m Int -> m ()
intersperseM value n = composeN n $ S.intersperseM (return $ value + 1)

{-# INLINE insertBy #-}
insertBy :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
insertBy value n = composeN n $ S.insertBy compare (value + 1)

o_1_space_inserting :: Int -> [Benchmark]
o_1_space_inserting value =
    [ bgroup "filtering"
        [ benchIOSink value "intersperse" (intersperse value 1)
        , benchIOSink value "intersperseM" (intersperseM value 1)
        , benchIOSink value "insertBy" (insertBy value 1)
        ]
    ]

o_1_space_insertingX4 :: Int -> [Benchmark]
o_1_space_insertingX4 value =
    [ bgroup "insertingX4"
        [ benchIOSink value "intersperse" (intersperse value 4)
        , benchIOSink value "insertBy" (insertBy value 4)
        ]
    ]

-------------------------------------------------------------------------------
-- Indexing
-------------------------------------------------------------------------------

{-# INLINE indexed #-}
indexed :: MonadIO m => Int -> SerialT m Int -> m ()
indexed n = composeN n (S.map snd . S.indexed)

{-# INLINE indexedR #-}
indexedR :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
indexedR value n = composeN n (S.map snd . S.indexedR value)

o_1_space_indexing :: Int -> [Benchmark]
o_1_space_indexing value =
    [ bgroup "indexing"
        [ benchIOSink value "indexed" (indexed 1)
        , benchIOSink value "indexedR" (indexedR value 1)
        ]
    ]

o_1_space_indexingX4 :: Int -> [Benchmark]
o_1_space_indexingX4 value =
    [ bgroup "indexingx4"
        [ benchIOSink value "indexed" (indexed 4)
        , benchIOSink value "indexedR" (indexedR value 4)
        ]
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
benchmarks :: String -> Int -> [Benchmark]
benchmarks moduleName size =
        [ bgroup (o_1_space_prefix moduleName) $ Prelude.concat
            [ o_1_space_functor size
            , o_1_space_mapping size
            , o_1_space_mappingX4 size
            , o_1_space_filtering size
            , o_1_space_filteringX4 size
            , o_1_space_inserting size
            , o_1_space_insertingX4 size
            , o_1_space_indexing size
            , o_1_space_indexingX4 size
            ]
        , bgroup (o_n_space_prefix moduleName) $ Prelude.concat
            [ o_n_space_traversable size
            , o_n_space_mapping size
            ]
        ]
