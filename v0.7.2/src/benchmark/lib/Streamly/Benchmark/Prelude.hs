-- |
-- Module      : Streamly.Benchmark.Prelude
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Streamly.Benchmark.Prelude
    -- TODO: export a single bench group for o_1_space_serial
    ( o_1_space_serial_pure
    , o_1_space_serial_foldable
    , o_1_space_serial_generation
    , o_1_space_serial_elimination
    , o_1_space_serial_foldMultiStream
    , o_1_space_serial_pipes
    , o_1_space_serial_pipesX4
    , o_1_space_serial_transformer
    , o_1_space_serial_transformation
    , o_1_space_serial_transformationX4
    , o_1_space_serial_filtering
    , o_1_space_serial_filteringX4
    , o_1_space_serial_joining
    , o_1_space_serial_concatFoldable
    , o_1_space_serial_concatSerial
    , o_1_space_serial_outerProductStreams
    , o_1_space_serial_mixed
    , o_1_space_serial_mixedX4

    , o_1_space_wSerial_transformation
    , o_1_space_wSerial_concatMap
    , o_1_space_wSerial_outerProduct

    , o_1_space_zipSerial_transformation

    , o_n_space_serial_toList
    , o_n_space_serial_outerProductStreams

    , o_n_space_wSerial_outerProductStreams

    , o_n_space_serial_traversable
    , o_n_space_serial_foldr

    , o_n_heap_serial_foldl
    , o_n_heap_serial_buffering

    , o_n_stack_serial_iterated

    , o_1_space_async_generation
    , o_1_space_async_concatFoldable
    , o_1_space_async_concatMap
    , o_1_space_async_transformation

    , o_1_space_wAsync_generation
    , o_1_space_wAsync_concatFoldable
    , o_1_space_wAsync_concatMap
    , o_1_space_wAsync_transformation

    , o_1_space_ahead_generation
    , o_1_space_ahead_concatFoldable
    , o_1_space_ahead_concatMap
    , o_1_space_ahead_transformation

    , o_1_space_async_zip

    -- TODO: rename to o_n_*
    , o_1_space_parallel_generation
    , o_1_space_parallel_concatFoldable
    , o_1_space_parallel_concatMap
    , o_1_space_parallel_transformation
    , o_1_space_parallel_outerProductStreams
    , o_n_space_parallel_outerProductStreams

    , o_1_space_async_avgRate

    , o_1_space_ahead_avgRate
    ) where

import Control.DeepSeq (NFData(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Strict (StateT, get, put)
import Data.Functor.Identity (Identity, runIdentity)
import Data.IORef (newIORef, modifyIORef')
import GHC.Generics (Generic)
import System.Random (randomRIO)
import Prelude
       (Monad, String, Int, (+), ($), (.), return, even, (>), (<=), (==), (>=),
        subtract, undefined, Maybe(..), Bool, not, (>>=), curry,
        maxBound, div, IO, compare, Double, fromIntegral, Integer, (<$>),
        (<*>), flip, sqrt, round, (*), seq)
import qualified Prelude as P
import qualified Data.Foldable as F
import qualified GHC.Exts as GHC

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection

import qualified Streamly.Internal.Data.Stream.StreamD as D
#endif

import qualified Streamly as S hiding (runStream)
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Prelude as Internal
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.Data.Stream.Parallel as Par
import Streamly.Internal.Data.Time.Units

import qualified Streamly.Internal.Prelude as IP

import qualified Streamly.Benchmark.Prelude.NestedOps as Nested

import Gauge
import Streamly hiding (runStream)
import Streamly.Benchmark.Common

type Stream m a = S.SerialT m a

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

-- enumerate

{-# INLINE sourceIntFromTo #-}
sourceIntFromTo :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceIntFromTo value n = S.enumerateFromTo n (n + value)

{-# INLINE sourceIntFromThenTo #-}
sourceIntFromThenTo :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceIntFromThenTo value n = S.enumerateFromThenTo n (n + 1) (n + value)

{-# INLINE sourceFracFromTo #-}
sourceFracFromTo :: (Monad m, S.IsStream t) => Int -> Int -> t m Double
sourceFracFromTo value n =
    S.enumerateFromTo (fromIntegral n) (fromIntegral (n + value))

{-# INLINE sourceFracFromThenTo #-}
sourceFracFromThenTo :: (Monad m, S.IsStream t) => Int -> Int -> t m Double
sourceFracFromThenTo value n = S.enumerateFromThenTo (fromIntegral n)
    (fromIntegral n + 1.0001) (fromIntegral (n + value))

{-# INLINE sourceIntegerFromStep #-}
sourceIntegerFromStep :: (Monad m, S.IsStream t) => Int -> Int -> t m Integer
sourceIntegerFromStep value n =
    S.take value $ S.enumerateFromThen (fromIntegral n) (fromIntegral n + 1)

-- unfoldr

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceUnfoldr value n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrN #-}
sourceUnfoldrN :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceUnfoldrN upto start = S.unfoldr step start
    where
    step cnt =
        if cnt > start + upto
        then Nothing
        else Just (cnt, cnt + 1)

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

{-# INLINE sourceUnfoldrMN #-}
sourceUnfoldrMN :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrMN upto start = S.unfoldrM step start
    where
    step cnt =
        if cnt > start + upto
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldrMAction #-}
sourceUnfoldrMAction :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (m Int)
sourceUnfoldrMAction value n = S.serially $ S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (return cnt, cnt + 1))

{-# INLINE sourceUnfoldrAction #-}
sourceUnfoldrAction :: (S.IsStream t, Monad m, Monad m1)
    => Int -> Int -> t m (m1 Int)
sourceUnfoldrAction value n = S.serially $ S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else (Just (return cnt, cnt + 1))

-- fromIndices

{-# INLINE _sourceFromIndices #-}
_sourceFromIndices :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
_sourceFromIndices value n = S.take value $ S.fromIndices (+ n)

{-# INLINE _sourceFromIndicesM #-}
_sourceFromIndicesM :: (S.MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
_sourceFromIndicesM value n = S.take value $ S.fromIndicesM (P.fmap return (+ n))

-- fromList

{-# INLINE sourceFromList #-}
sourceFromList :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceFromList value n = S.fromList [n..n+value]

{-# INLINE sourceFromListM #-}
sourceFromListM :: (S.MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
sourceFromListM value n = S.fromListM (P.fmap return [n..n+value])

{-# INLINE sourceIsList #-}
sourceIsList :: Int -> Int -> S.SerialT Identity Int
sourceIsList value n = GHC.fromList [n..n+value]

{-# INLINE sourceIsString #-}
sourceIsString :: Int -> Int -> S.SerialT Identity P.Char
sourceIsString value n = GHC.fromString (P.replicate (n + value) 'a')

-- fromFoldable

{-# INLINE sourceFromFoldable #-}
sourceFromFoldable :: S.IsStream t => Int -> Int -> t m Int
sourceFromFoldable value n = S.fromFoldable [n..n+value]

{-# INLINE sourceFromFoldableM #-}
sourceFromFoldableM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceFromFoldableM value n = S.fromFoldableM (P.fmap return [n..n+value])

{-# INLINE currentTime #-}
currentTime :: (S.IsStream t, S.MonadAsync m)
    => Int -> Double -> Int -> t m AbsTime
currentTime value g _ = S.take value $ Internal.currentTime g

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.drain

{-# INLINE toNull #-}
toNull :: Monad m => (t m a -> S.SerialT m a) -> t m a -> m ()
toNull t = runStream . t

{-# INLINE uncons #-}
uncons :: Monad m => Stream m Int -> m ()
uncons s = do
    r <- S.uncons s
    case r of
        Nothing -> return ()
        Just (_, t) -> uncons t

{-# INLINE init #-}
init :: Monad m => Stream m a -> m ()
init s = S.init s >>= P.mapM_ S.drain

{-# INLINE tail #-}
tail :: Monad m => Stream m a -> m ()
tail s = S.tail s >>= P.mapM_ tail

{-# INLINE nullHeadTail #-}
nullHeadTail :: Monad m => Stream m Int -> m ()
nullHeadTail s = do
    r <- S.null s
    when (not r) $ do
        _ <- S.head s
        S.tail s >>= P.mapM_ nullHeadTail

{-# INLINE mapM_ #-}
mapM_ :: Monad m => Stream m Int -> m ()
mapM_ = S.mapM_ (\_ -> return ())

{-# INLINE toList #-}
toList :: Monad m => Stream m Int -> m [Int]
toList = S.toList

{-# INLINE toListRev #-}
toListRev :: Monad m => Stream m Int -> m [Int]
toListRev = Internal.toListRev

{-# INLINE foldrMElem #-}
foldrMElem :: Monad m => Int -> Stream m Int -> m Bool
foldrMElem e m =
    S.foldrM
        (\x xs ->
             if x == e
                 then return P.True
                 else xs)
        (return P.False)
        m

{-# INLINE foldrMToStream #-}
foldrMToStream :: Monad m => Stream m Int -> m (Stream Identity Int)
foldrMToStream = S.foldr S.cons S.nil

{-# INLINE foldrMBuild #-}
foldrMBuild :: Monad m => Stream m Int -> m [Int]
foldrMBuild = S.foldrM (\x xs -> xs >>= return . (x :)) (return [])

{-# INLINE foldl'Build #-}
foldl'Build :: Monad m => Stream m Int -> m [Int]
foldl'Build = S.foldl' (flip (:)) []

{-# INLINE foldlM'Build #-}
foldlM'Build :: Monad m => Stream m Int -> m [Int]
foldlM'Build = S.foldlM' (\xs x -> return $ x : xs) []

{-# INLINE foldrMReduce #-}
foldrMReduce :: Monad m => Stream m Int -> m Int
foldrMReduce = S.foldrM (\x xs -> xs >>= return . (x +)) (return 0)

{-# INLINE foldl'Reduce #-}
foldl'Reduce :: Monad m => Stream m Int -> m Int
foldl'Reduce = S.foldl' (+) 0

{-# INLINE foldl'ReduceMap #-}
foldl'ReduceMap :: Monad m => Stream m Int -> m Int
foldl'ReduceMap = P.fmap (+ 1) . S.foldl' (+) 0

{-# INLINE foldl1'Reduce #-}
foldl1'Reduce :: Monad m => Stream m Int -> m (Maybe Int)
foldl1'Reduce = S.foldl1' (+)

{-# INLINE foldlM'Reduce #-}
foldlM'Reduce :: Monad m => Stream m Int -> m Int
foldlM'Reduce = S.foldlM' (\xs a -> return $ a + xs) 0

{-# INLINE last #-}
last :: Monad m => Stream m Int -> m (Maybe Int)
last = S.last

{-# INLINE _null #-}
_null :: Monad m => Stream m Int -> m Bool
_null = S.null

{-# INLINE _head #-}
_head :: Monad m => Stream m Int -> m (Maybe Int)
_head = S.head

{-# INLINE elem #-}
elem :: Monad m => Int -> Stream m Int -> m Bool
elem value = S.elem (value + 1)

{-# INLINE notElem #-}
notElem :: Monad m => Int -> Stream m Int -> m Bool
notElem value = S.notElem (value + 1)

{-# INLINE length #-}
length :: Monad m => Stream m Int -> m Int
length = S.length

{-# INLINE all #-}
all :: Monad m => Int -> Stream m Int -> m Bool
all value = S.all (<= (value + 1))

{-# INLINE any #-}
any :: Monad m => Int -> Stream m Int -> m Bool
any value = S.any (> (value + 1))

{-# INLINE and #-}
and :: Monad m => Int -> Stream m Int -> m Bool
and value = S.and . S.map (<= (value + 1))

{-# INLINE or #-}
or :: Monad m => Int -> Stream m Int -> m Bool
or value = S.or . S.map (> (value + 1))

{-# INLINE find #-}
find :: Monad m => Int -> Stream m Int -> m (Maybe Int)
find value = S.find (== (value + 1))

{-# INLINE findIndex #-}
findIndex :: Monad m => Int -> Stream m Int -> m (Maybe Int)
findIndex value = S.findIndex (== (value + 1))

{-# INLINE elemIndex #-}
elemIndex :: Monad m => Int -> Stream m Int -> m (Maybe Int)
elemIndex value = S.elemIndex (value + 1)

{-# INLINE maximum #-}
maximum :: Monad m => Stream m Int -> m (Maybe Int)
maximum = S.maximum

{-# INLINE minimum #-}
minimum :: Monad m => Stream m Int -> m (Maybe Int)
minimum = S.minimum

{-# INLINE sum #-}
sum :: Monad m => Stream m Int -> m Int
sum = S.sum

{-# INLINE product #-}
product :: Monad m => Stream m Int -> m Int
product = S.product

{-# INLINE minimumBy #-}
minimumBy :: Monad m => Stream m Int -> m (Maybe Int)
minimumBy = S.minimumBy compare

{-# INLINE maximumBy #-}
maximumBy :: Monad m => Stream m Int -> m (Maybe Int)
maximumBy = S.maximumBy compare

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Stream m a -> m ()
transform = runStream

{-# INLINE composeN #-}
composeN ::
       MonadIO m
    => Int
    -> (Stream m Int -> Stream m Int)
    -> Stream m Int
    -> m ()
composeN n f =
    case n of
        1 -> transform . f
        2 -> transform . f . f
        3 -> transform . f . f . f
        4 -> transform . f . f . f . f
        _ -> undefined

-- polymorphic stream version of composeN
{-# INLINE composeN' #-}
composeN' ::
       (S.IsStream t, Monad m)
    => Int
    -> (t m Int -> Stream m Int)
    -> t m Int
    -> m ()
composeN' n f =
    case n of
        1 -> transform . f
        2 -> transform . f . S.adapt . f
        3 -> transform . f . S.adapt . f . S.adapt . f
        4 -> transform . f . S.adapt . f . S.adapt . f . S.adapt . f
        _ -> undefined

{-# INLINE scan #-}
scan :: MonadIO m => Int -> Stream m Int -> m ()
scan n = composeN n $ S.scanl' (+) 0

{-# INLINE scanl1' #-}
scanl1' :: MonadIO m => Int -> Stream m Int -> m ()
scanl1' n = composeN n $ S.scanl1' (+)

{-# INLINE fmap #-}
fmap :: MonadIO m => Int -> Stream m Int -> m ()
fmap n = composeN n $ P.fmap (+ 1)

{-# INLINE fmap' #-}
fmap' ::
       (S.IsStream t, S.MonadAsync m, P.Functor (t m))
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
fmap' t n = composeN' n $ t . P.fmap (+ 1)

{-# INLINE map #-}
map :: MonadIO m => Int -> Stream m Int -> m ()
map n = composeN n $ S.map (+ 1)

{-# INLINE map' #-}
map' ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
map' t n = composeN' n $ t . S.map (+ 1)

{-# INLINE mapM #-}
mapM ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
mapM t n = composeN' n $ t . S.mapM return

{-# INLINE tap #-}
tap :: MonadIO m => Int -> Stream m Int -> m ()
tap n = composeN n $ S.tap FL.sum

{-# INLINE tapRate #-}
tapRate :: Int -> Stream IO Int -> IO ()
tapRate n str = do
    cref <- newIORef 0
    composeN n (Internal.tapRate 1 (\c -> modifyIORef' cref (c +))) str

{-# INLINE pollCounts #-}
pollCounts :: Int -> Stream IO Int -> IO ()
pollCounts n str = do
    composeN n (Internal.pollCounts (P.const P.True) f FL.drain) str
  where
    f = Internal.rollingMap (P.-) . Internal.delayPost 1

{-# INLINE tapAsyncS #-}
tapAsyncS :: S.MonadAsync m => Int -> Stream m Int -> m ()
tapAsyncS n = composeN n $ Par.tapAsync S.sum

{-# INLINE tapAsync #-}
tapAsync :: S.MonadAsync m => Int -> Stream m Int -> m ()
tapAsync n = composeN n $ Internal.tapAsync FL.sum

{-# INLINE mapMaybe #-}
mapMaybe :: MonadIO m => Int -> Stream m Int -> m ()
mapMaybe n =
    composeN n $
    S.mapMaybe
        (\x ->
             if P.odd x
                 then Nothing
                 else Just x)

{-# INLINE mapMaybeM #-}
mapMaybeM :: S.MonadAsync m => Int -> Stream m Int -> m ()
mapMaybeM n =
    composeN n $
    S.mapMaybeM
        (\x ->
             if P.odd x
                 then return Nothing
                 else return $ Just x)

{-# INLINE sequence #-}
sequence ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> t m (m Int)
    -> m ()
sequence t = transform . t . S.sequence

{-# INLINE filterEven #-}
filterEven :: MonadIO m => Int -> Stream m Int -> m ()
filterEven n = composeN n $ S.filter even

{-# INLINE filterAllOut #-}
filterAllOut :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterAllOut value n = composeN n $ S.filter (> (value + 1))

{-# INLINE filterAllIn #-}
filterAllIn :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterAllIn value n = composeN n $ S.filter (<= (value + 1))

{-# INLINE _takeOne #-}
_takeOne :: MonadIO m => Int -> Stream m Int -> m ()
_takeOne n = composeN n $ S.take 1

{-# INLINE takeAll #-}
takeAll :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeAll value n = composeN n $ S.take (value + 1)

{-# INLINE takeWhileTrue #-}
takeWhileTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeWhileTrue value n = composeN n $ S.takeWhile (<= (value + 1))

{-# INLINE _takeWhileMTrue #-}
_takeWhileMTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
_takeWhileMTrue value n = composeN n $ S.takeWhileM (return . (<= (value + 1)))

{-# INLINE dropOne #-}
dropOne :: MonadIO m => Int -> Stream m Int -> m ()
dropOne n = composeN n $ S.drop 1

{-# INLINE dropAll #-}
dropAll :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropAll value n = composeN n $ S.drop (value + 1)

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropWhileTrue value n = composeN n $ S.dropWhile (<= (value + 1))

{-# INLINE _dropWhileMTrue #-}
_dropWhileMTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
_dropWhileMTrue value n = composeN n $ S.dropWhileM (return . (<= (value + 1)))

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropWhileFalse value n = composeN n $ S.dropWhile (> (value + 1))

{-# INLINE findIndices #-}
findIndices :: MonadIO m => Int -> Int -> Stream m Int -> m ()
findIndices value n = composeN n $ S.findIndices (== (value + 1))

{-# INLINE elemIndices #-}
elemIndices :: MonadIO m => Int -> Int -> Stream m Int -> m ()
elemIndices value n = composeN n $ S.elemIndices (value + 1)

{-# INLINE intersperse #-}
intersperse :: S.MonadAsync m => Int -> Int -> Stream m Int -> m ()
intersperse value n = composeN n $ S.intersperse (value + 1)

{-# INLINE insertBy #-}
insertBy :: MonadIO m => Int -> Int -> Stream m Int -> m ()
insertBy value n = composeN n $ S.insertBy compare (value + 1)

{-# INLINE deleteBy #-}
deleteBy :: MonadIO m => Int -> Int -> Stream m Int -> m ()
deleteBy value n = composeN n $ S.deleteBy (>=) (value + 1)

{-# INLINE reverse #-}
reverse :: MonadIO m => Int -> Stream m Int -> m ()
reverse n = composeN n $ S.reverse

{-# INLINE reverse' #-}
reverse' :: MonadIO m => Int -> Stream m Int -> m ()
reverse' n = composeN n $ Internal.reverse'

{-# INLINE foldrS #-}
foldrS :: MonadIO m => Int -> Stream m Int -> m ()
foldrS n = composeN n $ Internal.foldrS S.cons S.nil

{-# INLINE foldrSMap #-}
foldrSMap :: MonadIO m => Int -> Stream m Int -> m ()
foldrSMap n = composeN n $ Internal.foldrS (\x xs -> x + 1 `S.cons` xs) S.nil

{-# INLINE foldrT #-}
foldrT :: MonadIO m => Int -> Stream m Int -> m ()
foldrT n = composeN n $ Internal.foldrT S.cons S.nil

{-# INLINE foldrTMap #-}
foldrTMap :: MonadIO m => Int -> Stream m Int -> m ()
foldrTMap n = composeN n $ Internal.foldrT (\x xs -> x + 1 `S.cons` xs) S.nil

{-# INLINE takeByTime #-}
takeByTime :: NanoSecond64 -> Int -> Stream IO Int -> IO ()
takeByTime i n = composeN n (Internal.takeByTime i)

#ifdef INSPECTION
-- inspect $ hasNoType 'takeByTime ''SPEC
inspect $ hasNoTypeClasses 'takeByTime
-- inspect $ 'takeByTime `hasNoType` ''D.Step
#endif

{-# INLINE dropByTime #-}
dropByTime :: NanoSecond64 -> Int -> Stream IO Int -> IO ()
dropByTime i n = composeN n (Internal.dropByTime i)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropByTime
-- inspect $ 'dropByTime `hasNoType` ''D.Step
#endif

-------------------------------------------------------------------------------
-- Pipes
-------------------------------------------------------------------------------

{-# INLINE transformMapM #-}
transformMapM ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
transformMapM t n = composeN' n $ t . Internal.transform (Pipe.mapM return)

{-# INLINE transformComposeMapM #-}
transformComposeMapM ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
transformComposeMapM t n =
    composeN' n $
    t .
    Internal.transform
        (Pipe.mapM (\x -> return (x + 1)) `Pipe.compose`
         Pipe.mapM (\x -> return (x + 2)))

{-# INLINE transformTeeMapM #-}
transformTeeMapM ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
transformTeeMapM t n =
    composeN' n $
    t .
    Internal.transform
        (Pipe.mapM (\x -> return (x + 1)) `Pipe.tee`
         Pipe.mapM (\x -> return (x + 2)))

{-# INLINE transformZipMapM #-}
transformZipMapM ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
transformZipMapM t n =
    composeN' n $
    t .
    Internal.transform
        (Pipe.zipWith
             (+)
             (Pipe.mapM (\x -> return (x + 1)))
             (Pipe.mapM (\x -> return (x + 2))))

-------------------------------------------------------------------------------
-- Mixed Transformation
-------------------------------------------------------------------------------

{-# INLINE scanMap #-}
scanMap :: MonadIO m => Int -> Stream m Int -> m ()
scanMap n = composeN n $ S.map (subtract 1) . S.scanl' (+) 0

{-# INLINE dropMap #-}
dropMap :: MonadIO m => Int -> Stream m Int -> m ()
dropMap n = composeN n $ S.map (subtract 1) . S.drop 1

{-# INLINE dropScan #-}
dropScan :: MonadIO m => Int -> Stream m Int -> m ()
dropScan n = composeN n $ S.scanl' (+) 0 . S.drop 1

{-# INLINE takeDrop #-}
takeDrop :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeDrop value n = composeN n $ S.drop 1 . S.take (value + 1)

{-# INLINE takeScan #-}
takeScan :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeScan value n = composeN n $ S.scanl' (+) 0 . S.take (value + 1)

{-# INLINE takeMap #-}
takeMap :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeMap value n = composeN n $ S.map (subtract 1) . S.take (value + 1)

{-# INLINE filterDrop #-}
filterDrop :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterDrop value n = composeN n $ S.drop 1 . S.filter (<= (value + 1))

{-# INLINE filterTake #-}
filterTake :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterTake value n = composeN n $ S.take (value + 1) . S.filter (<= (value + 1))

{-# INLINE filterScan #-}
filterScan :: MonadIO m => Int -> Stream m Int -> m ()
filterScan n = composeN n $ S.scanl' (+) 0 . S.filter (<= maxBound)

{-# INLINE filterScanl1 #-}
filterScanl1 :: MonadIO m => Int -> Stream m Int -> m ()
filterScanl1 n = composeN n $ S.scanl1' (+) . S.filter (<= maxBound)

{-# INLINE filterMap #-}
filterMap :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterMap value n = composeN n $ S.map (subtract 1) . S.filter (<= (value + 1))

-------------------------------------------------------------------------------
-- Scan and fold
-------------------------------------------------------------------------------

data Pair a b =
    Pair !a !b
    deriving (Generic, NFData)

{-# INLINE sumProductFold #-}
sumProductFold :: Monad m => Stream m Int -> m (Int, Int)
sumProductFold = S.foldl' (\(s, p) x -> (s + x, p P.* x)) (0, 1)

{-# INLINE sumProductScan #-}
sumProductScan :: Monad m => Stream m Int -> m (Pair Int Int)
sumProductScan =
    S.foldl' (\(Pair _ p) (s0, x) -> Pair s0 (p P.* x)) (Pair 0 1) .
    S.scanl' (\(s, _) x -> (s + x, x)) (0, 0)

-------------------------------------------------------------------------------
-- Iteration
-------------------------------------------------------------------------------

{-# INLINE iterStreamLen #-}
iterStreamLen :: Int
iterStreamLen = 10

{-# INLINE maxIters #-}
maxIters :: Int
maxIters = 10000

{-# INLINE iterateSource #-}
iterateSource ::
       S.MonadAsync m
    => (Stream m Int -> Stream m Int)
    -> Int
    -> Int
    -> Stream m Int
iterateSource g i n = f i (sourceUnfoldrMN iterStreamLen n)
  where
    f (0 :: Int) m = g m
    f x m = g (f (x P.- 1) m)

-- this is quadratic
{-# INLINE iterateScan #-}
iterateScan :: S.MonadAsync m => Int -> Stream m Int
iterateScan = iterateSource (S.scanl' (+) 0) (maxIters `div` 10)

-- this is quadratic
{-# INLINE iterateScanl1 #-}
iterateScanl1 :: S.MonadAsync m => Int -> Stream m Int
iterateScanl1 = iterateSource (S.scanl1' (+)) (maxIters `div` 10)

{-# INLINE iterateMapM #-}
iterateMapM :: S.MonadAsync m => Int -> Stream m Int
iterateMapM = iterateSource (S.mapM return) maxIters

{-# INLINE iterateFilterEven #-}
iterateFilterEven :: S.MonadAsync m => Int -> Stream m Int
iterateFilterEven = iterateSource (S.filter even) maxIters

{-# INLINE iterateTakeAll #-}
iterateTakeAll :: S.MonadAsync m => Int -> Int -> Stream m Int
iterateTakeAll value = iterateSource (S.take (value + 1)) maxIters

{-# INLINE iterateDropOne #-}
iterateDropOne :: S.MonadAsync m => Int -> Stream m Int
iterateDropOne = iterateSource (S.drop 1) maxIters

{-# INLINE iterateDropWhileFalse #-}
iterateDropWhileFalse :: S.MonadAsync m => Int -> Int -> Stream m Int
iterateDropWhileFalse value =
    iterateSource (S.dropWhile (> (value + 1))) maxIters

{-# INLINE iterateDropWhileTrue #-}
iterateDropWhileTrue :: S.MonadAsync m => Int -> Int -> Stream m Int
iterateDropWhileTrue value =
    iterateSource (S.dropWhile (<= (value + 1))) maxIters

-------------------------------------------------------------------------------
-- Combining streams
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Appending
-------------------------------------------------------------------------------

{-# INLINE serial2 #-}
serial2 :: Int -> Int -> IO ()
serial2 count n =
    S.drain $ S.serial (sourceUnfoldrMN count n) (sourceUnfoldrMN count (n + 1))

{-# INLINE serial4 #-}
serial4 :: Int -> Int -> IO ()
serial4 count n =
    S.drain $
    S.serial
        ((S.serial (sourceUnfoldrMN count n) (sourceUnfoldrMN count (n + 1))))
        ((S.serial
              (sourceUnfoldrMN count (n + 2))
              (sourceUnfoldrMN count (n + 3))))

{-# INLINE append2 #-}
append2 :: Int -> Int -> IO ()
append2 count n =
    S.drain $
    Internal.append (sourceUnfoldrMN count n) (sourceUnfoldrMN count (n + 1))

{-# INLINE append4 #-}
append4 :: Int -> Int -> IO ()
append4 count n =
    S.drain $
    Internal.append
        ((Internal.append
              (sourceUnfoldrMN count n)
              (sourceUnfoldrMN count (n + 1))))
        ((Internal.append
              (sourceUnfoldrMN count (n + 2))
              (sourceUnfoldrMN count (n + 3))))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'append2
inspect $ 'append2 `hasNoType` ''SPEC
inspect $ 'append2 `hasNoType` ''D.AppendState
#endif

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------

{-# INLINE wSerial2 #-}
wSerial2 :: Int -> Int -> IO ()
wSerial2 value n =
    S.drain $
    S.wSerial
        (sourceUnfoldrMN (value `div` 2) n)
        (sourceUnfoldrMN (value `div` 2) (n + 1))

{-# INLINE interleave2 #-}
interleave2 :: Int -> Int -> IO ()
interleave2 value n =
    S.drain $
    Internal.interleave
        (sourceUnfoldrMN (value `div` 2) n)
        (sourceUnfoldrMN (value `div` 2) (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'interleave2
inspect $ 'interleave2 `hasNoType` ''SPEC
inspect $ 'interleave2 `hasNoType` ''D.InterleaveState
#endif

{-# INLINE roundRobin2 #-}
roundRobin2 :: Int -> Int -> IO ()
roundRobin2 value n =
    S.drain $
    Internal.roundrobin
        (sourceUnfoldrMN (value `div` 2) n)
        (sourceUnfoldrMN (value `div` 2) (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'roundRobin2
inspect $ 'roundRobin2 `hasNoType` ''SPEC
inspect $ 'roundRobin2 `hasNoType` ''D.InterleaveState
#endif

-------------------------------------------------------------------------------
-- Merging
-------------------------------------------------------------------------------

{-# INLINE mergeBy #-}
mergeBy :: Int -> Int -> IO ()
mergeBy count n =
    S.drain $
    S.mergeBy
        P.compare
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mergeBy
inspect $ 'mergeBy `hasNoType` ''SPEC
inspect $ 'mergeBy `hasNoType` ''D.Step
#endif

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

{-# INLINE zip #-}
zip :: Int -> Int -> IO ()
zip count n =
    S.drain $
    S.zipWith (,) (sourceUnfoldrMN count n) (sourceUnfoldrMN count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'zip
inspect $ 'zip `hasNoType` ''SPEC
inspect $ 'zip `hasNoType` ''D.Step
#endif

{-# INLINE zipM #-}
zipM :: Int -> Int -> IO ()
zipM count n =
    S.drain $
    S.zipWithM
        (curry return)
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'zipM
inspect $ 'zipM `hasNoType` ''SPEC
inspect $ 'zipM `hasNoType` ''D.Step
#endif

{-# INLINE zipAsync #-}
zipAsync :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (Int, Int)
zipAsync count n = do
    S.zipAsyncWith (,) (sourceUnfoldrMN count n) (sourceUnfoldrMN count (n + 1))

{-# INLINE zipAsyncM #-}
zipAsyncM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (Int, Int)
zipAsyncM count n = do
    S.zipAsyncWithM
        (curry return)
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

{-# INLINE zipAsyncAp #-}
zipAsyncAp :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (Int, Int)
zipAsyncAp count n = do
    S.zipAsyncly $
        (,) <$> (sourceUnfoldrMN count n) <*> (sourceUnfoldrMN count (n + 1))

{-# INLINE mergeAsyncByM #-}
mergeAsyncByM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
mergeAsyncByM count n = do
    S.mergeAsyncByM
        (\a b -> return (a `compare` b))
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

{-# INLINE mergeAsyncBy #-}
mergeAsyncBy :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
mergeAsyncBy count n = do
    S.mergeAsyncBy
        compare
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

-------------------------------------------------------------------------------
-- Multi-stream folds
-------------------------------------------------------------------------------

{-# INLINE isPrefixOf #-}
isPrefixOf :: Monad m => Stream m Int -> m Bool
isPrefixOf src = S.isPrefixOf src src

{-# INLINE isSubsequenceOf #-}
isSubsequenceOf :: Monad m => Stream m Int -> m Bool
isSubsequenceOf src = S.isSubsequenceOf src src

{-# INLINE stripPrefix #-}
stripPrefix :: Monad m => Stream m Int -> m ()
stripPrefix src = do
    _ <- S.stripPrefix src src
    return ()

{-# INLINE eqBy' #-}
eqBy' :: (Monad m, P.Eq a) => Stream m a -> m P.Bool
eqBy' src = S.eqBy (==) src src

{-# INLINE eqBy #-}
eqBy :: Int -> Int -> IO Bool
eqBy value n = eqBy' (source value n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqBy
inspect $ 'eqBy `hasNoType` ''SPEC
inspect $ 'eqBy `hasNoType` ''D.Step
#endif


{-# INLINE eqByPure #-}
eqByPure :: Int -> Int -> Identity Bool
eqByPure value n = eqBy' (sourceUnfoldr value n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqByPure
inspect $ 'eqByPure `hasNoType` ''SPEC
inspect $ 'eqByPure `hasNoType` ''D.Step
#endif

{-# INLINE cmpBy' #-}
cmpBy' :: (Monad m, P.Ord a) => Stream m a -> m P.Ordering
cmpBy' src = S.cmpBy P.compare src src

{-# INLINE cmpBy #-}
cmpBy :: Int -> Int -> IO P.Ordering
cmpBy value n = cmpBy' (source value n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cmpBy
inspect $ 'cmpBy `hasNoType` ''SPEC
inspect $ 'cmpBy `hasNoType` ''D.Step
#endif

{-# INLINE cmpByPure #-}
cmpByPure :: Int -> Int -> Identity P.Ordering
cmpByPure value n = cmpBy' (sourceUnfoldr value n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cmpByPure
inspect $ 'cmpByPure `hasNoType` ''SPEC
inspect $ 'cmpByPure `hasNoType` ''D.Step
#endif

-------------------------------------------------------------------------------
-- Streams of streams
-------------------------------------------------------------------------------

-- Special cases of concatMap

{-# INLINE sourceFoldMapWith #-}
sourceFoldMapWith :: (S.IsStream t, S.Semigroup (t m Int))
                  => Int -> Int -> t m Int
sourceFoldMapWith value n = S.foldMapWith (S.<>) S.yield [n..n+value]

{-# INLINE sourceFoldMapWithM #-}
sourceFoldMapWithM :: (S.IsStream t, Monad m, S.Semigroup (t m Int))
                   => Int -> Int -> t m Int
sourceFoldMapWithM value n = S.foldMapWith (S.<>) (S.yieldM . return) [n..n+value]

{-# INLINE sourceFoldMapM #-}
sourceFoldMapM :: (S.IsStream t, Monad m, P.Monoid (t m Int))
               => Int -> Int -> t m Int
sourceFoldMapM value n = F.foldMap (S.yieldM . return) [n..n+value]

{-# INLINE sourceConcatMapId #-}
sourceConcatMapId :: (S.IsStream t, Monad m)
                  => Int -> Int -> t m Int
sourceConcatMapId value n =
    S.concatMap P.id $ S.fromFoldable $ P.map (S.yieldM . return) [n..n+value]

-- concatMap unfoldrM/unfoldrM

{-# INLINE concatMap #-}
concatMap :: Int -> Int -> Int -> IO ()
concatMap outer inner n =
    S.drain $ S.concatMap
        (\_ -> sourceUnfoldrMN inner n)
        (sourceUnfoldrMN outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap
inspect $ 'concatMap `hasNoType` ''SPEC
#endif

-- concatMap unfoldr/unfoldr

{-# INLINE concatMapPure #-}
concatMapPure :: Int -> Int -> Int -> IO ()
concatMapPure outer inner n =
    S.drain $ S.concatMap
        (\_ -> sourceUnfoldrN inner n)
        (sourceUnfoldrN outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapPure
inspect $ 'concatMapPure `hasNoType` ''SPEC
#endif

-- concatMap replicate/unfoldrM

{-# INLINE concatMapRepl4xN #-}
concatMapRepl4xN :: Int -> Int -> IO ()
concatMapRepl4xN value n = S.drain $ S.concatMap (S.replicate 4)
                          (sourceUnfoldrMN (value `div` 4) n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapRepl4xN
inspect $ 'concatMapRepl4xN `hasNoType` ''SPEC
#endif

-- concatMapWith

{-# INLINE concatStreamsWith #-}
concatStreamsWith
    :: (forall c. S.SerialT IO c -> S.SerialT IO c -> S.SerialT IO c)
    -> Int
    -> Int
    -> Int
    -> IO ()
concatStreamsWith op outer inner n =
    S.drain $ S.concatMapWith op
        (\i -> sourceUnfoldrMN inner i)
        (sourceUnfoldrMN outer n)

{-# INLINE concatMapWithSerial #-}
concatMapWithSerial :: Int -> Int -> Int -> IO ()
concatMapWithSerial = concatStreamsWith S.serial

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithSerial
inspect $ 'concatMapWithSerial `hasNoType` ''SPEC
#endif

{-# INLINE concatMapWithAppend #-}
concatMapWithAppend :: Int -> Int -> Int -> IO ()
concatMapWithAppend = concatStreamsWith Internal.append

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithAppend
inspect $ 'concatMapWithAppend `hasNoType` ''SPEC
#endif

{-# INLINE concatMapWithWSerial #-}
concatMapWithWSerial :: Int -> Int -> Int -> IO ()
concatMapWithWSerial = concatStreamsWith S.wSerial

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithWSerial
inspect $ 'concatMapWithSerial `hasNoType` ''SPEC
#endif

-- concatUnfold

-- concatUnfold replicate/unfoldrM

{-# INLINE concatUnfoldRepl4xN #-}
concatUnfoldRepl4xN :: Int -> Int -> IO ()
concatUnfoldRepl4xN value n =
    S.drain $ S.concatUnfold
        (UF.replicateM 4)
        (sourceUnfoldrMN (value `div` 4) n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatUnfoldRepl4xN
inspect $ 'concatUnfoldRepl4xN `hasNoType` ''D.ConcatMapUState
inspect $ 'concatUnfoldRepl4xN `hasNoType` ''SPEC
#endif

{-# INLINE concatUnfoldInterleaveRepl4xN #-}
concatUnfoldInterleaveRepl4xN :: Int -> Int -> IO ()
concatUnfoldInterleaveRepl4xN value n =
    S.drain $ Internal.concatUnfoldInterleave
        (UF.replicateM 4)
        (sourceUnfoldrMN (value `div` 4) n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatUnfoldInterleaveRepl4xN
-- inspect $ 'concatUnfoldInterleaveRepl4xN `hasNoType` ''SPEC
-- inspect $ 'concatUnfoldInterleaveRepl4xN `hasNoType` ''D.ConcatUnfoldInterleaveState
#endif

{-# INLINE concatUnfoldRoundrobinRepl4xN #-}
concatUnfoldRoundrobinRepl4xN :: Int -> Int -> IO ()
concatUnfoldRoundrobinRepl4xN value n =
    S.drain $ Internal.concatUnfoldRoundrobin
        (UF.replicateM 4)
        (sourceUnfoldrMN (value `div` 4) n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatUnfoldRoundrobinRepl4xN
-- inspect $ 'concatUnfoldRoundrobinRepl4xN `hasNoType` ''SPEC
-- inspect $ 'concatUnfoldRoundrobinRepl4xN `hasNoType` ''D.ConcatUnfoldInterleaveState
#endif

-------------------------------------------------------------------------------
-- Monad transformation (hoisting etc.)
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldrState #-}
sourceUnfoldrState :: (S.IsStream t, S.MonadAsync m)
                   => Int -> Int -> t (StateT Int m) Int
sourceUnfoldrState value n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else do
            s <- get
            put (s + 1)
            return (Just (s, cnt + 1))

{-# INLINE evalStateT #-}
evalStateT :: S.MonadAsync m => Int -> Int -> Stream m Int
evalStateT value n = Internal.evalStateT 0 (sourceUnfoldrState value n)

{-# INLINE withState #-}
withState :: S.MonadAsync m => Int -> Int -> Stream m Int
withState value n =
    Internal.evalStateT (0 :: Int) (Internal.liftInner (sourceUnfoldrM value n))

-------------------------------------------------------------------------------
-- Concurrent application/fold
-------------------------------------------------------------------------------

{-# INLINE parAppMap #-}
parAppMap :: S.MonadAsync m => Stream m Int -> m ()
parAppMap src = S.drain $ S.map (+1) S.|$ src

{-# INLINE parAppSum #-}
parAppSum :: S.MonadAsync m => Stream m Int -> m ()
parAppSum src = (S.sum S.|$. src) >>= \x -> P.seq x (return ())

-------------------------------------------------------------------------------
-- Type class instances
-------------------------------------------------------------------------------

{-# INLINE eqInstance #-}
eqInstance :: Stream Identity Int -> Bool
eqInstance src = src == src

{-# INLINE eqInstanceNotEq #-}
eqInstanceNotEq :: Stream Identity Int -> Bool
eqInstanceNotEq src = src P./= src

{-# INLINE ordInstance #-}
ordInstance :: Stream Identity Int -> Bool
ordInstance src = src P.< src

{-# INLINE ordInstanceMin #-}
ordInstanceMin :: Stream Identity Int -> Stream Identity Int
ordInstanceMin src = P.min src src

{-# INLINE showInstance #-}
showInstance :: Stream Identity Int -> P.String
showInstance src = P.show src

{-# INLINE showInstanceList #-}
showInstanceList :: [Int] -> P.String
showInstanceList src = P.show src

{-# INLINE readInstance #-}
readInstance :: P.String -> Stream Identity Int
readInstance str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "readInstance: no parse"

{-# INLINE readInstanceList #-}
readInstanceList :: P.String -> [Int]
readInstanceList str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "readInstance: no parse"

-------------------------------------------------------------------------------
-- Pure (Identity) streams
-------------------------------------------------------------------------------

{-# INLINE pureFoldl' #-}
pureFoldl' :: Stream Identity Int -> Int
pureFoldl' = runIdentity . S.foldl' (+) 0

-------------------------------------------------------------------------------
-- Foldable Instance
-------------------------------------------------------------------------------

{-# INLINE foldableFoldl' #-}
foldableFoldl' :: Int -> Int -> Int
foldableFoldl' value n =
    F.foldl' (+) 0 (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableFoldrElem #-}
foldableFoldrElem :: Int -> Int -> Bool
foldableFoldrElem value n =
    F.foldr (\x xs -> if x == value then P.True else xs)
            (P.False)
            (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableSum #-}
foldableSum :: Int -> Int -> Int
foldableSum value n =
    P.sum (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableProduct #-}
foldableProduct :: Int -> Int -> Int
foldableProduct value n =
    P.product (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE _foldableNull #-}
_foldableNull :: Int -> Int -> Bool
_foldableNull value n =
    P.null (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableElem #-}
foldableElem :: Int -> Int -> Bool
foldableElem value n =
    P.elem value (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableNotElem #-}
foldableNotElem :: Int -> Int -> Bool
foldableNotElem value n =
    P.notElem value (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableFind #-}
foldableFind :: Int -> Int -> Maybe Int
foldableFind value n =
    F.find (== (value + 1)) (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableAll #-}
foldableAll :: Int -> Int -> Bool
foldableAll value n =
    P.all (<= (value + 1)) (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableAny #-}
foldableAny :: Int -> Int -> Bool
foldableAny value n =
    P.any (> (value + 1)) (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableAnd #-}
foldableAnd :: Int -> Int -> Bool
foldableAnd value n =
    P.and $ S.map (<= (value + 1)) (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableOr #-}
foldableOr :: Int -> Int -> Bool
foldableOr value n =
    P.or $ S.map (> (value + 1)) (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableLength #-}
foldableLength :: Int -> Int -> Int
foldableLength value n =
    P.length (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableMin #-}
foldableMin :: Int -> Int -> Int
foldableMin value n =
    P.minimum (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableMax #-}
foldableMax :: Int -> Int -> Int
foldableMax value n =
    P.maximum (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableMinBy #-}
foldableMinBy :: Int -> Int -> Int
foldableMinBy value n =
    F.minimumBy compare (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableListMinBy #-}
foldableListMinBy :: Int -> Int -> Int
foldableListMinBy value n = F.minimumBy compare [1..value+n]

{-# INLINE foldableMaxBy #-}
foldableMaxBy :: Int -> Int -> Int
foldableMaxBy value n =
    F.maximumBy compare (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableToList #-}
foldableToList :: Int -> Int -> [Int]
foldableToList value n =
    F.toList (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableMapM_ #-}
foldableMapM_ :: Monad m => Int -> Int -> m ()
foldableMapM_ value n =
    F.mapM_ (\_ -> return ()) (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableSequence_ #-}
foldableSequence_ :: Int -> Int -> IO ()
foldableSequence_ value n =
    F.sequence_ (sourceUnfoldrAction value n :: S.SerialT Identity (IO Int))

{-# INLINE _foldableMsum #-}
_foldableMsum :: Int -> Int -> IO Int
_foldableMsum value n =
    F.msum (sourceUnfoldrAction value n :: S.SerialT Identity (IO Int))

-------------------------------------------------------------------------------
-- Traversable Instance
-------------------------------------------------------------------------------

{-# INLINE traversableTraverse #-}
traversableTraverse :: Stream Identity Int -> IO (Stream Identity Int)
traversableTraverse = P.traverse return

{-# INLINE traversableSequenceA #-}
traversableSequenceA :: Stream Identity Int -> IO (Stream Identity Int)
traversableSequenceA = P.sequenceA . P.fmap return

{-# INLINE traversableMapM #-}
traversableMapM :: Stream Identity Int -> IO (Stream Identity Int)
traversableMapM = P.mapM return

{-# INLINE traversableSequence #-}
traversableSequence :: Stream Identity Int -> IO (Stream Identity Int)
traversableSequence = P.sequence . P.fmap return

-------------------------------------------------------------------------------
-- Benchmark groups
-------------------------------------------------------------------------------

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: (IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f = bench name $ nfIO $ randomRIO (1,1) >>= f . source value

{-# INLINE benchHoistSink #-}
benchHoistSink
    :: (IsStream t, NFData b)
    => Int -> String -> (t Identity Int -> IO b) -> Benchmark
benchHoistSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f .  sourceUnfoldr value

-- XXX We should be using sourceUnfoldrM for fair comparison with IO monad, but
-- we can't use it as it requires MonadAsync constraint.
{-# INLINE benchIdentitySink #-}
benchIdentitySink
    :: (IsStream t, NFData b)
    => Int -> String -> (t Identity Int -> Identity b) -> Benchmark
benchIdentitySink value name f = bench name $ nf (f . sourceUnfoldr value) 1

-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchIOSrc #-}
benchIOSrc
    :: (t IO a -> SerialT IO a)
    -> String
    -> (Int -> t IO a)
    -> Benchmark
benchIOSrc t name f =
    bench name $ nfIO $ randomRIO (1,1) >>= toNull t . f

{-# INLINE benchPureSink #-}
benchPureSink :: NFData b => Int -> String -> (SerialT Identity Int -> b) -> Benchmark
benchPureSink value name f = benchPure name (sourceUnfoldr value) f

{-# INLINE benchPureSinkIO #-}
benchPureSinkIO
    :: NFData b
    => Int -> String -> (SerialT Identity Int -> IO b) -> Benchmark
benchPureSinkIO value name f =
    bench name $ nfIO $ randomRIO (1, 1) >>= f . sourceUnfoldr value

{-# INLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchSrcIO #-}
benchSrcIO
    :: (t IO a -> SerialT IO a)
    -> String
    -> (Int -> t IO a)
    -> Benchmark
benchSrcIO t name f
    = bench name $ nfIO $ randomRIO (1,1) >>= toNull t . f

{-# INLINE benchMonadicSrcIO #-}
benchMonadicSrcIO :: String -> (Int -> IO ()) -> Benchmark
benchMonadicSrcIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f


-------------------------------------------------------------------------------
-- Serial : O(1) Space
-------------------------------------------------------------------------------

o_1_space_serial_pure :: Int -> [Benchmark]
o_1_space_serial_pure value =
    [ bgroup
          "serially"
          [ bgroup
                "pure"
                [ benchPureSink value "id" P.id
                , benchPureSink1 "eqBy" (eqByPure value)
                , benchPureSink value "==" eqInstance
                , benchPureSink value "/=" eqInstanceNotEq
                , benchPureSink1 "cmpBy" (cmpByPure value)
                , benchPureSink value "<" ordInstance
                , benchPureSink value "min" ordInstanceMin
                , benchPureSrc "IsList.fromList" (sourceIsList value)
            -- length is used to check for foldr/build fusion
                , benchPureSink
                      value
                      "length . IsList.toList"
                      (P.length . GHC.toList)
                , benchPureSrc "IsString.fromString" (sourceIsString value)
                , benchPureSink value "showsPrec pure streams" showInstance
                , benchPureSink value "foldl'" pureFoldl'
                ]
          ]
    ]

o_1_space_serial_foldable :: Int -> [Benchmark]
o_1_space_serial_foldable value =
    [ bgroup
          "serially"
          [ bgroup
                "foldable"
              -- Foldable instance
              -- type class operations
                [ bench "foldl'" $ nf (foldableFoldl' value) 1
                , bench "foldrElem" $ nf (foldableFoldrElem value) 1
            -- , bench "null" $ nf (_foldableNull value) 1
                , bench "elem" $ nf (foldableElem value) 1
                , bench "length" $ nf (foldableLength value) 1
                , bench "sum" $ nf (foldableSum value) 1
                , bench "product" $ nf (foldableProduct value) 1
                , bench "minimum" $ nf (foldableMin value) 1
                , bench "maximum" $ nf (foldableMax value) 1
                , bench "length . toList" $
                  nf (P.length . foldableToList value) 1
            -- folds
                , bench "notElem" $ nf (foldableNotElem value) 1
                , bench "find" $ nf (foldableFind value) 1
                , bench "all" $ nf (foldableAll value) 1
                , bench "any" $ nf (foldableAny value) 1
                , bench "and" $ nf (foldableAnd value) 1
                , bench "or" $ nf (foldableOr value) 1
            -- Note: minimumBy/maximumBy do not work in constant memory they are in
            -- the O(n) group of benchmarks down below in this file.
            -- Applicative and Traversable operations
            -- TBD: traverse_
                , benchIOSink1 "mapM_" (foldableMapM_ value)
            -- TBD: for_
            -- TBD: forM_
                , benchIOSink1 "sequence_" (foldableSequence_ value)
            -- TBD: sequenceA_
            -- TBD: asum
            -- , benchIOSink1 "msum" (_foldableMsum value)
                ]
          ]
    ]

o_1_space_serial_generation :: Int -> [Benchmark]
o_1_space_serial_generation value =
    [ bgroup
          "serially"
          [ bgroup
                "generation"
              -- Most basic, barely stream continuations running
                [ benchIOSrc serially "unfoldr" (sourceUnfoldr value)
                , benchIOSrc serially "unfoldrM" (sourceUnfoldrM value)
                , benchIOSrc serially "intFromTo" (sourceIntFromTo value)
                , benchIOSrc
                      serially
                      "intFromThenTo"
                      (sourceIntFromThenTo value)
                , benchIOSrc
                      serially
                      "integerFromStep"
                      (sourceIntegerFromStep value)
                , benchIOSrc
                      serially
                      "fracFromThenTo"
                      (sourceFracFromThenTo value)
                , benchIOSrc serially "fracFromTo" (sourceFracFromTo value)
                , benchIOSrc serially "fromList" (sourceFromList value)
                , benchIOSrc serially "fromListM" (sourceFromListM value)
            -- These are essentially cons and consM
                , benchIOSrc
                      serially
                      "fromFoldable"
                      (sourceFromFoldable value)
                , benchIOSrc
                      serially
                      "fromFoldableM"
                      (sourceFromFoldableM value)
                , benchIOSrc serially "currentTime/0.00001s" $
                  currentTime value 0.00001
                ]
          ]
    ]

o_1_space_serial_elimination :: Int -> [Benchmark]
o_1_space_serial_elimination value =
    [ bgroup
          "serially"
          [ bgroup
                "elimination"
                [ bgroup
                      "reduce"
                      [ bgroup
                            "IO"
                            [ benchIOSink value "foldl'" foldl'Reduce
                            , benchIOSink value "foldl1'" foldl1'Reduce
                            , benchIOSink value "foldlM'" foldlM'Reduce
                            ]
                      , bgroup
                            "Identity"
                            [ benchIdentitySink value "foldl'" foldl'Reduce
                            , benchIdentitySink
                                  value
                                  "foldl1'"
                                  foldl1'Reduce
                            , benchIdentitySink
                                  value
                                  "foldlM'"
                                  foldlM'Reduce
                            ]
                      ]
                , bgroup
                      "build"
                      [ bgroup
                            "IO"
                            [ benchIOSink
                                  value
                                  "foldrMElem"
                                  (foldrMElem value)
                            ]
                      , bgroup
                            "Identity"
                            [ benchIdentitySink
                                  value
                                  "foldrMElem"
                                  (foldrMElem value)
                            , benchIdentitySink
                                  value
                                  "foldrMToStreamLength"
                                  (S.length . runIdentity . foldrMToStream)
                            , benchPureSink
                                  value
                                  "foldrMToListLength"
                                  (P.length . runIdentity . foldrMBuild)
                            ]
                      ]
                , benchIOSink value "uncons" uncons
                , benchIOSink value "toNull" $ toNull serially
                , benchIOSink value "mapM_" mapM_
                , benchIOSink value "init" init
            -- this is too low and causes all benchmarks reported in ns
            -- , benchIOSink value "head" head
                , benchIOSink value "last" last
            -- , benchIOSink value "lookup" lookup
                , benchIOSink value "find" (find value)
                , benchIOSink value "findIndex" (findIndex value)
                , benchIOSink value "elemIndex" (elemIndex value)
            -- this is too low and causes all benchmarks reported in ns
            -- , benchIOSink value "null" null
                , benchIOSink value "elem" (elem value)
                , benchIOSink value "notElem" (notElem value)
                , benchIOSink value "all" (all value)
                , benchIOSink value "any" (any value)
                , benchIOSink value "and" (and value)
                , benchIOSink value "or" (or value)
                , benchIOSink value "length" length
                , benchHoistSink
                      value
                      "length . generally"
                      (length . IP.generally)
                , benchIOSink value "sum" sum
                , benchIOSink value "product" product
                , benchIOSink value "maximumBy" maximumBy
                , benchIOSink value "maximum" maximum
                , benchIOSink value "minimumBy" minimumBy
                , benchIOSink value "minimum" minimum
                ]
          ]
    ]

o_1_space_serial_foldMultiStream :: Int -> [Benchmark]
o_1_space_serial_foldMultiStream value =
    [ bgroup
          "serially"
          [ bgroup
                "fold-multi-stream"
                [ benchIOSink1 "eqBy" (eqBy value)
                , benchIOSink1 "cmpBy" (cmpBy value)
                , benchIOSink value "isPrefixOf" isPrefixOf
                , benchIOSink value "isSubsequenceOf" isSubsequenceOf
                , benchIOSink value "stripPrefix" stripPrefix
                ]
          ]
    ]

o_1_space_serial_pipes :: Int -> [Benchmark]
o_1_space_serial_pipes value =
    [ bgroup
          "serially"
          [ bgroup
                "pipes"
                [ benchIOSink value "mapM" (transformMapM serially 1)
                , benchIOSink
                      value
                      "compose"
                      (transformComposeMapM serially 1)
                , benchIOSink value "tee" (transformTeeMapM serially 1)
                , benchIOSink value "zip" (transformZipMapM serially 1)
                ]
          ]
    ]

o_1_space_serial_pipesX4 :: Int -> [Benchmark]
o_1_space_serial_pipesX4 value =
    [ bgroup
          "serially"
          [ bgroup
                "pipesX4"
                [ benchIOSink value "mapM" (transformMapM serially 4)
                , benchIOSink
                      value
                      "compose"
                      (transformComposeMapM serially 4)
                , benchIOSink value "tee" (transformTeeMapM serially 4)
                , benchIOSink value "zip" (transformZipMapM serially 4)
                ]
          ]
    ]


o_1_space_serial_transformer :: Int -> [Benchmark]
o_1_space_serial_transformer value =
    [ bgroup
          "serially"
          [ bgroup
                "transformer"
                [ benchIOSrc serially "evalState" (evalStateT value)
                , benchIOSrc serially "withState" (withState value)
                ]
          ]
    ]

o_1_space_serial_transformation :: Int -> [Benchmark]
o_1_space_serial_transformation value =
    [ bgroup
          "serially"
          [ bgroup
                "transformation"
                [ benchIOSink value "scanl" (scan 1)
                , benchIOSink value "scanl1'" (scanl1' 1)
                , benchIOSink value "map" (map 1)
                , benchIOSink value "fmap" (fmap 1)
                , benchIOSink value "mapM" (mapM serially 1)
                , benchIOSink value "mapMaybe" (mapMaybe 1)
                , benchIOSink value "mapMaybeM" (mapMaybeM 1)
                , bench "sequence" $
                  nfIO $
                  randomRIO (1, 1000) >>= \n ->
                      sequence serially (sourceUnfoldrMAction value n)
                , benchIOSink value "findIndices" (findIndices value 1)
                , benchIOSink value "elemIndices" (elemIndices value 1)
                , benchIOSink value "foldrS" (foldrS 1)
                , benchIOSink value "foldrSMap" (foldrSMap 1)
                , benchIOSink value "foldrT" (foldrT 1)
                , benchIOSink value "foldrTMap" (foldrTMap 1)
                , benchIOSink value "tap" (tap 1)
                , benchIOSink value "tapRate 1 second" (tapRate 1)
                , benchIOSink value "pollCounts 1 second" (pollCounts 1)
                , benchIOSink value "tapAsync" (tapAsync 1)
                , benchIOSink value "tapAsyncS" (tapAsyncS 1)
                ]
          ]
    ]

o_1_space_serial_transformationX4 :: Int -> [Benchmark]
o_1_space_serial_transformationX4 value =
    [ bgroup
          "serially"
          [ bgroup
                "transformationX4"
                [ benchIOSink value "scan" (scan 4)
                , benchIOSink value "scanl1'" (scanl1' 4)
                , benchIOSink value "map" (map 4)
                , benchIOSink value "fmap" (fmap 4)
                , benchIOSink value "mapM" (mapM serially 4)
                , benchIOSink value "mapMaybe" (mapMaybe 4)
                , benchIOSink value "mapMaybeM" (mapMaybeM 4)
            -- , bench "sequence" $ nfIO $ randomRIO (1,1000) >>= \n ->
                -- sequence serially (sourceUnfoldrMAction n)
                , benchIOSink value "findIndices" (findIndices value 4)
                , benchIOSink value "elemIndices" (elemIndices value 4)
                ]
          ]
    ]

o_1_space_serial_filtering :: Int -> [Benchmark]
o_1_space_serial_filtering value =
    [ bgroup
          "serially"
          [ bgroup
                "filtering"
                [ benchIOSink value "filter-even" (filterEven 1)
                , benchIOSink value "filter-all-out" (filterAllOut value 1)
                , benchIOSink value "filter-all-in" (filterAllIn value 1)
                , benchIOSink value "take-all" (takeAll value 1)
                , benchIOSink
                      value
                      "takeByTime-all"
                      (takeByTime (NanoSecond64 maxBound) 1)
                , benchIOSink value "takeWhile-true" (takeWhileTrue value 1)
            --, benchIOSink value "takeWhileM-true" (_takeWhileMTrue 1)
            -- "drop-one" is dual to "last"
                , benchIOSink value "drop-one" (dropOne 1)
                , benchIOSink value "drop-all" (dropAll value 1)
                , benchIOSink
                      value
                      "dropByTime-all"
                      (dropByTime (NanoSecond64 maxBound) 1)
                , benchIOSink value "dropWhile-true" (dropWhileTrue value 1)
            --, benchIOSink value "dropWhileM-true" (_dropWhileMTrue 1)
                , benchIOSink
                      value
                      "dropWhile-false"
                      (dropWhileFalse value 1)
                , benchIOSink value "deleteBy" (deleteBy value 1)
                , benchIOSink value "intersperse" (intersperse value 1)
                , benchIOSink value "insertBy" (insertBy value 1)
                ]
          ]
    ]

o_1_space_serial_filteringX4 :: Int -> [Benchmark]
o_1_space_serial_filteringX4 value =
    [ bgroup
          "serially"
          [ bgroup
                "filteringX4"
                [ benchIOSink value "filter-even" (filterEven 4)
                , benchIOSink value "filter-all-out" (filterAllOut value 4)
                , benchIOSink value "filter-all-in" (filterAllIn value 4)
                , benchIOSink value "take-all" (takeAll value 4)
                , benchIOSink value "takeWhile-true" (takeWhileTrue value 4)
            --, benchIOSink value "takeWhileM-true" (_takeWhileMTrue 4)
                , benchIOSink value "drop-one" (dropOne 4)
                , benchIOSink value "drop-all" (dropAll value 4)
                , benchIOSink value "dropWhile-true" (dropWhileTrue value 4)
            --, benchIOSink value "dropWhileM-true" (_dropWhileMTrue 4)
                , benchIOSink
                      value
                      "dropWhile-false"
                      (dropWhileFalse value 4)
                , benchIOSink value "deleteBy" (deleteBy value 4)
                , benchIOSink value "intersperse" (intersperse value 4)
                , benchIOSink value "insertBy" (insertBy value 4)
                ]
          ]
    ]

o_1_space_serial_joining :: Int -> [Benchmark]
o_1_space_serial_joining value =
    [ bgroup
          "serially"
          [ bgroup
                "joining"
                [ benchIOSrc1 "zip (2,x/2)" (zip (value `div` 2))
                , benchIOSrc1 "zipM (2,x/2)" (zipM (value `div` 2))
                , benchIOSrc1 "mergeBy (2,x/2)" (mergeBy (value `div` 2))
                , benchIOSrc1 "serial (2,x/2)" (serial2 (value `div` 2))
                , benchIOSrc1 "append (2,x/2)" (append2 (value `div` 2))
                , benchIOSrc1 "serial (2,2,x/4)" (serial4 (value `div` 4))
                , benchIOSrc1 "append (2,2,x/4)" (append4 (value `div` 4))
                , benchIOSrc1 "wSerial (2,x/2)" (wSerial2 value) -- XXX Move this elsewhere?
                , benchIOSrc1 "interleave (2,x/2)" (interleave2 value)
                , benchIOSrc1 "roundRobin (2,x/2)" (roundRobin2 value)
                ]
          ]
    ]

o_1_space_serial_concatFoldable :: Int -> [Benchmark]
o_1_space_serial_concatFoldable value =
    [ bgroup
          "serially"
          [ bgroup
                "concat-foldable"
                [ benchIOSrc
                      serially
                      "foldMapWith"
                      (sourceFoldMapWith value)
                , benchIOSrc
                      serially
                      "foldMapWithM"
                      (sourceFoldMapWithM value)
                , benchIOSrc serially "foldMapM" (sourceFoldMapM value)
                , benchIOSrc
                      serially
                      "foldWithConcatMapId"
                      (sourceConcatMapId value)
                ]
          ]
    ]

o_1_space_serial_concatSerial :: Int -> [Benchmark]
o_1_space_serial_concatSerial value =
    [ bgroup
          "serially"
          [ bgroup
                "concat-serial"
                [ benchIOSrc1
                      "concatMapPure (2,x/2)"
                      (concatMapPure 2 (value `div` 2))
                , benchIOSrc1
                      "concatMap (2,x/2)"
                      (concatMap 2 (value `div` 2))
                , benchIOSrc1
                      "concatMap (x/2,2)"
                      (concatMap (value `div` 2) 2)
                , benchIOSrc1
                      "concatMapRepl (x/4,4)"
                      (concatMapRepl4xN value)
                , benchIOSrc1
                      "concatUnfoldRepl (x/4,4)"
                      (concatUnfoldRepl4xN value)
                , benchIOSrc1
                      "concatMapWithSerial (2,x/2)"
                      (concatMapWithSerial 2 (value `div` 2))
                , benchIOSrc1
                      "concatMapWithSerial (x/2,2)"
                      (concatMapWithSerial (value `div` 2) 2)
                , benchIOSrc1
                      "concatMapWithAppend (2,x/2)"
                      (concatMapWithAppend 2 (value `div` 2))
                ]
          ]
    ]

o_1_space_serial_outerProductStreams :: Int -> [Benchmark]
o_1_space_serial_outerProductStreams value =
    [ bgroup
          "serially"
          [ bgroup
                "outer-product-streams"
                [ benchIO "toNullAp" $ Nested.toNullAp value serially
                , benchIO "toNull" $ Nested.toNull value serially
                , benchIO "toNull3" $ Nested.toNull3 value serially
                , benchIO "filterAllOut" $ Nested.filterAllOut value serially
                , benchIO "filterAllIn" $ Nested.filterAllIn value serially
                , benchIO "filterSome" $ Nested.filterSome value serially
                , benchIO "breakAfterSome" $
                  Nested.breakAfterSome value serially
                ]
          ]
    ]

o_1_space_serial_mixed :: Int -> [Benchmark]
o_1_space_serial_mixed value =
    [ bgroup
          "serially"
          -- scanl-map and foldl-map are equivalent to the scan and fold in the foldl
          -- library. If scan/fold followed by a map is efficient enough we may not
          -- need monolithic implementations of these.
          [ bgroup
                "mixed"
                [ benchIOSink value "scanl-map" (scanMap 1)
                , benchIOSink value "foldl-map" foldl'ReduceMap
                , benchIOSink value "sum-product-fold" sumProductFold
                , benchIOSink value "sum-product-scan" sumProductScan
                ]
          ]
    ]

o_1_space_serial_mixedX4 :: Int -> [Benchmark]
o_1_space_serial_mixedX4 value =
    [ bgroup
          "serially"
          [ bgroup
                "mixedX4"
                [ benchIOSink value "scan-map" (scanMap 4)
                , benchIOSink value "drop-map" (dropMap 4)
                , benchIOSink value "drop-scan" (dropScan 4)
                , benchIOSink value "take-drop" (takeDrop value 4)
                , benchIOSink value "take-scan" (takeScan value 4)
                , benchIOSink value "take-map" (takeMap value 4)
                , benchIOSink value "filter-drop" (filterDrop value 4)
                , benchIOSink value "filter-take" (filterTake value 4)
                , benchIOSink value "filter-scan" (filterScan 4)
                , benchIOSink value "filter-scanl1" (filterScanl1 4)
                , benchIOSink value "filter-map" (filterMap value 4)
                ]
          ]
    ]

o_1_space_wSerial_transformation :: Int -> [Benchmark]
o_1_space_wSerial_transformation value =
    [ bgroup
          "wSerially"
          [ bgroup
                "transformation"
                [benchIOSink value "fmap" $ fmap' wSerially 1]
          ]
    ]

o_1_space_wSerial_concatMap :: Int -> [Benchmark]
o_1_space_wSerial_concatMap value =
    [ bgroup
          "wSerially"
          [ bgroup
                "concatMap"
                [ benchIOSrc1
                      "concatMapWithWSerial (2,x/2)"
                      (concatMapWithWSerial 2 (value `div` 2))
                , benchIOSrc1
                      "concatMapWithWSerial (x/2,2)"
                      (concatMapWithWSerial (value `div` 2) 2)
                ]
          ]
    ]

o_1_space_wSerial_outerProduct :: Int -> [Benchmark]
o_1_space_wSerial_outerProduct value =
    [ bgroup
          "wSerially"
          [ bgroup
                "outer-product"
                [ benchIO "toNullAp" $ Nested.toNullAp value wSerially
                , benchIO "toNull" $ Nested.toNull value wSerially
                , benchIO "toNull3" $ Nested.toNull3 value wSerially
                , benchIO "filterAllOut" $ Nested.filterAllOut value wSerially
                , benchIO "filterAllIn" $ Nested.filterAllIn value wSerially
                , benchIO "filterSome" $ Nested.filterSome value wSerially
                , benchIO "breakAfterSome" $
                  Nested.breakAfterSome value wSerially
                ]
          ]
    ]

o_1_space_zipSerial_transformation :: Int -> [Benchmark]
o_1_space_zipSerial_transformation value =
    [ bgroup
          "zipSerially"
          [ bgroup
                "transformation"
                [benchIOSink value "fmap" $ fmap' zipSerially 1]
            -- XXX needs fixing
            {-
          , bgroup "outer-product"
            [ benchIO "toNullAp"  $ Nested.toNullAp value  zipSerially
            ]
            -}
          ]
    ]

-------------------------------------------------------------------------------
-- Serial : O(n) Space
-------------------------------------------------------------------------------

o_n_space_serial_toList :: Int -> [Benchmark]
o_n_space_serial_toList value =
    [ bgroup
          "serially"
          [ bgroup
                "toList" -- < 2MB
          -- Converting the stream to a list or pure stream in a strict monad
                [ benchIOSink value "foldrMToList" foldrMBuild
                , benchIOSink value "toList" toList
                , benchIOSink value "toListRev" toListRev
          -- , benchIOSink value "toPure" toPure
          -- , benchIOSink value "toPureRev" toPureRev
                ]
          ]
    ]

o_n_space_serial_outerProductStreams :: Int -> [Benchmark]
o_n_space_serial_outerProductStreams value =
    [ bgroup
          "serially"
          [ bgroup
                "outer-product-streams"
                [ benchIO "toList" $ Nested.toList value serially
                , benchIO "toListSome" $ Nested.toListSome value serially
                ]
          ]
    ]

o_n_space_wSerial_outerProductStreams :: Int -> [Benchmark]
o_n_space_wSerial_outerProductStreams value =
    [ bgroup
          "wSerially"
          [ bgroup
                "outer-product-streams"
                [ benchIO "toList" $ Nested.toList value wSerially
                , benchIO "toListSome" $ Nested.toListSome value wSerially
                ]
          ]
    ]

o_n_space_serial_traversable :: Int -> [Benchmark]
o_n_space_serial_traversable value =
    [ bgroup
          "serially"
        -- Buffering operations using heap proportional to number of elements.
          [ bgroup
                "traversable" -- < 2MB
            -- Traversable instance
                [ benchPureSinkIO value "traverse" traversableTraverse
                , benchPureSinkIO value "sequenceA" traversableSequenceA
                , benchPureSinkIO value "mapM" traversableMapM
                , benchPureSinkIO value "sequence" traversableSequence
                ]
          ]
    ]

o_n_space_serial_foldr :: Int -> [Benchmark]
o_n_space_serial_foldr value =
    [ bgroup
          "serially"
        -- Head recursive strict right folds.
          [ bgroup
                "foldr"
            -- < 2MB
          -- accumulation due to strictness of IO monad
                [ benchIOSink value "foldrM/build/IO" foldrMBuild
          -- Right folds for reducing are inherently non-streaming as the
          -- expression needs to be fully built before it can be reduced.
                , benchIdentitySink
                      value
                      "foldrM/reduce/Identity"
                      foldrMReduce
          -- takes < 4MB
                , benchIOSink value "foldrM/reduce/IO" foldrMReduce
          -- XXX the definitions of minimumBy and maximumBy in Data.Foldable use
          -- foldl1 which does not work in constant memory for our implementation.
          -- It works in constant memory for lists but even for lists it takes 15x
          -- more time compared to our foldl' based implementation.
          -- XXX these take < 16M stack space
                , bench "minimumBy" $ nf (flip foldableMinBy 1) value
                , bench "maximumBy" $ nf (flip foldableMaxBy 1) value
                , bench "minimumByList" $ nf (flip foldableListMinBy 1) value
                ]
          ]
    ]


o_n_heap_serial_foldl :: Int -> [Benchmark]
o_n_heap_serial_foldl value =
    [ bgroup
          "serially"
          [ bgroup
                "foldl"
          -- Left folds for building a structure are inherently non-streaming
          -- as the structure cannot be lazily consumed until fully built.
                [ benchIOSink value "foldl'/build/IO" foldl'Build
                , benchIdentitySink value "foldl'/build/Identity" foldl'Build
                , benchIOSink value "foldlM'/build/IO" foldlM'Build
                , benchIdentitySink
                      value
                      "foldlM'/build/Identity"
                      foldlM'Build
          -- Reversing/sorting a stream
                , benchIOSink value "reverse" (reverse 1)
                , benchIOSink value "reverse'" (reverse' 1)
                ]
          ]
    ]

o_n_heap_serial_buffering :: Int -> [Benchmark]
o_n_heap_serial_buffering value =
    [ bgroup
          "serially"
          [ bgroup
                "buffering"
            -- Buffers the output of show/read.
            -- XXX can the outputs be streaming? Can we have special read/show
            -- style type classes, readM/showM supporting streaming effects?
                [ bench "readsPrec pure streams" $
                  nf readInstance (mkString value)
                , bench "readsPrec Haskell lists" $
                  nf readInstanceList (mkListString value)
                , bench "showPrec Haskell lists" $
                  nf showInstanceList (mkList value)
          -- interleave x/4 streams of 4 elements each. Needs to buffer
          -- proportional to x/4. This is different from WSerial because
          -- WSerial expands slowly because of binary interleave behavior and
          -- this expands immediately because of Nary interleave behavior.
                , benchIOSrc1
                      "concatUnfoldInterleaveRepl (x/4,4)"
                      (concatUnfoldInterleaveRepl4xN value)
                , benchIOSrc1
                      "concatUnfoldRoundrobinRepl (x/4,4)"
                      (concatUnfoldRoundrobinRepl4xN value)
                ]
          ]
    ]

-- Head recursive operations.
o_n_stack_serial_iterated :: Int -> [Benchmark]
o_n_stack_serial_iterated value =
    [ bgroup
          "serially"
          [ bgroup
                "iterated"
                [ benchIOSrc serially "mapMx10K" iterateMapM
                , benchIOSrc serially "scanx100" iterateScan
                , benchIOSrc serially "scanl1x100" iterateScanl1
                , benchIOSrc serially "filterEvenx10K" iterateFilterEven
                , benchIOSrc serially "takeAllx10K" (iterateTakeAll value)
                , benchIOSrc serially "dropOnex10K" iterateDropOne
                , benchIOSrc
                      serially
                      "dropWhileFalsex10K"
                      (iterateDropWhileFalse value)
                , benchIOSrc
                      serially
                      "dropWhileTruex10K"
                      (iterateDropWhileTrue value)
                , benchIOSink value "tail" tail
                , benchIOSink value "nullHeadTail" nullHeadTail
                ]
          ]
    ]

o_1_space_async_generation :: Int -> [Benchmark]
o_1_space_async_generation value =
    [ bgroup
          "asyncly"
          [ bgroup
                "generation"
                [ benchSrcIO asyncly "unfoldr" (sourceUnfoldr value)
                , benchSrcIO asyncly "unfoldrM" (sourceUnfoldrM value)
                , benchSrcIO asyncly "fromFoldable" (sourceFromFoldable value)
                , benchSrcIO asyncly "fromFoldableM" (sourceFromFoldableM value)
                , benchSrcIO
                      asyncly
                      "unfoldrM maxThreads 1"
                      (maxThreads 1 . sourceUnfoldrM value)
                , benchSrcIO
                      asyncly
                      "unfoldrM maxBuffer 1 (x/10 ops)"
                      (maxBuffer 1 . sourceUnfoldrMN (value `div` 10))
                ]
          ]
    ]

o_1_space_async_concatFoldable :: Int -> [Benchmark]
o_1_space_async_concatFoldable value =
    [ bgroup
          "asyncly"
          [ bgroup
                "concat-foldable"
                [ benchSrcIO asyncly "foldMapWith" (sourceFoldMapWith value)
                , benchSrcIO
                      asyncly
                      "foldMapWithM"
                      (sourceFoldMapWithM value)
                , benchSrcIO asyncly "foldMapM" (sourceFoldMapM value)
                ]
          ]
    ]

o_1_space_async_concatMap :: Int -> [Benchmark]
o_1_space_async_concatMap value =
    value2 `seq`
    [ bgroup
          "asyncly"
          [ bgroup
                "concatMap"
                [ benchMonadicSrcIO
                      "concatMapWith (2,x/2)"
                      (concatStreamsWith async 2 (value `div` 2))
                , benchMonadicSrcIO
                      "concatMapWith (sqrt x,sqrt x)"
                      (concatStreamsWith async value2 value2)
                , benchMonadicSrcIO
                      "concatMapWith (sqrt x * 2,sqrt x / 2)"
                      (concatStreamsWith async (value2 * 2) (value2 `div` 2))
                ]
          ]
    ]
  where
    value2 = round $ sqrt $ (fromIntegral value :: Double)

o_1_space_async_transformation :: Int -> [Benchmark]
o_1_space_async_transformation value =
    [ bgroup
          "asyncly"
          [ bgroup
                "transformation"
                [ benchIOSink value "map" $ map' asyncly 1
                , benchIOSink value "fmap" $ fmap' asyncly 1
                , benchIOSink value "mapM" $ mapM asyncly 1
                ]
          ]
    ]

o_1_space_wAsync_generation :: Int -> [Benchmark]
o_1_space_wAsync_generation value =
    [ bgroup
          "wAsyncly"
          [ bgroup
                "generation"
                [ benchSrcIO wAsyncly "unfoldr" (sourceUnfoldr value)
                , benchSrcIO wAsyncly "unfoldrM" (sourceUnfoldrM value)
                , benchSrcIO wAsyncly "fromFoldable" (sourceFromFoldable value)
                , benchSrcIO
                      wAsyncly
                      "fromFoldableM"
                      (sourceFromFoldableM value)
                , benchSrcIO
                      wAsyncly
                      "unfoldrM maxThreads 1"
                      (maxThreads 1 . sourceUnfoldrM value)
                , benchSrcIO
                      wAsyncly
                      "unfoldrM maxBuffer 1 (x/10 ops)"
                      (maxBuffer 1 . sourceUnfoldrMN (value `div` 10))
                ]
          ]
    ]

o_1_space_wAsync_concatFoldable :: Int -> [Benchmark]
o_1_space_wAsync_concatFoldable value =
    [ bgroup
          "wAsyncly"
          [ bgroup
                "concat-foldable"
                [ benchSrcIO wAsyncly "foldMapWith" (sourceFoldMapWith value)
                , benchSrcIO wAsyncly "foldMapWithM" (sourceFoldMapWithM value)
                , benchSrcIO wAsyncly "foldMapM" (sourceFoldMapM value)
                ]
          ]
    ]

-- When we merge streams using wAsync the size of the queue increases
-- slowly because of the binary composition adding just one more item
-- to the work queue only after every scheduling pass through the
-- work queue.
--
-- We should see the memory consumption increasing slowly if these
-- benchmarks are left to run on infinite number of streams of infinite
-- sizes.
o_1_space_wAsync_concatMap :: Int -> [Benchmark]
o_1_space_wAsync_concatMap value =
    value2 `seq`
    [ bgroup
          "wAsyncly"
          [ benchMonadicSrcIO
                "concatMapWith (2,x/2)"
                (concatStreamsWith wAsync 2 (value `div` 2))
          , benchMonadicSrcIO
                "concatMapWith (sqrt x,sqrt x)"
                (concatStreamsWith wAsync value2 value2)
          , benchMonadicSrcIO
                "concatMapWith (sqrt x * 2,sqrt x / 2)"
                (concatStreamsWith wAsync (value2 * 2) (value2 `div` 2))
          ]
    ]
  where
    value2 = round $ sqrt $ (fromIntegral value :: Double)

o_1_space_wAsync_transformation :: Int -> [Benchmark]
o_1_space_wAsync_transformation value =
    [ bgroup
          "wAsyncly"
          [ bgroup
                "transformation"
                [ benchIOSink value "map" $ map' wAsyncly 1
                , benchIOSink value "fmap" $ fmap' wAsyncly 1
                , benchIOSink value "mapM" $ mapM wAsyncly 1
                ]
          ]
    ]

-- unfoldr and fromFoldable are always serial and therefore the same for
-- all stream types. They can be removed to reduce the number of benchmarks.
o_1_space_ahead_generation :: Int -> [Benchmark]
o_1_space_ahead_generation value =
    [ bgroup
          "aheadly"
          [ bgroup
                "generation"
                [ benchSrcIO aheadly "unfoldr" (sourceUnfoldr value)
                , benchSrcIO aheadly "unfoldrM" (sourceUnfoldrM value)
--                , benchSrcIO aheadly "fromFoldable" (sourceFromFoldable value)
                , benchSrcIO
                      aheadly
                      "fromFoldableM"
                      (sourceFromFoldableM value)
                , benchSrcIO
                      aheadly
                      "unfoldrM maxThreads 1"
                      (maxThreads 1 . sourceUnfoldrM value)
                , benchSrcIO
                      aheadly
                      "unfoldrM maxBuffer 1 (x/10 ops)"
                      (maxBuffer 1 . sourceUnfoldrMN (value `div` 10))
                ]
          ]
    ]

o_1_space_ahead_concatFoldable :: Int -> [Benchmark]
o_1_space_ahead_concatFoldable value =
    [ bgroup
          "aheadly"
          [ bgroup
                "concat-foldable"
                [ benchSrcIO aheadly "foldMapWith" (sourceFoldMapWith value)
                , benchSrcIO aheadly "foldMapWithM" (sourceFoldMapWithM value)
                , benchSrcIO aheadly "foldMapM" (sourceFoldMapM value)
                ]
          ]
    ]

o_1_space_ahead_concatMap :: Int -> [Benchmark]
o_1_space_ahead_concatMap value =
    value2 `seq`
    [ bgroup
          "aheadly"
          [ benchMonadicSrcIO
                "concatMapWith (2,x/2)"
                (concatStreamsWith ahead 2 (value `div` 2))
          , benchMonadicSrcIO
                "concatMapWith (sqrt x,sqrt x)"
                (concatStreamsWith ahead value2 value2)
          , benchMonadicSrcIO
                "concatMapWith (sqrt x * 2,sqrt x / 2)"
                (concatStreamsWith ahead (value2 * 2) (value2 `div` 2))
          ]
    ]
  where
    value2 = round $ sqrt $ (fromIntegral value :: Double)


o_1_space_ahead_transformation :: Int -> [Benchmark]
o_1_space_ahead_transformation value =
    [ bgroup
          "aheadly"
          [ bgroup
                "transformation"
                [ benchIOSink value "map" $ map' aheadly 1
                , benchIOSink value "fmap" $ fmap' aheadly 1
                , benchIOSink value "mapM" $ mapM aheadly 1
                ]
          ]
    ]

o_1_space_async_zip :: Int -> [Benchmark]
o_1_space_async_zip value =
    [ bgroup
          "asyncly"
          [ bgroup
                "zip"
                [ benchSrcIO
                      serially
                      "zipAsync (2,x/2)"
                      (zipAsync (value `div` 2))
                , benchSrcIO
                      serially
                      "zipAsyncM (2,x/2)"
                      (zipAsyncM (value `div` 2))
                , benchSrcIO
                      serially
                      "zipAsyncAp (2,x/2)"
                      (zipAsyncAp (value `div` 2))
                , benchIOSink value "fmap zipAsyncly" $ fmap' S.zipAsyncly 1
                , benchSrcIO
                      serially
                      "mergeAsyncBy (2,x/2)"
                      (mergeAsyncBy (value `div` 2))
                , benchSrcIO
                      serially
                      "mergeAsyncByM (2,x/2)"
                      (mergeAsyncByM (value `div` 2))
        -- Parallel stages in a pipeline
                , benchIOSink value "parAppMap" parAppMap
                , benchIOSink value "parAppSum" parAppSum
                ]
          ]
    ]

o_1_space_parallel_generation :: Int -> [Benchmark]
o_1_space_parallel_generation value =
    [ bgroup
          "parallely"
          [ bgroup
                "generation"
                [ benchSrcIO parallely "unfoldr" (sourceUnfoldr value)
                , benchSrcIO parallely "unfoldrM" (sourceUnfoldrM value)
--                , benchSrcIO parallely "fromFoldable" (sourceFromFoldable value)
                , benchSrcIO
                      parallely
                      "fromFoldableM"
                      (sourceFromFoldableM value)
                , benchSrcIO
                      parallely
                      "unfoldrM maxThreads 1"
                      (maxThreads 1 . sourceUnfoldrM value)
                , benchSrcIO
                      parallely
                      "unfoldrM maxBuffer 1 (x/10 ops)"
                      (maxBuffer 1 . sourceUnfoldrMN (value `div` 10))
                ]
          ]
    ]

o_1_space_parallel_concatFoldable :: Int -> [Benchmark]
o_1_space_parallel_concatFoldable value =
    [ bgroup
          "parallely"
          [ bgroup
                "concat-foldable"
                [ benchSrcIO parallely "foldMapWith" (sourceFoldMapWith value)
                , benchSrcIO parallely "foldMapWithM" (sourceFoldMapWithM value)
                , benchSrcIO parallely "foldMapM" (sourceFoldMapM value)
                ]
          ]
    ]

o_1_space_parallel_concatMap :: Int -> [Benchmark]
o_1_space_parallel_concatMap value =
    value2 `seq`
    [ bgroup
          "parallely"
          [ benchMonadicSrcIO
                "concatMapWith (2,x/2)"
                (concatStreamsWith parallel 2 (value `div` 2))
          , benchMonadicSrcIO
                "concatMapWith (sqrt x,sqrt x)"
                (concatStreamsWith parallel value2 value2)
          , benchMonadicSrcIO
                "concatMapWith (sqrt x * 2,sqrt x / 2)"
                (concatStreamsWith parallel (value2 * 2) (value2 `div` 2))
          ]
    ]
  where
    value2 = round $ sqrt $ (fromIntegral value :: Double)


o_1_space_parallel_transformation :: Int -> [Benchmark]
o_1_space_parallel_transformation value =
    [ bgroup
          "parallely"
          [ bgroup
                "transformation"
                [ benchIOSink value "map" $ map' parallely 1
                , benchIOSink value "fmap" $ fmap' parallely 1
                , benchIOSink value "mapM" $ mapM parallely 1
                ]
          ]
    ]

o_1_space_parallel_outerProductStreams :: Int -> [Benchmark]
o_1_space_parallel_outerProductStreams value =
    [ bgroup
          "parallely"
          [ bgroup
                "outer-product-streams"
                [ benchIO "toNullAp" $ Nested.toNullAp value parallely
                , benchIO "toNull" $ Nested.toNull value parallely
                , benchIO "toNull3" $ Nested.toNull3 value parallely
                , benchIO "filterAllOut" $ Nested.filterAllOut value parallely
                , benchIO "filterAllIn" $ Nested.filterAllIn value parallely
                , benchIO "filterSome" $ Nested.filterSome value parallely
                , benchIO "breakAfterSome" $
                  Nested.breakAfterSome value parallely
                ]
          ]
    ]

o_n_space_parallel_outerProductStreams :: Int -> [Benchmark]
o_n_space_parallel_outerProductStreams value =
    [ bgroup
          "parallely"
          [ bgroup
                "outer-product-streams"
                [ benchIO "toList" $ Nested.toList value parallely
                , benchIO "toListSome" $ Nested.toListSome value parallely
                ]
          ]
    ]

-- XXX arbitrarily large rate should be the same as rate Nothing
o_1_space_async_avgRate :: Int -> [Benchmark]
o_1_space_async_avgRate value =
    [ bgroup
          "asyncly"
          [ bgroup
                "avgRate"
          -- benchIO "unfoldr" $ toNull asyncly
          -- benchSrcIO asyncly "unfoldrM" (sourceUnfoldrM value)
                [ benchSrcIO
                      asyncly
                      "unfoldrM/Nothing"
                      (S.rate Nothing . sourceUnfoldrM value)
                , benchSrcIO
                      asyncly
                      "unfoldrM/1,000,000"
                      (S.avgRate 1000000 . sourceUnfoldrM value)
                , benchSrcIO
                      asyncly
                      "unfoldrM/3,000,000"
                      (S.avgRate 3000000 . sourceUnfoldrM value)
                , benchSrcIO
                      asyncly
                      "unfoldrM/10,000,000/maxThreads1"
                      (maxThreads 1 .
                       S.avgRate 10000000 . sourceUnfoldrM value)
                , benchSrcIO
                      asyncly
                      "unfoldrM/10,000,000"
                      (S.avgRate 10000000 . sourceUnfoldrM value)
                , benchSrcIO
                      asyncly
                      "unfoldrM/20,000,000"
                      (S.avgRate 20000000 . sourceUnfoldrM value)
                ]
          ]
    ]

o_1_space_ahead_avgRate :: Int -> [Benchmark]
o_1_space_ahead_avgRate value =
    [ bgroup
          "aheadly"
          [ bgroup
                "avgRate"
                [ benchSrcIO
                      aheadly
                      "unfoldrM/1,000,000"
                      (S.avgRate 1000000 . sourceUnfoldrM value)
                ]
          ]
    ]
