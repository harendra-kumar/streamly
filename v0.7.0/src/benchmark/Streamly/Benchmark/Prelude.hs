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

module Streamly.Benchmark.Prelude where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Strict (StateT, get, put)
import Data.Functor.Identity (Identity, runIdentity)
import Data.IORef (newIORef, modifyIORef')
import GHC.Generics (Generic)
import Prelude
       (Monad, Int, (+), ($), (.), return, fmap, even, (>), (<=), (==), (>=),
        subtract, undefined, Maybe(..), odd, Bool, not, (>>=), mapM_, curry,
        maxBound, div, IO, compare, Double, fromIntegral, Integer, (<$>),
        (<*>), flip)
import qualified Prelude as P
import qualified Data.Foldable as F
import qualified GHC.Exts as GHC

#ifdef INSPECTION
import Test.Inspection

import qualified Streamly.Internal.Data.Stream.StreamD as D
#endif

import qualified Streamly          as S hiding (runStream)
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Prelude as Internal
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.Data.Stream.Parallel as Par
import Streamly.Internal.Data.Time.Units

-- To detect memory leak issues use larger streams.
-- See Note in .cabal file on how to generate streams of specific size.

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

-- fromIndices

{-# INLINE sourceFromIndices #-}
sourceFromIndices :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceFromIndices value n = S.take value $ S.fromIndices (+ n)

{-# INLINE sourceFromIndicesM #-}
sourceFromIndicesM :: (S.MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
sourceFromIndicesM value n = S.take value $ S.fromIndicesM (Prelude.fmap return (+ n))

-- fromList

{-# INLINE sourceFromList #-}
sourceFromList :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceFromList value n = S.fromList [n..n+value]

{-# INLINE sourceFromListM #-}
sourceFromListM :: (S.MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
sourceFromListM value n = S.fromListM (Prelude.fmap return [n..n+value])

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
sourceFromFoldableM value n = S.fromFoldableM (Prelude.fmap return [n..n+value])

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

{-# INLINE toList #-}
toList :: Monad m => Stream m Int -> m [Int]

{-# INLINE head #-}
{-# INLINE last #-}
{-# INLINE maximum #-}
{-# INLINE minimum #-}
{-# INLINE find #-}
{-# INLINE findIndex #-}
{-# INLINE elemIndex #-}
{-# INLINE foldl1'Reduce #-}
head, last, minimum, maximum, foldl1'Reduce
    :: Monad m => Stream m Int -> m (Maybe Int)

find, findIndex, elemIndex
    :: Monad m => Int -> Stream m Int -> m (Maybe Int)

{-# INLINE minimumBy #-}
{-# INLINE maximumBy #-}
minimumBy, maximumBy :: Monad m => Stream m Int -> m (Maybe Int)

{-# INLINE foldl'Reduce #-}
{-# INLINE foldl'ReduceMap #-}
{-# INLINE foldlM'Reduce #-}
{-# INLINE foldrMReduce #-}
{-# INLINE length #-}
{-# INLINE sum #-}
{-# INLINE product #-}
foldl'Reduce, foldl'ReduceMap, foldlM'Reduce, foldrMReduce, length, sum, product
    :: Monad m
    => Stream m Int -> m Int

{-# INLINE foldl'Build #-}
{-# INLINE foldlM'Build #-}
{-# INLINE foldrMBuild #-}
foldrMBuild, foldl'Build, foldlM'Build
    :: Monad m
    => Stream m Int -> m [Int]

{-# INLINE all #-}
{-# INLINE any #-}
{-# INLINE and #-}
{-# INLINE or #-}
{-# INLINE null #-}
{-# INLINE elem #-}
{-# INLINE notElem #-}
null :: Monad m => Stream m Int -> m Bool

elem, notElem, all, any, and, or :: Monad m => Int -> Stream m Int -> m Bool

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
init s = S.init s >>= Prelude.mapM_ S.drain

{-# INLINE tail #-}
tail :: Monad m => Stream m a -> m ()
tail s = S.tail s >>= Prelude.mapM_ tail

{-# INLINE nullHeadTail #-}
nullHeadTail :: Monad m => Stream m Int -> m ()
nullHeadTail s = do
    r <- S.null s
    when (not r) $ do
        _ <- S.head s
        S.tail s >>= Prelude.mapM_ nullHeadTail

{-# INLINE mapM_ #-}
mapM_ :: Monad m => Stream m Int -> m ()
mapM_  = S.mapM_ (\_ -> return ())

toList = S.toList

{-# INLINE toListRev #-}
toListRev :: Monad m => Stream m Int -> m [Int]
toListRev = Internal.toListRev

foldrMBuild  = S.foldrM  (\x xs -> xs >>= return . (x :)) (return [])
foldl'Build = S.foldl' (flip (:)) []
foldlM'Build = S.foldlM' (\xs x -> return $ x : xs) []

foldrMReduce = S.foldrM (\x xs -> xs >>= return . (x +)) (return 0)
foldl'Reduce = S.foldl' (+) 0
foldl'ReduceMap = P.fmap (+1) . S.foldl' (+) 0
foldl1'Reduce = S.foldl1' (+)
foldlM'Reduce = S.foldlM' (\xs a -> return $ a + xs) 0

last   = S.last
null   = S.null
head   = S.head
elem value   = S.elem (value + 1)
notElem value = S.notElem (value + 1)
length = S.length
all value    = S.all (<= (value + 1))
any value    = S.any (> (value + 1))
and value    = S.and . S.map (<= (value + 1))
or value     = S.or . S.map (> (value + 1))
find value   = S.find (== (value + 1))
findIndex value = S.findIndex (== (value + 1))
elemIndex value = S.elemIndex (value + 1)
maximum = S.maximum
minimum = S.minimum
sum    = S.sum
product = S.product
minimumBy = S.minimumBy compare
maximumBy = S.maximumBy compare

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Stream m a -> m ()
transform = runStream

{-# INLINE composeN #-}
composeN
    :: MonadIO m
    => Int -> (Stream m Int -> Stream m Int) -> Stream m Int -> m ()
composeN n f =
    case n of
        1 -> transform . f
        2 -> transform . f . f
        3 -> transform . f . f . f
        4 -> transform . f . f . f . f
        _ -> undefined

-- polymorphic stream version of composeN
{-# INLINE composeN' #-}
composeN'
    :: (S.IsStream t, Monad m)
    => Int -> (t m Int -> Stream m Int) -> t m Int -> m ()
composeN' n f =
    case n of
        1 -> transform . f
        2 -> transform . f . S.adapt . f
        3 -> transform . f . S.adapt . f . S.adapt . f
        4 -> transform . f . S.adapt . f . S.adapt . f . S.adapt . f
        _ -> undefined

{-# INLINE scan #-}
{-# INLINE scanl1' #-}
{-# INLINE map #-}
{-# INLINE fmap #-}
{-# INLINE mapMaybe #-}
{-# INLINE filterEven #-}
{-# INLINE filterAllOut #-}
{-# INLINE filterAllIn #-}
{-# INLINE takeOne #-}
{-# INLINE takeAll #-}
{-# INLINE takeWhileTrue #-}
{-# INLINE takeWhileMTrue #-}
{-# INLINE dropOne #-}
{-# INLINE dropAll #-}
{-# INLINE dropWhileTrue #-}
{-# INLINE dropWhileMTrue #-}
{-# INLINE dropWhileFalse #-}
{-# INLINE findIndices #-}
{-# INLINE elemIndices #-}
{-# INLINE insertBy #-}
{-# INLINE deleteBy #-}
{-# INLINE reverse #-}
{-# INLINE reverse' #-}
{-# INLINE foldrS #-}
{-# INLINE foldrSMap #-}
{-# INLINE foldrT #-}
{-# INLINE foldrTMap #-}
scan, scanl1', map, fmap, mapMaybe, filterEven,
    takeOne, dropOne,
    reverse, reverse',
    foldrS, foldrSMap, foldrT, foldrTMap
    :: MonadIO m
    => Int -> Stream m Int -> m ()

filterAllOut,
    filterAllIn, takeAll, takeWhileTrue, takeWhileMTrue,
    dropAll, dropWhileTrue, dropWhileMTrue, dropWhileFalse,
    findIndices, elemIndices, insertBy, deleteBy
    :: MonadIO m
    => Int -> Int -> Stream m Int -> m ()

{-# INLINE mapMaybeM #-}
{-# INLINE intersperse #-}
mapMaybeM :: S.MonadAsync m => Int -> Stream m Int -> m ()
intersperse :: S.MonadAsync m => Int -> Int -> Stream m Int -> m ()

{-# INLINE mapM #-}
{-# INLINE map' #-}
{-# INLINE fmap' #-}
mapM, map' :: (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int) -> Int -> t m Int -> m ()

fmap' :: (S.IsStream t, S.MonadAsync m, P.Functor (t m))
    => (t m Int -> S.SerialT m Int) -> Int -> t m Int -> m ()

{-# INLINE sequence #-}
sequence :: (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int) -> t m (m Int) -> m ()

scan          n = composeN n $ S.scanl' (+) 0
scanl1'       n = composeN n $ S.scanl1' (+)
fmap          n = composeN n $ Prelude.fmap (+1)
fmap' t       n = composeN' n $ t . Prelude.fmap (+1)
map           n = composeN n $ S.map (+1)
map' t        n = composeN' n $ t . S.map (+1)
mapM t        n = composeN' n $ t . S.mapM return

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
    composeN n (Internal.pollCounts f FL.drain) str
    where f = Internal.rollingMap (P.-) . Internal.delayPost 1

{-# INLINE tapAsyncS #-}
tapAsyncS :: S.MonadAsync m => Int -> Stream m Int -> m ()
tapAsyncS n = composeN n $ Par.tapAsync S.sum

{-# INLINE tapAsync #-}
tapAsync :: S.MonadAsync m => Int -> Stream m Int -> m ()
tapAsync n = composeN n $ Internal.tapAsync FL.sum

mapMaybe      n = composeN n $ S.mapMaybe
    (\x -> if Prelude.odd x then Nothing else Just x)
mapMaybeM     n = composeN n $ S.mapMaybeM
    (\x -> if Prelude.odd x then return Nothing else return $ Just x)
sequence t    = transform . t . S.sequence
filterEven    n = composeN n $ S.filter even
filterAllOut value  n = composeN n $ S.filter (> (value + 1))
filterAllIn value   n = composeN n $ S.filter (<= (value + 1))
takeOne       n = composeN n $ S.take 1
takeAll value       n = composeN n $ S.take (value + 1)
takeWhileTrue value n = composeN n $ S.takeWhile (<= (value + 1))
takeWhileMTrue value n = composeN n $ S.takeWhileM (return . (<= (value + 1)))
dropOne        n = composeN n $ S.drop 1
dropAll value        n = composeN n $ S.drop (value + 1)
dropWhileTrue value  n = composeN n $ S.dropWhile (<= (value + 1))
dropWhileMTrue value n = composeN n $ S.dropWhileM (return . (<= (value + 1)))
dropWhileFalse value n = composeN n $ S.dropWhile (> (value + 1))
findIndices value    n = composeN n $ S.findIndices (== (value + 1))
elemIndices value    n = composeN n $ S.elemIndices (value + 1)
intersperse value    n = composeN n $ S.intersperse (value + 1)
insertBy value       n = composeN n $ S.insertBy compare (value + 1)
deleteBy value       n = composeN n $ S.deleteBy (>=) (value + 1)
reverse        n = composeN n $ S.reverse
reverse'       n = composeN n $ Internal.reverse'
foldrS         n = composeN n $ Internal.foldrS S.cons S.nil
foldrSMap      n = composeN n $ Internal.foldrS (\x xs -> x + 1 `S.cons` xs) S.nil
foldrT         n = composeN n $ Internal.foldrT S.cons S.nil
foldrTMap      n = composeN n $ Internal.foldrT (\x xs -> x + 1 `S.cons` xs) S.nil

{-# INLINE takeByTime #-}
takeByTime :: NanoSecond64 -> Int -> Stream IO Int -> IO ()
takeByTime i n = composeN n (Internal.takeByTime i)

#ifdef INSPECTION
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
{-# INLINE transformComposeMapM #-}
{-# INLINE transformTeeMapM #-}
{-# INLINE transformZipMapM #-}

transformMapM, transformComposeMapM, transformTeeMapM,
    transformZipMapM :: (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int) -> Int -> t m Int -> m ()

transformMapM t n = composeN' n $ t . Internal.transform (Pipe.mapM return)
transformComposeMapM t n = composeN' n $ t . Internal.transform
    (Pipe.mapM (\x -> return (x + 1))
        `Pipe.compose` Pipe.mapM (\x -> return (x + 2)))
transformTeeMapM t n = composeN' n $ t . Internal.transform
    (Pipe.mapM (\x -> return (x + 1))
        `Pipe.tee` Pipe.mapM (\x -> return (x + 2)))
transformZipMapM t n = composeN' n $ t . Internal.transform
    (Pipe.zipWith (+) (Pipe.mapM (\x -> return (x + 1)))
        (Pipe.mapM (\x -> return (x + 2))))

-------------------------------------------------------------------------------
-- Mixed Transformation
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
{-# INLINE filterScanl1 #-}
{-# INLINE filterMap #-}
scanMap, dropMap, dropScan,
    filterScan, filterScanl1
    :: MonadIO m => Int -> Stream m Int -> m ()

takeDrop, takeScan, takeMap, filterDrop,
    filterTake, filterMap
    :: MonadIO m => Int -> Int -> Stream m Int -> m ()

scanMap    n = composeN n $ S.map (subtract 1) . S.scanl' (+) 0
dropMap    n = composeN n $ S.map (subtract 1) . S.drop 1
dropScan   n = composeN n $ S.scanl' (+) 0 . S.drop 1
takeDrop value   n = composeN n $ S.drop 1 . S.take (value + 1)
takeScan value   n = composeN n $ S.scanl' (+) 0 . S.take (value + 1)
takeMap value    n = composeN n $ S.map (subtract 1) . S.take (value + 1)
filterDrop value n = composeN n $ S.drop 1 . S.filter (<= (value + 1))
filterTake value n = composeN n $ S.take (value + 1) . S.filter (<= (value + 1))
filterScan n = composeN n $ S.scanl' (+) 0 . S.filter (<= maxBound)
filterScanl1 n = composeN n $ S.scanl1' (+) . S.filter (<= maxBound)
filterMap value  n = composeN n $ S.map (subtract 1) . S.filter (<= (value + 1))

-------------------------------------------------------------------------------
-- Scan and fold
-------------------------------------------------------------------------------

data Pair a b = Pair !a !b deriving (Generic, NFData)

{-# INLINE sumProductFold #-}
sumProductFold :: Monad m => Stream m Int -> m (Int, Int)
sumProductFold = S.foldl' (\(s,p) x -> (s + x, p P.* x)) (0,1)

{-# INLINE sumProductScan #-}
sumProductScan :: Monad m => Stream m Int -> m (Pair Int Int)
sumProductScan = S.foldl' (\(Pair _  p) (s0,x) -> Pair s0 (p P.* x)) (Pair 0 1)
    . S.scanl' (\(s,_) x -> (s + x,x)) (0,0)

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
{-# INLINE iterateScanl1 #-}
{-# INLINE iterateFilterEven #-}
{-# INLINE iterateTakeAll #-}
{-# INLINE iterateDropOne #-}
{-# INLINE iterateDropWhileFalse #-}
{-# INLINE iterateDropWhileTrue #-}
iterateMapM, iterateScan, iterateScanl1, iterateFilterEven,
    iterateDropOne
    :: S.MonadAsync m
    => Int -> Stream m Int

iterateTakeAll,
    iterateDropWhileFalse, iterateDropWhileTrue
    :: S.MonadAsync m
    => Int -> Int -> Stream m Int

-- this is quadratic
iterateScan            = iterateSource (S.scanl' (+) 0) (maxIters `div` 10)
-- so is this
iterateScanl1          = iterateSource (S.scanl1' (+)) (maxIters `div` 10)

iterateMapM            = iterateSource (S.mapM return) maxIters
iterateFilterEven      = iterateSource (S.filter even) maxIters
iterateTakeAll value         = iterateSource (S.take (value + 1)) maxIters
iterateDropOne         = iterateSource (S.drop 1) maxIters
iterateDropWhileFalse value  = iterateSource (S.dropWhile (> (value + 1))) maxIters
iterateDropWhileTrue value   = iterateSource (S.dropWhile (<= (value + 1))) maxIters

-------------------------------------------------------------------------------
-- Combining streams
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Appending
-------------------------------------------------------------------------------

{-# INLINE serial2 #-}
serial2 :: Int -> Int -> IO ()
serial2 count n =
    S.drain $ S.serial
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

{-# INLINE serial4 #-}
serial4 :: Int -> Int -> IO ()
serial4 count n =
    S.drain $ S.serial
        ((S.serial (sourceUnfoldrMN count n)
                   (sourceUnfoldrMN count (n + 1))))
        ((S.serial (sourceUnfoldrMN count (n+2))
                   (sourceUnfoldrMN count (n + 3))))

{-# INLINE append2 #-}
append2 :: Int -> Int -> IO ()
append2 count n =
    S.drain $ Internal.append
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

{-# INLINE append4 #-}
append4 :: Int -> Int -> IO ()
append4 count n =
    S.drain $ Internal.append
        ((Internal.append (sourceUnfoldrMN count n)
                          (sourceUnfoldrMN count (n + 1))))
        ((Internal.append (sourceUnfoldrMN count (n+2))
                          (sourceUnfoldrMN count (n + 3))))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'append2
inspect $ 'append2 `hasNoType` ''D.AppendState
#endif

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------

{-# INLINE wSerial2 #-}
wSerial2 :: Int -> Int -> IO ()
wSerial2 value n = S.drain $ S.wSerial
    (sourceUnfoldrMN (value `div` 2) n)
    (sourceUnfoldrMN (value `div` 2) (n + 1))

{-# INLINE interleave2 #-}
interleave2 :: Int -> Int -> IO ()
interleave2 value n = S.drain $ Internal.interleave
    (sourceUnfoldrMN (value `div` 2) n)
    (sourceUnfoldrMN (value `div` 2) (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'interleave2
inspect $ 'interleave2 `hasNoType` ''D.InterleaveState
#endif

{-# INLINE roundRobin2 #-}
roundRobin2 :: Int -> Int -> IO ()
roundRobin2 value n = S.drain $ Internal.roundrobin
    (sourceUnfoldrMN (value `div` 2) n)
    (sourceUnfoldrMN (value `div` 2) (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'roundRobin2
inspect $ 'roundRobin2 `hasNoType` ''D.InterleaveState
#endif

-------------------------------------------------------------------------------
-- Merging
-------------------------------------------------------------------------------

{-# INLINE mergeBy #-}
mergeBy :: Int -> Int -> IO ()
mergeBy count n =
    S.drain $ S.mergeBy P.compare
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mergeBy
inspect $ 'mergeBy `hasNoType` ''D.Step
#endif

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

{-# INLINE zip #-}
zip :: Int -> Int -> IO ()
zip count n =
    S.drain $ S.zipWith (,)
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'zip
inspect $ 'zip `hasNoType` ''D.Step
#endif

{-# INLINE zipM #-}
zipM :: Int -> Int -> IO ()
zipM count n =
    S.drain $ S.zipWithM (curry return)
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'zipM
inspect $ 'zipM `hasNoType` ''D.Step
#endif

{-# INLINE zipAsync #-}
zipAsync :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (Int, Int)
zipAsync count n = do
    S.zipAsyncWith (,)
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

{-# INLINE zipAsyncM #-}
zipAsyncM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (Int, Int)
zipAsyncM count n = do
    S.zipAsyncWithM (curry return)
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

{-# INLINE zipAsyncAp #-}
zipAsyncAp :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (Int, Int)
zipAsyncAp count n  = do
    S.zipAsyncly $ (,)
        <$> (sourceUnfoldrMN count n)
        <*> (sourceUnfoldrMN count (n + 1))

{-# INLINE mergeAsyncByM #-}
mergeAsyncByM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
mergeAsyncByM count n = do
    S.mergeAsyncByM (\a b -> return (a `compare` b))
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

{-# INLINE mergeAsyncBy #-}
mergeAsyncBy :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
mergeAsyncBy count n = do
    S.mergeAsyncBy compare
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

-------------------------------------------------------------------------------
-- Multi-stream folds
-------------------------------------------------------------------------------

{-# INLINE isPrefixOf #-}
{-# INLINE isSubsequenceOf #-}
isPrefixOf, isSubsequenceOf :: Monad m => Stream m Int -> m Bool

isPrefixOf src = S.isPrefixOf src src
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
inspect $ 'eqBy `hasNoType` ''D.Step
#endif


{-# INLINE eqByPure #-}
eqByPure :: Int -> Int -> Identity Bool
eqByPure value n = eqBy' (sourceUnfoldr value n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqByPure
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
inspect $ 'cmpBy `hasNoType` ''D.Step
#endif

{-# INLINE cmpByPure #-}
cmpByPure :: Int -> Int -> Identity P.Ordering
cmpByPure value n = cmpBy' (sourceUnfoldr value n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cmpByPure
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
#endif

-- concatMap replicate/unfoldrM

{-# INLINE concatMapRepl4xN #-}
concatMapRepl4xN :: Int -> Int -> IO ()
concatMapRepl4xN value n = S.drain $ S.concatMap (S.replicate 4)
                          (sourceUnfoldrMN (value `div` 4) n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapRepl4xN
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
#endif

{-# INLINE concatMapWithAppend #-}
concatMapWithAppend :: Int -> Int -> Int -> IO ()
concatMapWithAppend = concatStreamsWith Internal.append

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithAppend
#endif

{-# INLINE concatMapWithWSerial #-}
concatMapWithWSerial :: Int -> Int -> Int -> IO ()
concatMapWithWSerial = concatStreamsWith S.wSerial

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithWSerial
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
#endif

{-# INLINE concatUnfoldInterleaveRepl4xN #-}
concatUnfoldInterleaveRepl4xN :: Int -> Int -> IO ()
concatUnfoldInterleaveRepl4xN value n =
    S.drain $ Internal.concatUnfoldInterleave
        (UF.replicateM 4)
        (sourceUnfoldrMN (value `div` 4) n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatUnfoldInterleaveRepl4xN
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

{-# INLINE foldableFoldl' #-}
foldableFoldl' :: Stream Identity Int -> Int
foldableFoldl' = F.foldl' (+) 0

{-# INLINE foldableSum #-}
foldableSum :: Stream Identity Int -> Int
foldableSum = P.sum

{-# INLINE traversableMapM #-}
traversableMapM :: Stream Identity Int -> IO (Stream Identity Int)
traversableMapM = P.mapM return
