-- |
-- Module      : ArrayOps
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Streamly.Benchmark.Memory.ArrayOps where

-- import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
-- import Data.Maybe (fromJust)
import Prelude (Int, Bool, (+), ($), (==), (>), (.), Maybe(..), undefined)
import qualified Prelude as P
#ifdef DEVBUILD
import qualified Data.Foldable as F
#endif
import qualified GHC.Exts as GHC
-- import Control.DeepSeq (NFData)
-- import GHC.Generics (Generic)

import qualified Streamly           as S hiding (foldMapWith, runStream)
import qualified Streamly.Memory.Array as A
import qualified Streamly.Prelude   as S

value, maxValue :: Int
#ifdef LINEAR_ASYNC
value = 10000
#else
value = 100000
#endif
maxValue = value + 1

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream = A.Array

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: MonadIO m => Int -> m (Stream Int)
sourceUnfoldr n = S.fold (A.writeN value) $ S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else (Just (cnt, cnt + 1))

{-# INLINE sourceIntFromTo #-}
sourceIntFromTo :: MonadIO m => Int -> m (Stream Int)
sourceIntFromTo n = S.fold (A.writeN value) $ S.enumerateFromTo n (n + value)

{-# INLINE sourceIntFromToFromStream #-}
sourceIntFromToFromStream :: MonadIO m => Int -> m (Stream Int)
sourceIntFromToFromStream n = S.fold A.write $ S.enumerateFromTo n (n + value)

sourceIntFromToFromList :: MonadIO m => Int -> m (Stream Int)
sourceIntFromToFromList n = P.return $ A.fromList $ [n..n + value]

{-# INLINE sourceFromList #-}
sourceFromList :: MonadIO m => Int -> m (Stream Int)
sourceFromList n = S.fold (A.writeN value) $ S.fromList [n..n+value]

{-# INLINE sourceIsList #-}
sourceIsList :: Int -> Stream Int
sourceIsList n = GHC.fromList [n..n+value]

{-# INLINE sourceIsString #-}
sourceIsString :: Int -> Stream P.Char
sourceIsString n = GHC.fromString (P.replicate (n + value) 'a')

{-
-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.runStream

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
head, last, minimum, maximum, find, findIndex, elemIndex, foldl1'Reduce
    :: Monad m => Stream m Int -> m (Maybe Int)

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
null, elem, notElem, all, any, and, or :: Monad m => Stream m Int -> m Bool

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
init s = S.init s >>= Prelude.mapM_ S.runStream

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

{-# INLINE toRevList #-}
toRevList :: Monad m => Stream m Int -> m [Int]
toRevList = S.toRevList

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
elem   = S.elem maxValue
notElem = S.notElem maxValue
length = S.length
all    = S.all (<= maxValue)
any    = S.any (> maxValue)
and    = S.and . S.map (<= maxValue)
or     = S.or . S.map (> maxValue)
find   = S.find (== maxValue)
findIndex = S.findIndex (== maxValue)
elemIndex = S.elemIndex maxValue
maximum = S.maximum
minimum = S.minimum
sum    = S.sum
product = S.product
minimumBy = S.minimumBy compare
maximumBy = S.maximumBy compare
-}

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-
{-# INLINE transform #-}
transform :: Stream a -> Stream a
transform = P.id
-}

{-# INLINE composeN #-}
composeN :: P.Monad m
    => Int -> (Stream Int -> m (Stream Int)) -> Stream Int -> m (Stream Int)
composeN n f x =
    case n of
        1 -> f x
        2 -> f x P.>>= f
        3 -> f x P.>>= f P.>>= f
        4 -> f x P.>>= f P.>>= f P.>>= f
        _ -> undefined

{-# INLINE scanl' #-}
{-# INLINE scanl1' #-}
{-# INLINE map #-}
{-
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
{-# INLINE foldrS #-}
{-# INLINE foldrSMap #-}
{-# INLINE foldrT #-}
{-# INLINE foldrTMap #-}
    -}
scanl' , scanl1', map{-, fmap, mapMaybe, filterEven, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, takeWhileMTrue, dropOne,
    dropAll, dropWhileTrue, dropWhileMTrue, dropWhileFalse,
    findIndices, elemIndices, insertBy, deleteBy, reverse,
    foldrS, foldrSMap, foldrT, foldrTMap -}
    :: MonadIO m => Int -> Stream Int -> m (Stream Int)

{-
{-# INLINE mapMaybeM #-}
mapMaybeM :: S.MonadAsync m => Int -> Stream m Int -> m ()

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
    -}

{-# INLINE onArray #-}
onArray
    :: MonadIO m => (S.SerialT m Int -> S.SerialT m Int)
    -> Stream Int
    -> m (Stream Int)
onArray f arr = S.fold (A.writeN value) $ f $ (S.unfold A.read arr)

scanl'        n = composeN n $ onArray $ S.scanl' (+) 0
scanl1'       n = composeN n $ onArray $ S.scanl1' (+)
map           n = composeN n $ onArray $ S.map (+1)
-- map           n = composeN n $ A.map (+1)
{-
fmap          n = composeN n $ Prelude.fmap (+1)
fmap' t       n = composeN' n $ t . Prelude.fmap (+1)
map' t        n = composeN' n $ t . S.map (+1)
mapM t        n = composeN' n $ t . S.mapM return
mapMaybe      n = composeN n $ S.mapMaybe
    (\x -> if Prelude.odd x then Nothing else Just x)
mapMaybeM     n = composeN n $ S.mapMaybeM
    (\x -> if Prelude.odd x then return Nothing else return $ Just x)
sequence t    = transform . t . S.sequence
filterEven    n = composeN n $ S.filter even
filterAllOut  n = composeN n $ S.filter (> maxValue)
filterAllIn   n = composeN n $ S.filter (<= maxValue)
takeOne       n = composeN n $ S.take 1
takeAll       n = composeN n $ S.take maxValue
takeWhileTrue n = composeN n $ S.takeWhile (<= maxValue)
takeWhileMTrue n = composeN n $ S.takeWhileM (return . (<= maxValue))
dropOne        n = composeN n $ S.drop 1
dropAll        n = composeN n $ S.drop maxValue
dropWhileTrue  n = composeN n $ S.dropWhile (<= maxValue)
dropWhileMTrue n = composeN n $ S.dropWhileM (return . (<= maxValue))
dropWhileFalse n = composeN n $ S.dropWhile (> maxValue)
findIndices    n = composeN n $ S.findIndices (== maxValue)
elemIndices    n = composeN n $ S.elemIndices maxValue
insertBy       n = composeN n $ S.insertBy compare maxValue
deleteBy       n = composeN n $ S.deleteBy (>=) maxValue
reverse        n = composeN n $ S.reverse
foldrS         n = composeN n $ S.foldrS S.cons S.nil
foldrSMap      n = composeN n $ S.foldrS (\x xs -> x + 1 `S.cons` xs) S.nil
foldrT         n = composeN n $ S.foldrT S.cons S.nil
foldrTMap      n = composeN n $ S.foldrT (\x xs -> x + 1 `S.cons` xs) S.nil

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
iterateMapM, iterateScan, iterateScanl1, iterateFilterEven, iterateTakeAll,
    iterateDropOne, iterateDropWhileFalse, iterateDropWhileTrue
    :: S.MonadAsync m
    => Int -> Stream m Int

-- this is quadratic
iterateScan            = iterateSource (S.scanl' (+) 0) (maxIters `div` 10)
-- so is this
iterateScanl1          = iterateSource (S.scanl1' (+)) (maxIters `div` 10)

iterateMapM            = iterateSource (S.mapM return) maxIters
iterateFilterEven      = iterateSource (S.filter even) maxIters
iterateTakeAll         = iterateSource (S.take maxValue) maxIters
iterateDropOne         = iterateSource (S.drop 1) maxIters
iterateDropWhileFalse  = iterateSource (S.dropWhile (> maxValue)) maxIters
iterateDropWhileTrue   = iterateSource (S.dropWhile (<= maxValue)) maxIters

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

{-# INLINE zip #-}
{-# INLINE zipM #-}
{-# INLINE mergeBy #-}
zip, zipM, mergeBy :: Monad m => Stream m Int -> m ()

zip src       = do
    r <- S.tail src
    let src1 = fromJust r
    transform (S.zipWith (,) src src1)
zipM src      =  do
    r <- S.tail src
    let src1 = fromJust r
    transform (S.zipWithM (curry return) src src1)

mergeBy src     =  do
    r <- S.tail src
    let src1 = fromJust r
    transform (S.mergeBy P.compare src src1)

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

{-# INLINE zipAsync #-}
{-# INLINE zipAsyncM #-}
{-# INLINE zipAsyncAp #-}
zipAsync, zipAsyncAp, zipAsyncM :: S.MonadAsync m => Stream m Int -> m ()

zipAsync src  = do
    r <- S.tail src
    let src1 = fromJust r
    transform (S.zipAsyncWith (,) src src1)

zipAsyncM src = do
    r <- S.tail src
    let src1 = fromJust r
    transform (S.zipAsyncWithM (curry return) src src1)

zipAsyncAp src  = do
    r <- S.tail src
    let src1 = fromJust r
    transform (S.zipAsyncly $ (,) <$> S.serially src
                                  <*> S.serially src1)

{-# INLINE eqBy #-}
eqBy :: (Monad m, P.Eq a) => Stream m a -> m P.Bool
eqBy src = S.eqBy (==) src src

{-# INLINE cmpBy #-}
cmpBy :: (Monad m, P.Ord a) => Stream m a -> m P.Ordering
cmpBy src = S.cmpBy P.compare src src

concatStreamLen, maxNested :: Int
concatStreamLen = 1
maxNested = 100000

{-# INLINE concatMap #-}
concatMap :: S.MonadAsync m => Int -> Stream m Int
concatMap n = S.concatMap (\_ -> sourceUnfoldrMN maxNested n)
                          (sourceUnfoldrMN concatStreamLen n)

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
{-# INLINE filterScanl1 #-}
{-# INLINE filterMap #-}
scanMap, dropMap, dropScan, takeDrop, takeScan, takeMap, filterDrop,
    filterTake, filterScan, filterScanl1, filterMap
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
filterScanl1 n = composeN n $ S.scanl1' (+) . S.filter (<= maxBound)
filterMap  n = composeN n $ S.map (subtract 1) . S.filter (<= maxValue)

data Pair a b = Pair !a !b deriving (Generic, NFData)

{-# INLINE sumProductFold #-}
sumProductFold :: Monad m => Stream m Int -> m (Int, Int)
sumProductFold = S.foldl' (\(s,p) x -> (s + x, p P.* x)) (0,1)

{-# INLINE sumProductScan #-}
sumProductScan :: Monad m => Stream m Int -> m (Pair Int Int)
sumProductScan = S.foldl' (\(Pair _  p) (s0,x) -> Pair s0 (p P.* x)) (Pair 0 1)
    . S.scanl' (\(s,_) x -> (s + x,x)) (0,0)

-------------------------------------------------------------------------------
-- Pure stream operations
-------------------------------------------------------------------------------

-}
{-# INLINE eqInstance #-}
eqInstance :: Stream Int -> Bool
eqInstance src = src == src

{-# INLINE eqInstanceNotEq #-}
eqInstanceNotEq :: Stream Int -> Bool
eqInstanceNotEq src = src P./= src

{-# INLINE ordInstance #-}
ordInstance :: Stream Int -> Bool
ordInstance src = src P.< src

{-# INLINE ordInstanceMin #-}
ordInstanceMin :: Stream Int -> Stream Int
ordInstanceMin src = P.min src src

{-# INLINE showInstance #-}
showInstance :: Stream Int -> P.String
showInstance src = P.show src

{-# INLINE readInstance #-}
readInstance :: P.String -> Stream Int
readInstance str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "readInstance: no parse"

{-# INLINE pureFoldl' #-}
pureFoldl' :: MonadIO m => Stream Int -> m Int
pureFoldl' = S.foldl' (+) 0 . S.unfold A.read

#ifdef DEVBUILD
{-# INLINE foldableFoldl' #-}
foldableFoldl' :: Stream Int -> Int
foldableFoldl' = F.foldl' (+) 0

{-# INLINE foldableSum #-}
foldableSum :: Stream Int -> Int
foldableSum = P.sum
#endif

{-
{-# INLINE traversableMapM #-}
traversableMapM :: Stream Identity Int -> IO (Stream Identity Int)
traversableMapM = P.mapM return
-}
