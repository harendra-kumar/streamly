-- |
-- Module      : StreamDKOps
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StreamDKOps where

-- import Control.Monad (when)
-- import Data.Maybe (isJust)
import Prelude
       (Monad, Int, (+), ($), (.), return, even, (>), (<=), div,
        subtract, undefined, Maybe(..), not, (>>=),
        maxBound, flip, (<$>), (<*>), round, (/), (**), (<))
import qualified Prelude as P
-- import qualified Data.List as List

import qualified Streamly.Streams.StreamDK as S
-- import qualified Streamly.Streams.Prelude as SP
-- import qualified Streamly.Internal.Data.SVar as S

value, value2, value3, value16, maxValue :: Int
value = 100000
value2 = round (P.fromIntegral value**(1/2::P.Double)) -- double nested loop
value3 = round (P.fromIntegral value**(1/3::P.Double)) -- triple nested loop
value16 = round (P.fromIntegral value**(1/16::P.Double)) -- triple nested loop
maxValue = value

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream m a = S.Stream m a

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: Monad m => Int -> Stream m Int
sourceUnfoldr n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrN #-}
sourceUnfoldrN :: Monad m => Int -> Int -> Stream m Int
sourceUnfoldrN m n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + m
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Stream m Int
sourceUnfoldrM n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldrMN #-}
sourceUnfoldrMN :: Monad m => Int -> Int -> Stream m Int
sourceUnfoldrMN m n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + m
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-
{-# INLINE sourceFromEnum #-}
sourceFromEnum :: Monad m => Int -> Stream m Int
sourceFromEnum n = S.enumFromStepN n 1 value
-}

{-
{-# INLINE sourceFromFoldable #-}
sourceFromFoldable :: Int -> Stream m Int
sourceFromFoldable n = S.fromFoldable [n..n+value]
-}

{-
{-# INLINE sourceFromFoldableM #-}
sourceFromFoldableM :: S.MonadAsync m => Int -> Stream m Int
sourceFromFoldableM n = S.fromFoldableM (Prelude.fmap return [n..n+value])
-}

{-
{-# INLINE sourceFoldMapWith #-}
sourceFoldMapWith :: Int -> Stream m Int
sourceFoldMapWith n = SP.foldMapWith S.serial S.yield [n..n+value]

{-# INLINE sourceFoldMapWithM #-}
sourceFoldMapWithM :: Monad m => Int -> Stream m Int
sourceFoldMapWithM n = SP.foldMapWith S.serial (S.yieldM . return) [n..n+value]
-}

{-# INLINE source #-}
source :: Monad m => Int -> Stream m Int
source = sourceUnfoldrM

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.drain
-- runStream = S.mapM_ (\_ -> return ())

{-
{-# INLINE mapM_ #-}
mapM_ :: Monad m => Stream m a -> m ()
mapM_ = S.mapM_ (\_ -> return ())
-}

{-# INLINE toNull #-}
toNull :: Monad m => Stream m Int -> m ()
toNull = runStream

{-# INLINE uncons #-}
uncons :: Monad m => Stream m Int -> m ()
uncons s = do
    r <- S.uncons s
    case r of
        Nothing -> return ()
        Just (_, t) -> uncons t

{-
{-# INLINE init #-}
init :: (Monad m, S.IsStream t) => t m a -> m ()
init s = do
    t <- S.init s
    P.mapM_ S.drain t

{-# INLINE tail #-}
tail :: (Monad m, S.IsStream t) => t m a -> m ()
tail s = S.tail s >>= P.mapM_ tail

{-# INLINE nullTail #-}
{-# INLINE headTail #-}
{-# INLINE zip #-}
nullTail, headTail, zip
    :: Monad m
    => Stream m Int -> m ()

nullTail s = do
    r <- S.null s
    when (not r) $ S.tail s >>= P.mapM_ nullTail

headTail s = do
    h <- S.head s
    when (isJust h) $ S.tail s >>= P.mapM_ headTail

{-# INLINE toList #-}
toList :: Monad m => Stream m Int -> m [Int]
toList = S.toList

{-# INLINE foldl #-}
foldl :: Monad m => Stream m Int -> m Int
foldl  = S.foldl' (+) 0

{-# INLINE last #-}
last :: Monad m => Stream m Int -> m (Maybe Int)
last   = S.last
-}

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

{-
{-# INLINE scan #-}
{-# INLINE map #-}
{-# INLINE fmap #-}
{-# INLINE filterEven #-}
{-# INLINE filterAllOut #-}
{-# INLINE filterAllIn #-}
{-# INLINE takeOne #-}
{-# INLINE takeAll #-}
{-# INLINE takeWhileTrue #-}
{-# INLINE dropOne #-}
{-# INLINE dropAll #-}
{-# INLINE dropWhileTrue #-}
{-# INLINE dropWhileFalse #-}
{-# INLINE foldlS #-}
{-# INLINE concatMap #-}
scan, map, fmap, filterEven, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, dropAll, dropOne,
    dropWhileTrue, dropWhileFalse, foldlS, concatMap
    :: Monad m
    => Int -> Stream m Int -> m ()

{-# INLINE mapM #-}
{-# INLINE mapMSerial #-}
{-# INLINE intersperse #-}
mapM, mapMSerial, intersperse
    :: S.MonadAsync m => Int -> Stream m Int -> m ()

scan           n = composeN n $ S.scanl' (+) 0
map            n = composeN n $ P.fmap (+1)
fmap           n = composeN n $ P.fmap (+1)
mapM           n = composeN n $ S.mapM return
mapMSerial     n = composeN n $ S.mapMSerial return
filterEven     n = composeN n $ S.filter even
filterAllOut   n = composeN n $ S.filter (> maxValue)
filterAllIn    n = composeN n $ S.filter (<= maxValue)
takeOne        n = composeN n $ S.take 1
takeAll        n = composeN n $ S.take maxValue
takeWhileTrue  n = composeN n $ S.takeWhile (<= maxValue)
dropOne        n = composeN n $ S.drop 1
dropAll        n = composeN n $ S.drop maxValue
dropWhileTrue  n = composeN n $ S.dropWhile (<= maxValue)
dropWhileFalse n = composeN n $ S.dropWhile (<= 1)
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
-}
