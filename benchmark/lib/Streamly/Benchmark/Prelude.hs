-- |
-- Module      : Streamly.Benchmark.Prelude
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Streamly.Benchmark.Prelude where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData(..))
import Control.Exception (try)
import Data.Functor.Identity (Identity)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup((<>)))
#endif
import GHC.Exception (ErrorCall)
import System.Random (randomRIO)

import qualified Data.Foldable as F
import qualified Data.List as List
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Stream.IsStream as Internal
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.Data.Stream.Serial as Serial

import Gauge
import Streamly.Internal.Data.Time.Units

-- Common polymorphic stream APIs used across multiple stream modules

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- enumerate
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- unfold
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceUnfoldr count start = S.unfoldr step start
    where
    step cnt =
        if cnt > start + count
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrM count start = S.unfoldrM step start
    where
    step cnt =
        if cnt > start + count
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldrMSerial #-}
sourceUnfoldrMSerial :: (S.IsStream t, Monad m) => Int -> Int -> t m Int
sourceUnfoldrMSerial count start = Serial.unfoldrM step start
    where
    step cnt =
        if cnt > start + count
        then return Nothing
        else return (Just (cnt, cnt + 1))

-------------------------------------------------------------------------------
-- fromList
-------------------------------------------------------------------------------

{-# INLINE sourceFromList #-}
sourceFromList :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceFromList value n = S.fromList [n..n+value]

{-# INLINE sourceFromListM #-}
sourceFromListM :: (S.MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
sourceFromListM value n = S.fromListM (fmap return [n..n+value])

-------------------------------------------------------------------------------
-- fromFoldable
-------------------------------------------------------------------------------

{-# INLINE sourceFromFoldable #-}
sourceFromFoldable :: S.IsStream t => Int -> Int -> t m Int
sourceFromFoldable value n = S.fromFoldable [n..n+value]

{-# INLINE sourceFromFoldableM #-}
sourceFromFoldableM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceFromFoldableM value n = S.fromFoldableM (fmap return [n..n+value])

-------------------------------------------------------------------------------
-- Time enumeration
-------------------------------------------------------------------------------

{-# INLINE absTimes #-}
absTimes :: (S.IsStream t, S.MonadAsync m, Functor (t m))
    => Int -> Int -> t m AbsTime
absTimes value _ = S.take value Internal.absTimes

-------------------------------------------------------------------------------
-- Buffering
-------------------------------------------------------------------------------

{-# INLINE mkAsync #-}
mkAsync :: (S.MonadAsync m, S.IsStream t) => (t m a -> S.SerialT m a) -> t m a -> m ()
mkAsync adapter = S.drain . adapter . S.mkAsync

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE toNull #-}
toNull :: Monad m => (t m a -> S.SerialT m a) -> t m a -> m ()
toNull t = S.drain . t

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: (S.IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrM value

-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchIOSrc #-}
benchIOSrc
    :: (t IO a -> S.SerialT IO a)
    -> String
    -> (Int -> t IO a)
    -> Benchmark
benchIOSrc t name f =
    bench name $ nfIO $ randomRIO (1,1) >>= toNull t . f

{-# NOINLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldrAction #-}
sourceUnfoldrAction :: (S.IsStream t, Monad m, Monad m1)
    => Int -> Int -> t m (m1 Int)
sourceUnfoldrAction value n = S.serially $ S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else Just (return cnt, cnt + 1)

{-# INLINE composeN #-}
composeN ::
       (S.IsStream t, Monad m)
    => Int
    -> (t m Int -> S.SerialT m Int)
    -> t m Int
    -> m ()
composeN n f =
    case n of
        1 -> S.drain . f
        2 -> S.drain . f . S.adapt . f
        3 -> S.drain . f . S.adapt . f . S.adapt . f
        4 -> S.drain . f . S.adapt . f . S.adapt . f . S.adapt . f
        _ -> undefined

{-# INLINE fmapN #-}
fmapN ::
       (S.IsStream t, S.MonadAsync m, Functor (t m))
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
fmapN t n = composeN n $ t . fmap (+ 1)

{-# INLINE mapN #-}
mapN ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
mapN t n = composeN n $ t . S.map (+ 1)

{-# INLINE mapM #-}
mapM ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
mapM t n = composeN n $ t . S.mapM return

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
transformMapM t n = composeN n $ t . Internal.transform (Pipe.mapM return)

{-# INLINE transformComposeMapM #-}
transformComposeMapM ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
transformComposeMapM t n =
    composeN n $
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
    composeN n $
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
    composeN n $
    t .
    Internal.transform
        (Pipe.zipWith
             (+)
             (Pipe.mapM (\x -> return (x + 1)))
             (Pipe.mapM (\x -> return (x + 2))))

-------------------------------------------------------------------------------
-- Streams of streams
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Concat foldable
-------------------------------------------------------------------------------

{-# INLINE sourceFoldMapWith #-}
sourceFoldMapWith :: (S.IsStream t, Semigroup (t m Int))
    => Int -> Int -> t m Int
sourceFoldMapWith value n = S.concatMapFoldableWith (<>) S.value [n..n+value]

{-# INLINE concatForFoldableWith #-}
concatForFoldableWith :: (S.IsStream t, Semigroup (t m Int))
    => Int -> Int -> t m Int
concatForFoldableWith value n =
    S.concatForFoldableWith (<>) [n..n+value] S.value

{-# INLINE concatFoldableWith #-}
concatFoldableWith :: (S.IsStream t, Semigroup (t m Int))
    => Int -> Int -> t m Int
concatFoldableWith value n =
    let step x =
            if x <= n + value
            then Just (S.value x, x + 1)
            else Nothing
        list = List.unfoldr step n
     in S.concatFoldableWith (<>) list

{-# INLINE sourceFoldMapWithStream #-}
sourceFoldMapWithStream :: (S.IsStream t, Semigroup (t m Int))
    => Int -> Int -> t m Int
sourceFoldMapWithStream value n = S.concatMapFoldableWith (<>) S.value
    $ (S.enumerateFromTo n (n + value) :: S.SerialT Identity Int)

{-# INLINE sourceFoldMapWithM #-}
sourceFoldMapWithM :: (S.IsStream t, Monad m, Semigroup (t m Int))
    => Int -> Int -> t m Int
sourceFoldMapWithM value n =
    S.concatMapFoldableWith (<>) (S.valueM . return) [n..n+value]

{-# INLINE sourceFoldMapM #-}
sourceFoldMapM :: (S.IsStream t, Monad m, Monoid (t m Int))
    => Int -> Int -> t m Int
sourceFoldMapM value n = F.foldMap (S.valueM . return) [n..n+value]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

{-# INLINE sourceConcatMapId #-}
sourceConcatMapId :: (S.IsStream t, Monad m)
    => Int -> Int -> t m (t m Int)
sourceConcatMapId value n =
    S.fromFoldable $ fmap (S.valueM . return) [n..n+value]

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
        (S.serially . sourceUnfoldrM inner)
        (S.serially $ sourceUnfoldrM outer n)

-------------------------------------------------------------------------------
-- Monadic outer product
-------------------------------------------------------------------------------

{-# INLINE runToList #-}
runToList :: Monad m => S.SerialT m a -> m [a]
runToList = S.toList

{-# INLINE apDiscardFst #-}
apDiscardFst
    :: (S.IsStream t, S.MonadAsync m, Applicative (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m ()
apDiscardFst linearCount t start = S.drain . t $
    S.serially (sourceUnfoldrM nestedCount2 start)
        *> S.serially (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE apDiscardSnd #-}
apDiscardSnd
    :: (S.IsStream t, S.MonadAsync m, Applicative (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m ()
apDiscardSnd linearCount t start = S.drain . t $
    S.serially (sourceUnfoldrM nestedCount2 start)
        <* S.serially (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE apLiftA2 #-}
apLiftA2
    :: (S.IsStream t, S.MonadAsync m, Applicative (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m ()
apLiftA2 linearCount t start = S.drain . t $
    liftA2 (+) (S.serially (sourceUnfoldrM nestedCount2 start))
        (S.serially (sourceUnfoldrM nestedCount2 start))

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullAp #-}
toNullAp
    :: (S.IsStream t, S.MonadAsync m, Applicative (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m ()
toNullAp linearCount t start = S.drain . t $
    (+) <$> S.serially (sourceUnfoldrM nestedCount2 start)
        <*> S.serially (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE monadThen #-}
monadThen
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m ()
monadThen linearCount t start = S.drain . t $ do
    (S.serially $ sourceUnfoldrM nestedCount2 start) >>
        (S.serially $ sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullM #-}
toNullM
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m ()
toNullM linearCount t start = S.drain . t $ do
    x <- S.serially $ sourceUnfoldrM nestedCount2 start
    y <- S.serially $ sourceUnfoldrM nestedCount2 start
    return $ x + y

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullM3 #-}
toNullM3
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m ()
toNullM3 linearCount t start = S.drain . t $ do
    x <- S.serially $ sourceUnfoldrM nestedCount3 start
    y <- S.serially $ sourceUnfoldrM nestedCount3 start
    z <- S.serially $ sourceUnfoldrM nestedCount3 start
    return $ x + y + z
  where
    nestedCount3 = round (fromIntegral linearCount**(1/3::Double))

{-# INLINE toListM #-}
toListM
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m [Int]
toListM linearCount t start = runToList . t $ do
    x <- S.serially $ sourceUnfoldrM nestedCount2 start
    y <- S.serially $ sourceUnfoldrM nestedCount2 start
    return $ x + y
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

-- Taking a specified number of elements is very expensive in logict so we have
-- a test to measure the same.
{-# INLINE toListSome #-}
toListSome
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m [Int]
toListSome linearCount t start =
    runToList . t $ S.take 10000 $ do
        x <- S.serially $ sourceUnfoldrM nestedCount2 start
        y <- S.serially $ sourceUnfoldrM nestedCount2 start
        return $ x + y
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE filterAllOutM #-}
filterAllOutM
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m ()
filterAllOutM linearCount t start = S.drain . t $ do
    x <- S.serially $ sourceUnfoldrM nestedCount2 start
    y <- S.serially $ sourceUnfoldrM nestedCount2 start
    let s = x + y
    if s < 0
    then return s
    else S.nil
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE filterAllInM #-}
filterAllInM
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m ()
filterAllInM linearCount t start = S.drain . t $ do
    x <- S.serially $ sourceUnfoldrM nestedCount2 start
    y <- S.serially $ sourceUnfoldrM nestedCount2 start
    let s = x + y
    if s > 0
    then return s
    else S.nil
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE filterSome #-}
filterSome
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m ()
filterSome linearCount t start = S.drain . t $ do
    x <- S.serially $ sourceUnfoldrM nestedCount2 start
    y <- S.serially $ sourceUnfoldrM nestedCount2 start
    let s = x + y
    if s > 1100000
    then return s
    else S.nil
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE breakAfterSome #-}
breakAfterSome
    :: (S.IsStream t, Monad (t IO))
    => Int -> (t IO Int -> S.SerialT IO Int) -> Int -> IO ()
breakAfterSome linearCount t start = do
    (_ :: Either ErrorCall ()) <- try $ S.drain . t $ do
        x <- S.serially $ sourceUnfoldrM nestedCount2 start
        y <- S.serially $ sourceUnfoldrM nestedCount2 start
        let s = x + y
        if s > 1100000
        then error "break"
        else return s
    return ()
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))
