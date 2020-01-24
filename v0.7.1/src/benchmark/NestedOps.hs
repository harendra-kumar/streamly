-- |
-- Module      : BenchmarkOps
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NestedOps where

import Control.Exception (try)
import GHC.Exception (ErrorCall)

import qualified Streamly          as S hiding (runStream)
import qualified Streamly.Prelude  as S

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream m a = S.SerialT m a

{-# INLINE source #-}
source :: (S.MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
source = sourceUnfoldrM

-- Change this to "sourceUnfoldrM value n" for consistency
{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrM n value = S.serially $ S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceUnfoldr start n = S.unfoldr step start
    where
    step cnt =
        if cnt > start + n
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.drain

{-# INLINE runToList #-}
runToList :: Monad m => Stream m a -> m [a]
runToList = S.toList

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

{-# INLINE toNullAp #-}
toNullAp
    :: (S.IsStream t, S.MonadAsync m, Applicative (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m ()
toNullAp linearCount t start = runStream . t $
    (+) <$> source start nestedCount2 <*> source start nestedCount2
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNull #-}
toNull
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m ()
toNull linearCount t start = runStream . t $ do
    x <- source start nestedCount2
    y <- source start nestedCount2
    return $ x + y
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNull3 #-}
toNull3
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m ()
toNull3 linearCount t start = runStream . t $ do
    x <- source start nestedCount3
    y <- source start nestedCount3
    z <- source start nestedCount3
    return $ x + y + z
  where
    nestedCount3 = round (fromIntegral linearCount**(1/3::Double))

{-# INLINE toList #-}
toList
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m [Int]
toList linearCount t start = runToList . t $ do
    x <- source start nestedCount2
    y <- source start nestedCount2
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
        x <- source start nestedCount2
        y <- source start nestedCount2
        return $ x + y
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE filterAllOut #-}
filterAllOut
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m ()
filterAllOut linearCount t start = runStream . t $ do
    x <- source start nestedCount2
    y <- source start nestedCount2
    let s = x + y
    if s < 0
    then return s
    else S.nil
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE filterAllIn #-}
filterAllIn
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.SerialT m Int) -> Int -> m ()
filterAllIn linearCount t start = runStream . t $ do
    x <- source start nestedCount2
    y <- source start nestedCount2
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
filterSome linearCount t start = runStream . t $ do
    x <- source start nestedCount2
    y <- source start nestedCount2
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
    (_ :: Either ErrorCall ()) <- try $ runStream . t $ do
        x <- source start nestedCount2
        y <- source start nestedCount2
        let s = x + y
        if s > 1100000
        then error "break"
        else return s
    return ()
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))
