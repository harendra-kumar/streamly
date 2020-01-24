-- |
-- Module      : NestedUnfoldOps
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

module NestedUnfoldOps where

import Control.Monad.IO.Class (MonadIO (..))
import Streamly.Internal.Data.Unfold (Unfold)

import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Fold as FL

linearCount :: Int
linearCount = 100000

-- n * (n + 1) / 2 == linearCount
concatCount :: Int
concatCount = 450

-- double nested loop
nestedCount2 :: Int
nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

-- triple nested loop
nestedCount3 :: Int
nestedCount3 = round (fromIntegral linearCount**(1/3::Double))

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

-- generate numbers up to the argument value
{-# INLINE source #-}
source :: Monad m => Int -> Unfold m Int Int
source n = UF.enumerateFromToIntegral n

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

{-# INLINE toNull #-}
toNull :: MonadIO m => Int -> m ()
toNull start = do
    let end = start + nestedCount2
    UF.fold
        (UF.map (\(x, y) -> x + y)
        $ UF.outerProduct (source end) (source end))
        FL.drain (start, start)

{-# INLINE toNull3 #-}
toNull3 :: MonadIO m => Int -> m ()
toNull3 start = do
    let end = start + nestedCount3
    UF.fold
            (UF.map (\(x, y) -> x + y)
            $ UF.outerProduct (source end)
                ((UF.map (\(x, y) -> x + y)
                $ UF.outerProduct (source end) (source end))))
            FL.drain (start, (start, start))

{-# INLINE concat #-}
concat :: MonadIO m => Int -> m ()
concat start = do
    let end = start + concatCount
    UF.fold
        (UF.concat (source end) (source end))
        FL.drain start

{-# INLINE toList #-}
toList :: MonadIO m => Int -> m [Int]
toList start = do
    let end = start + nestedCount2
    UF.fold
        (UF.map (\(x, y) -> x + y)
        $ UF.outerProduct (source end) (source end))
        FL.toList (start, start)

{-# INLINE toListSome #-}
toListSome :: MonadIO m => Int -> m [Int]
toListSome start = do
    let end = start + nestedCount2
    UF.fold
        (UF.take 1000 $ (UF.map (\(x, y) -> x + y)
            $ UF.outerProduct (source end) (source end)))
        FL.toList (start, start)

{-# INLINE filterAllOut #-}
filterAllOut :: MonadIO m => Int -> m ()
filterAllOut start = do
    let end = start + nestedCount2
    UF.fold
        (UF.filter (< 0)
        $ UF.map (\(x, y) -> x + y)
        $ UF.outerProduct (source end) (source end))
        FL.drain (start, start)

{-# INLINE filterAllIn #-}
filterAllIn :: MonadIO m => Int -> m ()
filterAllIn start = do
    let end = start + nestedCount2
    UF.fold
        (UF.filter (> 0)
        $ UF.map (\(x, y) -> x + y)
        $ UF.outerProduct (source end) (source end))
        FL.drain (start, start)

{-# INLINE filterSome #-}
filterSome :: MonadIO m => Int -> m ()
filterSome start = do
    let end = start + nestedCount2
    UF.fold
        (UF.filter (> 1100000)
        $ UF.map (\(x, y) -> x + y)
        $ UF.outerProduct (source end) (source end))
        FL.drain (start, start)

{-# INLINE breakAfterSome #-}
breakAfterSome :: MonadIO m => Int -> m ()
breakAfterSome start = do
    let end = start + nestedCount2
    UF.fold
        (UF.takeWhile (<= 1100000)
        $ UF.map (\(x, y) -> x + y)
        $ UF.outerProduct (source end) (source end))
        FL.drain (start, start)
