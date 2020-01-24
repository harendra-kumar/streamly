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

-- n * (n + 1) / 2 == linearCount
concatCount :: Int -> Int
concatCount linearCount =
    round (((1 + 8 * fromIntegral linearCount)**(1/2::Double) - 1) / 2)

-- double nested loop
nestedCount2 :: Int -> Int
nestedCount2 linearCount = round (fromIntegral linearCount**(1/2::Double))

-- triple nested loop
nestedCount3 :: Int -> Int
nestedCount3 linearCount = round (fromIntegral linearCount**(1/3::Double))

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
toNull :: MonadIO m => Int -> Int -> m ()
toNull linearCount start = do
    let end = start + nestedCount2 linearCount
    UF.fold
        (UF.map (\(x, y) -> x + y)
        $ UF.outerProduct (source end) (source end))
        FL.drain (start, start)

{-# INLINE toNull3 #-}
toNull3 :: MonadIO m => Int -> Int -> m ()
toNull3 linearCount start = do
    let end = start + nestedCount3 linearCount
    UF.fold
            (UF.map (\(x, y) -> x + y)
            $ UF.outerProduct (source end)
                ((UF.map (\(x, y) -> x + y)
                $ UF.outerProduct (source end) (source end))))
            FL.drain (start, (start, start))

{-# INLINE concat #-}
concat :: MonadIO m => Int -> Int -> m ()
concat linearCount start = do
    let end = start + concatCount linearCount
    UF.fold
        (UF.concat (source end) (source end))
        FL.drain start

{-# INLINE toList #-}
toList :: MonadIO m => Int -> Int -> m [Int]
toList linearCount start = do
    let end = start + nestedCount2 linearCount
    UF.fold
        (UF.map (\(x, y) -> x + y)
        $ UF.outerProduct (source end) (source end))
        FL.toList (start, start)

{-# INLINE toListSome #-}
toListSome :: MonadIO m => Int -> Int -> m [Int]
toListSome linearCount start = do
    let end = start + nestedCount2 linearCount
    UF.fold
        (UF.take 1000 $ (UF.map (\(x, y) -> x + y)
            $ UF.outerProduct (source end) (source end)))
        FL.toList (start, start)

{-# INLINE filterAllOut #-}
filterAllOut :: MonadIO m => Int -> Int -> m ()
filterAllOut linearCount start = do
    let end = start + nestedCount2 linearCount
    UF.fold
        (UF.filter (< 0)
        $ UF.map (\(x, y) -> x + y)
        $ UF.outerProduct (source end) (source end))
        FL.drain (start, start)

{-# INLINE filterAllIn #-}
filterAllIn :: MonadIO m => Int -> Int -> m ()
filterAllIn linearCount start = do
    let end = start + nestedCount2 linearCount
    UF.fold
        (UF.filter (> 0)
        $ UF.map (\(x, y) -> x + y)
        $ UF.outerProduct (source end) (source end))
        FL.drain (start, start)

{-# INLINE filterSome #-}
filterSome :: MonadIO m => Int -> Int -> m ()
filterSome linearCount start = do
    let end = start + nestedCount2 linearCount
    UF.fold
        (UF.filter (> 1100000)
        $ UF.map (\(x, y) -> x + y)
        $ UF.outerProduct (source end) (source end))
        FL.drain (start, start)

{-# INLINE breakAfterSome #-}
breakAfterSome :: MonadIO m => Int -> Int -> m ()
breakAfterSome linearCount start = do
    let end = start + nestedCount2 linearCount
    UF.fold
        (UF.takeWhile (<= 1100000)
        $ UF.map (\(x, y) -> x + y)
        $ UF.outerProduct (source end) (source end))
        FL.drain (start, start)
