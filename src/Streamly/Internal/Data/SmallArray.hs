-- |
-- Module      : Streamly.Internal.Data.SmallArray
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

module Streamly.Internal.Data.SmallArray
  (
    -- XXX should it be just Array instead? We should be able to replace one
    -- array type with another easily.
    SmallArray(..)

  , foldl'
  , foldr

  , length

  , writeN

  , toStreamD
  , toStreamDRev

  , toStream
  , toStreamRev
  , read

  , fromListN
  , fromStreamDN
  , fromStreamN

  , streamFold
  , fold
  )
where

import Prelude hiding (foldr, length, read)
import Control.DeepSeq (NFData(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.IO (unsafePerformIO)
import Data.Functor.Identity (runIdentity)

import Streamly.Internal.Data.SmallArray.Type

import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)
import Streamly.Internal.Data.Stream.Serial (SerialT)

import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Fold.Type as FL

{-# NOINLINE bottomElement #-}
bottomElement :: a
bottomElement = undefined

{-# INLINE length #-}
length :: SmallArray a -> Int
length = sizeofSmallArray

{-# INLINE_NORMAL toStreamD #-}
toStreamD :: Monad m => SmallArray a -> D.Stream m a
toStreamD arr = D.Stream step 0
  where
    {-# INLINE_LATE step #-}
    step _ i
        | i == length arr = return D.Stop
        | otherwise =
            return $
            case indexSmallArray## arr i of
                (# x #) -> D.Yield x (i + 1)

{-# INLINE_NORMAL toStreamDRev #-}
toStreamDRev :: Monad m => SmallArray a -> D.Stream m a
toStreamDRev arr = D.Stream step (length arr - 1)
  where
    {-# INLINE_LATE step #-}
    step _ i
        | i < 0 = return D.Stop
        | otherwise =
            return $
            case indexSmallArray## arr i of
                (# x #) -> D.Yield x (i - 1)

{-# INLINE_NORMAL foldl' #-}
foldl' :: (b -> a -> b) -> b -> SmallArray a -> b
foldl' f z arr = runIdentity $ D.foldl' f z $ toStreamD arr

{-# INLINE_NORMAL foldr #-}
foldr :: (a -> b -> b) -> b -> SmallArray a -> b
foldr f z arr = runIdentity $ D.foldr f z $ toStreamD arr

-- | @writeN n@ folds a maximum of @n@ elements from the input stream to an
-- 'SmallArray'.
--
-- Since we are folding to a 'SmallArray' @n@ should be <= 128, for larger number
-- of elements use an 'Array' from either "Streamly.Data.Array" or "Streamly.Data.Array.Foreign".
{-# INLINE_NORMAL writeN #-}
writeN :: MonadIO m => Int -> Fold m a (SmallArray a)
writeN limit = Fold step initial extract

    where

    initial = do
        marr <- liftIO $ newSmallArray limit bottomElement
        return $ FL.Partial (Tuple' marr 0)

    step st@(Tuple' marr i) x
        | i == limit = FL.Done <$> extract st
        | otherwise = do
            liftIO $ writeSmallArray marr i x
            return $ FL.Partial (Tuple' marr (i + 1))

    extract (Tuple' marr len) = liftIO $ freezeSmallArray marr 0 len

{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: MonadIO m => Int -> D.Stream m a -> m (SmallArray a)
fromStreamDN limit str = do
    marr <- liftIO $ newSmallArray (max limit 0) bottomElement
    i <-
        D.foldlM'
            (\i x -> i `seq` liftIO (writeSmallArray marr i x) >> return (i + 1))
            (return 0) $
        D.take limit str
    liftIO $ freezeSmallArray marr 0 i

-- | Create a 'SmallArray' from the first @n@ elements of a list. The
-- array may hold less than @n@ elements if the length of the list <=
-- @n@.
--
-- It is recommended to use a value of @n@ <= 128. For larger sized
-- arrays, use an 'Array' from "Streamly.Data.Array" or
-- "Streamly.Data.Array.Foreign"
{-# INLINABLE fromListN #-}
fromListN :: Int -> [a] -> SmallArray a
fromListN n xs = unsafePerformIO $ fromStreamDN n $ D.fromList xs

instance NFData a => NFData (SmallArray a) where
    {-# INLINE rnf #-}
    rnf = foldl' (\_ x -> rnf x) ()

-- | Create a 'SmallArray' from the first @n@ elements of a stream. The
-- array is allocated to size @n@, if the stream terminates before @n@
-- elements then the array may hold less than @n@ elements.
--
-- For optimal performance use this with @n@ <= 128.
{-# INLINE fromStreamN #-}
fromStreamN :: MonadIO m => Int -> SerialT m a -> m (SmallArray a)
fromStreamN n m = do
    when (n < 0) $ error "fromStreamN: negative write count specified"
    fromStreamDN n $ D.toStreamD m

{-# INLINE_EARLY toStream #-}
toStream :: (Monad m, IsStream t) => SmallArray a -> t m a
toStream = D.fromStreamD . toStreamD

{-# INLINE_EARLY toStreamRev #-}
toStreamRev :: (Monad m, IsStream t) => SmallArray a -> t m a
toStreamRev = D.fromStreamD . toStreamDRev

{-# INLINE fold #-}
fold :: Monad m => Fold m a b -> SmallArray a -> m b
fold f arr = D.fold f (toStreamD arr)

{-# INLINE streamFold #-}
streamFold :: Monad m => (SerialT m a -> m b) -> SmallArray a -> m b
streamFold f arr = f (toStream arr)

{-# INLINE_NORMAL read #-}
read :: Monad m => Unfold m (SmallArray a) a
read = Unfold step inject
  where
    inject arr = return (arr, 0)
    step (arr, i)
        | i == length arr = return D.Stop
        | otherwise =
            return $
            case indexSmallArray## arr i of
                (# x #) -> D.Yield x (arr, i + 1)
