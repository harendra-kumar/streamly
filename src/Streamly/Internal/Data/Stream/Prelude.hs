{-# OPTIONS_GHC -Wno-orphans #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.Prelude
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Internal.Data.Stream.Prelude
    (
    -- * Stream Conversion
      fromStreamS
    , toStreamS

    -- * Running Effects
    , drain

    -- * Conversion operations
    , fromList
    , toList

    -- * Fold operations
    , foldrM
    , foldrMx
    , foldr

    , foldlx'
    , foldlMx'
    , foldl'
    , fold

    -- Lazy left folds are useful only for reversing the stream
    , foldlS
    , foldlT

    , scanlx'
    , scanlMx'
    , postscanlx'
    , postscanlMx'
    , postscanOnce
    , scanOnce

    -- * Zip style operations
    , eqBy
    , cmpBy

    -- * Foldable instance
    , minimum
    , maximum

    -- * Nesting
    , K.concatMapBy
    , K.concatMap

    -- * Fold Utilities
    , concatFoldableWith
    , concatMapFoldableWith
    , concatForFoldableWith
    )
where

import Control.Monad.Trans.Class (MonadTrans(..))
import Prelude hiding (foldr, minimum, maximum)
import qualified Prelude

import Streamly.Internal.Data.Fold.Type (Fold (..))

#ifdef USE_STREAMK_ONLY
import qualified Streamly.Internal.Data.Stream.StreamK as S
#else
import qualified Streamly.Internal.Data.Stream.StreamD as S
#endif

import Streamly.Internal.Data.Stream.StreamK (IsStream(..))
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.StreamD as D

------------------------------------------------------------------------------
-- Conversion to and from direct style stream
------------------------------------------------------------------------------

-- These definitions are dependent on what is imported as S
{-# INLINE fromStreamS #-}
fromStreamS :: (IsStream t, Monad m) => S.Stream m a -> t m a
fromStreamS = fromStream . S.toStreamK

{-# INLINE toStreamS #-}
toStreamS :: (IsStream t, Monad m) => t m a -> S.Stream m a
toStreamS = S.fromStreamK . toStream

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

{-# INLINE_EARLY drain #-}
drain :: (IsStream t, Monad m) => t m a -> m ()
drain m = D.drain $ D.fromStreamK (toStream m)
{-# RULES "drain fallback to CPS" [1]
    forall a. D.drain (D.fromStreamK a) = K.drain a #-}

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- |
-- @
-- fromList = 'Prelude.foldr' 'K.cons' 'K.nil'
-- @
--
-- Construct a stream from a list of pure values. This is more efficient than
-- 'K.fromFoldable' for serial streams.
--
-- @since 0.4.0
{-# INLINE_EARLY fromList #-}
fromList :: (Monad m, IsStream t) => [a] -> t m a
fromList = fromStreamS . S.fromList
{-# RULES "fromList fallback to StreamK" [1]
    forall a. S.toStreamK (S.fromList a) = K.fromFoldable a #-}

-- | Convert a stream into a list in the underlying monad.
--
-- @since 0.1.0
{-# INLINE toList #-}
toList :: (Monad m, IsStream t) => t m a -> m [a]
toList m = S.toList $ toStreamS m

------------------------------------------------------------------------------
-- Folds
------------------------------------------------------------------------------

{-# INLINE foldrM #-}
foldrM :: (Monad m, IsStream t) => (a -> m b -> m b) -> m b -> t m a -> m b
foldrM step acc m = S.foldrM step acc $ toStreamS m

{-# INLINE foldrMx #-}
foldrMx :: (Monad m, IsStream t)
    => (a -> m x -> m x) -> m x -> (m x -> m b) -> t m a -> m b
foldrMx step final project m = D.foldrMx step final project $ D.toStreamD m

{-# INLINE foldr #-}
foldr :: (Monad m, IsStream t) => (a -> b -> b) -> b -> t m a -> m b
foldr f z = foldrM (\a b -> f a <$> b) (return z)

-- | Like 'foldlx'', but with a monadic step function.
--
-- @since 0.7.0
{-# INLINE foldlMx' #-}
foldlMx' :: (IsStream t, Monad m)
    => (x -> a -> m x) -> m x -> (x -> m b) -> t m a -> m b
foldlMx' step begin done m = S.foldlMx' step begin done $ toStreamS m

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
--
-- @since 0.7.0
{-# INLINE foldlx' #-}
foldlx' :: (IsStream t, Monad m)
    => (x -> a -> x) -> x -> (x -> b) -> t m a -> m b
foldlx' step begin done m = S.foldlx' step begin done $ toStreamS m

-- | Strict left associative fold.
--
-- @since 0.2.0
{-# INLINE foldl' #-}
foldl' :: (Monad m, IsStream t) => (b -> a -> b) -> b -> t m a -> m b
foldl' step begin m = S.foldl' step begin $ toStreamS m

{-# INLINE foldlS #-}
foldlS :: IsStream t => (t m b -> a -> t m b) -> t m b -> t m a -> t m b
foldlS = K.foldlS

-- | Lazy left fold to a transformer monad.
--
-- For example, to reverse a stream:
--
-- > S.toList $ S.foldlT (flip S.cons) S.nil $ (S.fromList [1..5] :: SerialT IO Int)
--
{-# INLINE foldlT #-}
foldlT :: (Monad m, IsStream t, Monad (s m), MonadTrans s)
    => (s m b -> a -> s m b) -> s m b -> t m a -> s m b
foldlT f z s = S.foldlT f z (toStreamS s)

{-# INLINE fold #-}
fold :: (Monad m, IsStream t) => Fold m a b -> t m a -> m b
fold fld m = S.fold fld $ toStreamS m

------------------------------------------------------------------------------
-- Scans
------------------------------------------------------------------------------

-- postscanlM' followed by mapM
{-# INLINE postscanlMx' #-}
postscanlMx' :: (IsStream t, Monad m)
    => (x -> a -> m x) -> m x -> (x -> m b) -> t m a -> t m b
postscanlMx' step begin done m =
    D.fromStreamD $ D.postscanlMx' step begin done $ D.toStreamD m

-- postscanl' followed by map
{-# INLINE postscanlx' #-}
postscanlx' :: (IsStream t, Monad m)
    => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
postscanlx' step begin done m =
    D.fromStreamD $ D.postscanlx' step begin done $ D.toStreamD m

-- scanlM' followed by mapM
--
{-# INLINE scanlMx' #-}
scanlMx' :: (IsStream t, Monad m)
    => (x -> a -> m x) -> m x -> (x -> m b) -> t m a -> t m b
scanlMx' step begin done m =
    D.fromStreamD $ D.scanlMx' step begin done $ D.toStreamD m

{-# INLINE_NORMAL postscanOnce #-}
postscanOnce :: (IsStream t, Monad m)
    => Fold m a b -> t m a -> t m b
postscanOnce fld m =
    D.fromStreamD $ D.postscanOnce fld $ D.toStreamD m

{-# INLINE scanOnce #-}
scanOnce :: (IsStream t, Monad m)
    => Fold m a b -> t m a -> t m b
scanOnce fld m = D.fromStreamD $ D.scanOnce fld $ D.toStreamD m

-- scanl followed by map
--
-- | Strict left scan with an extraction function. Like 'scanl'', but applies a
-- user supplied extraction function (the third argument) at each step. This is
-- designed to work with the @foldl@ library. The suffix @x@ is a mnemonic for
-- extraction.
--
-- @since 0.7.0
{-# INLINE scanlx' #-}
scanlx' :: (IsStream t, Monad m)
    => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scanlx' step begin done m =
    fromStreamS $ S.scanlx' step begin done $ toStreamS m

------------------------------------------------------------------------------
-- Comparison
------------------------------------------------------------------------------

-- | Compare two streams for equality
--
-- @since 0.5.3
{-# INLINE eqBy #-}
eqBy :: (IsStream t, Monad m) => (a -> b -> Bool) -> t m a -> t m b -> m Bool
eqBy f m1 m2 = D.eqBy f (D.toStreamD m1) (D.toStreamD m2)

-- | Compare two streams
--
-- @since 0.5.3
{-# INLINE cmpBy #-}
cmpBy
    :: (IsStream t, Monad m)
    => (a -> b -> Ordering) -> t m a -> t m b -> m Ordering
cmpBy f m1 m2 = D.cmpBy f (D.toStreamD m1) (D.toStreamD m2)

{-# INLINE minimum #-}
minimum :: (IsStream t, Monad m, Ord a) => t m a -> m (Maybe a)
minimum m = S.minimum (toStreamS m)

{-# INLINE maximum #-}
maximum :: (IsStream t, Monad m, Ord a) => t m a -> m (Maybe a)
maximum m = S.maximum (toStreamS m)

------------------------------------------------------------------------------
-- Fold Utilities
------------------------------------------------------------------------------

{-
-- XXX do we have facilities in Foldable to fold any Foldable in this manner?
--
-- | Perform a pair wise bottom up hierarchical fold of elements in the
-- container using the given function as the merge function.
--
-- This will perform a balanced merge sort if the merge function is
-- 'mergeBy compare'.
--
-- @since 0.7.0
{-# INLINABLE foldbWith #-}
foldbWith :: IsStream t
    => (t m a -> t m a -> t m a) -> SerialT Identity (t m a) -> t m a
foldbWith f = K.foldb f K.nil
-}


-- | A variant of 'Data.Foldable.fold' that allows you to fold a 'Foldable'
-- container of streams using the specified stream sum operation.
--
-- @concatFoldableWith 'async' $ map return [1..3]@
--
-- Equivalent to:
--
-- @
-- concatFoldableWith f = S.concatMapFoldableWith f id
-- @
--
-- /Since: 0.8.0 (Renamed foldWith to concatFoldableWith)/
--
-- /Since: 0.1.0 ("Streamly")/
{-# INLINABLE concatFoldableWith #-}
concatFoldableWith :: (IsStream t, Foldable f)
    => (t m a -> t m a -> t m a) -> f (t m a) -> t m a
concatFoldableWith f = Prelude.foldr f K.nil

-- | A variant of 'foldMap' that allows you to map a monadic streaming action
-- on a 'Foldable' container and then fold it using the specified stream merge
-- operation.
--
-- @concatMapFoldableWith 'async' return [1..3]@
--
-- Equivalent to:
--
-- @
-- concatMapFoldableWith f g xs = S.concatMapWith f g (S.fromFoldable xs)
-- @
--
-- /Since: 0.8.0 (Renamed foldMapWith to concatMapFoldableWith)/
--
-- /Since: 0.1.0 ("Streamly")/
{-# INLINABLE concatMapFoldableWith #-}
concatMapFoldableWith :: (IsStream t, Foldable f)
    => (t m b -> t m b -> t m b) -> (a -> t m b) -> f a -> t m b
concatMapFoldableWith f g = Prelude.foldr (f . g) K.nil

-- | Like 'concatMapFoldableWith' but with the last two arguments reversed i.e. the
-- monadic streaming function is the last argument.
--
-- Equivalent to:
--
-- @
-- concatForFoldableWith = flip S.concatMapFoldableWith
-- @
--
-- /Since: 0.8.0 (Renamed forEachWith to concatForFoldableWith)/
--
-- /Since: 0.1.0 ("Streamly")/
{-# INLINABLE concatForFoldableWith #-}
concatForFoldableWith :: (IsStream t, Foldable f)
    => (t m b -> t m b -> t m b) -> f a -> (a -> t m b) -> t m b
concatForFoldableWith f xs g = Prelude.foldr (f . g) K.nil xs
