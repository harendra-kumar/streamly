-- |
-- Module      : Streamly.Internal.Data.Unfold.Type
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Unfold.Type
    ( Unfold (..)
    , singletonM
    , singleton
    , identity
    , ConcatState (..)
    , concat
    , lmap
    , map
    , const
    , apSequence
    , apDiscardSnd
    , cross
    , concatMapM
    , concatMap
    , zipWithM
    , zipWith
    )
where

#include "inline.hs"

import Control.Arrow (Arrow(..))
import Control.Category (Category(..))
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Stream.StreamD.Step (Step(..))

import Prelude hiding (const, map, concat, concatMap, zipWith)

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold

------------------------------------------------------------------------------
-- Monadic Unfolds
------------------------------------------------------------------------------

-- The order of arguments allows 'Category' and 'Arrow' instances but precludes
-- contravariant and contra-applicative.
--
-- | An @Unfold m a b@ is a generator of a stream of values of type @b@ from a
-- seed of type 'a' in 'Monad' @m@.
--
-- @since 0.7.0

data Unfold m a b =
    -- | @Unfold step inject@
    forall s. Unfold (s -> m (Step s b)) (a -> m s)

-- | Map a function on the input argument of the 'Unfold'.
--
-- @
-- lmap f = concat (singleton f)
-- @
--
-- /Pre-release/
{-# INLINE_NORMAL lmap #-}
lmap :: (a -> c) -> Unfold m c b -> Unfold m a b
lmap f (Unfold ustep uinject) = Unfold ustep (uinject Prelude.. f)

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

-- | Map a function on the output of the unfold (the type @b@).
--
-- /Pre-release/
{-# INLINE_NORMAL map #-}
map :: Functor m => (b -> c) -> Unfold m a b -> Unfold m a c
map f (Unfold ustep uinject) = Unfold step uinject

    where

    {-# INLINE_LATE step #-}
    step st = fmap (fmap f) (ustep st)

-- | Maps a function on the output of the unfold (the type @b@).
instance Functor m => Functor (Unfold m a) where
    {-# INLINE fmap #-}
    fmap = map

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

{-# INLINE const #-}
const :: Applicative m => m b -> Unfold m a b
const m = Unfold step inject

    where

    inject _ = pure False

    step False = (`Yield` True) <$> m
    step True = pure Stop

-- | Outer product discarding the first element.
--
-- /Unimplemented/
--
{-# INLINE_NORMAL apSequence #-}
apSequence :: -- Monad m =>
    Unfold m a b -> Unfold m a c -> Unfold m a c
apSequence (Unfold _step1 _inject1) (Unfold _step2 _inject2) = undefined

-- | Outer product discarding the second element.
--
-- /Unimplemented/
--
{-# INLINE_NORMAL apDiscardSnd #-}
apDiscardSnd :: -- Monad m =>
    Unfold m a b -> Unfold m a c -> Unfold m a b
apDiscardSnd (Unfold _step1 _inject1) (Unfold _step2 _inject2) = undefined

data Cross a s1 b s2 = CrossOuter a s1 | CrossInner a s1 b s2

-- | Create a cross product (vector product or cartesian product) of the
-- output streams of two unfolds.
--
{-# INLINE_NORMAL cross #-}
cross :: Monad m => Unfold m a b -> Unfold m a c -> Unfold m a (b, c)
cross (Unfold step1 inject1) (Unfold step2 inject2) = Unfold step inject

    where

    inject a = do
        s1 <- inject1 a
        return $ CrossOuter a s1

    {-# INLINE_LATE step #-}
    step (CrossOuter a s1) = do
        r <- step1 s1
        case r of
            Yield b s -> do
                s2 <- inject2 a
                return $ Skip (CrossInner a s b s2)
            Skip s    -> return $ Skip (CrossOuter a s)
            Stop      -> return Stop

    step (CrossInner a s1 b s2) = do
        r <- step2 s2
        return $ case r of
            Yield c s -> Yield (b, c) (CrossInner a s1 b s)
            Skip s    -> Skip (CrossInner a s1 b s)
            Stop      -> Skip (CrossOuter a s1)

-- | Example:
--
-- >>> Stream.toList $ Stream.unfold ((,) <$> Unfold.lmap fst Unfold.fromList <*> Unfold.lmap snd Unfold.fromList) ([1,2],[3,4])
-- [(1,3),(1,4),(2,3),(2,4)]
--
instance Monad m => Applicative (Unfold m a) where
    {-# INLINE pure #-}
    pure = const Prelude.. return

    {-# INLINE (<*>) #-}
    u1 <*> u2 = fmap (\(a, b) -> a b) (cross u1 u2)

    -- {-# INLINE (*>) #-}
    -- (*>) = apSequence

    -- {-# INLINE (<*) #-}
    -- (<*) = apDiscardSnd

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

data ConcatMapState m b s1 x =
      ConcatMapOuter x s1
    | forall s2. ConcatMapInner x s1 s2 (s2 -> m (Step s2 b))

-- | Map an unfold generating action to each element of an unfold and
-- flatten the results into a single stream.
--
{-# INLINE_NORMAL concatMapM #-}
concatMapM :: Monad m
    => (b -> m (Unfold m a c)) -> Unfold m a b -> Unfold m a c
concatMapM f (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        return $ ConcatMapOuter x s

    {-# INLINE_LATE step #-}
    step (ConcatMapOuter seed st) = do
        r <- step1 st
        case r of
            Yield x s -> do
                Unfold step2 inject2 <- f x
                innerSt <- inject2 seed
                return $ Skip (ConcatMapInner seed s innerSt step2)
            Skip s    -> return $ Skip (ConcatMapOuter seed s)
            Stop      -> return Stop

    step (ConcatMapInner seed ost ist istep) = do
        r <- istep ist
        return $ case r of
            Yield x s -> Yield x (ConcatMapInner seed ost s istep)
            Skip s    -> Skip (ConcatMapInner seed ost s istep)
            Stop      -> Skip (ConcatMapOuter seed ost)

{-# INLINE concatMap #-}
concatMap :: Monad m => (b -> Unfold m a c) -> Unfold m a b -> Unfold m a c
concatMap f = concatMapM (return Prelude.. f)

-- Note: concatMap and Monad instance for unfolds have performance comparable
-- to Stream. In fact, concatMap is slower than Stream, that may be some
-- optimization issue though.
--
-- | Example:
--
-- >>>  u = do { x <- Unfold.lmap fst Unfold.fromList; y <- Unfold.lmap snd Unfold.fromList; return (x,y); }
-- >>> Stream.toList $ Stream.unfold u ([1,2],[3,4])
-- [(1,3),(1,4),(2,3),(2,4)]
--
instance Monad m => Monad (Unfold m a) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = flip concatMap

    -- {-# INLINE (>>) #-}
    -- (>>) = (*>)

-------------------------------------------------------------------------------
-- Category
-------------------------------------------------------------------------------

-- XXX change it to yieldM or change yieldM in Prelude to singletonM
--
-- | Lift a monadic function into an unfold generating a singleton stream.
--
{-# INLINE singletonM #-}
singletonM :: Monad m => (a -> m b) -> Unfold m a b
singletonM f = Unfold step inject

    where

    inject x = return $ Just x

    {-# INLINE_LATE step #-}
    step (Just x) = f x >>= \r -> return $ Yield r Nothing
    step Nothing = return Stop

-- | Lift a pure function into an unfold generating a singleton stream.
--
{-# INLINE singleton #-}
singleton :: Monad m => (a -> b) -> Unfold m a b
singleton f = singletonM $ return Prelude.. f

-- | Identity unfold. Generates a singleton stream with the seed as the only
-- element in the stream.
--
-- > identity = singletonM return
--
{-# INLINE identity #-}
identity :: Monad m => Unfold m a a
identity = singletonM return

{-# ANN type ConcatState Fuse #-}
data ConcatState s1 s2 = ConcatOuter s1 | ConcatInner s1 s2

-- | Apply the second unfold to each output element of the first unfold and
-- flatten the output in a single stream.
--
-- /Pre-release/
--
{-# INLINE_NORMAL concat #-}
concat :: Monad m => Unfold m a b -> Unfold m b c -> Unfold m a c
concat (Unfold step1 inject1) (Unfold step2 inject2) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        return $ ConcatOuter s

    {-# INLINE_LATE step #-}
    step (ConcatOuter st) = do
        r <- step1 st
        case r of
            Yield x s -> do
                innerSt <- inject2 x
                return $ Skip (ConcatInner s innerSt)
            Skip s    -> return $ Skip (ConcatOuter s)
            Stop      -> return Stop

    step (ConcatInner ost ist) = do
        r <- step2 ist
        return $ case r of
            Yield x s -> Yield x (ConcatInner ost s)
            Skip s    -> Skip (ConcatInner ost s)
            Stop      -> Skip (ConcatOuter ost)

instance Monad m => Category (Unfold m) where
    {-# INLINE id #-}
    id = identity

    {-# INLINE (.) #-}
    (.) = flip concat

-------------------------------------------------------------------------------
-- Arrow
-------------------------------------------------------------------------------

{-# INLINE_NORMAL zipWithM #-}
zipWithM :: Monad m
    => (a -> b -> m c) -> Unfold m x a -> Unfold m y b -> Unfold m (x, y) c
zipWithM f (Unfold step1 inject1) (Unfold step2 inject2) = Unfold step inject

    where

    inject (x, y) = do
        s1 <- inject1 x
        s2 <- inject2 y
        return (s1, s2, Nothing)

    {-# INLINE_LATE step #-}
    step (s1, s2, Nothing) = do
        r <- step1 s1
        return $
          case r of
            Yield x s -> Skip (s, s2, Just x)
            Skip s    -> Skip (s, s2, Nothing)
            Stop      -> Stop

    step (s1, s2, Just x) = do
        r <- step2 s2
        case r of
            Yield y s -> do
                z <- f x y
                return $ Yield z (s1, s, Nothing)
            Skip s -> return $ Skip (s1, s, Just x)
            Stop   -> return Stop

-- | Divide the input into two unfolds and then zip the outputs to a single
-- stream.
--
-- @
--   S.mapM_ print
-- $ S.concatUnfold (UF.zipWith (,) UF.identity (UF.singleton sqrt))
-- $ S.map (\x -> (x,x))
-- $ S.fromList [1..10]
-- @
--
-- /Pre-release/
--
{-# INLINE zipWith #-}
zipWith :: Monad m
    => (a -> b -> c) -> Unfold m x a -> Unfold m y b -> Unfold m (x, y) c
zipWith f = zipWithM (\a b -> return (f a b))

{-# ANN module "HLint: ignore Use zip" #-}
instance Monad m => Arrow (Unfold m) where
    {-# INLINE arr #-}
    arr = singleton

    {-# INLINE (***) #-}
    (***) = zipWith (,)
