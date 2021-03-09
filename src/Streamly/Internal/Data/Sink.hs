-- |
-- Module      : Streamly.Internal.Data.Sink
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
-- The 'Sink' type is a just a special case of 'Fold' and we can do without
-- it. However, in some cases 'Sink' is a simpler type and may provide better
-- performance than 'Fold' because it does not maintain any state. Folds can
-- be used for both pure and monadic computations. Sinks are not applicable to
-- pure computations.

module Streamly.Internal.Data.Sink
    (
      Sink (..)

    -- * Upgrading
    , toFold

    -- * Composing Sinks
    -- ** Distribute
    , tee
    , distribute

    -- ** Demultiplex
    , demux

    -- ** Unzip
    , unzipM
    , unzip

    -- -- ** Group
    -- , grouped

    -- -- ** Nest
    -- , concatFold

    -- -- * Comonad
    -- , duplicate

    -- * Input Transformation
    -- | These are contravariant operations i.e. they apply on the input of the
    -- 'Sink', for this reason they are prefixed with 'l' for 'left'.
    , lmap
    , lmapM
    , lfilter
    , lfilterM

    -- * Sinks
    , drain
    , drainM
    -- , drainN
    -- , drainWhile
    )
where

import Control.Monad ((>=>), when, void)
import Data.Map.Strict (Map)
import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, replicate, concatMap, mconcat, foldMap, unzip)

import Streamly.Internal.Data.Fold.Types (Fold(..), Step(..))
import Streamly.Internal.Data.Sink.Types (Sink(..))

import qualified Data.Map.Strict as Map

------------------------------------------------------------------------------
-- Conversion
------------------------------------------------------------------------------

-- | Convert a 'Sink' to a 'Fold'. When you want to compose sinks and folds
-- together, upgrade a sink to a fold before composing.
toFold :: Monad m => Sink m a -> Fold m a ()
toFold (Sink f) = Fold step begin done
    where
    begin = return $ Partial ()
    step _ a = Partial <$> f a
    done _ = return ()

------------------------------------------------------------------------------
-- Composing with sinks
------------------------------------------------------------------------------

-- | Distribute one copy each of the input to both the sinks.
--
-- @
--                 |-------Sink m a
-- ---stream m a---|
--                 |-------Sink m a
-- @
-- @
-- > let pr x = Sink.drainM (putStrLn . ((x ++ " ") ++) . show)
-- > sink (Sink.tee (pr \"L") (pr \"R")) (S.enumerateFromTo 1 2)
-- L 1
-- R 1
-- L 2
-- R 2
-- @
--
tee :: Monad m => Sink m a -> Sink m a -> Sink m a
tee (Sink fL) (Sink fR) = Sink (\a -> fL a >> fR a)

-- | Distribute copies of the input to all the sinks in a container.
--
-- @
--                 |-------Sink m a
-- ---stream m a---|
--                 |-------Sink m a
--                 |
--                       ...
-- @
-- @
-- > let pr x = Sink.drainM (putStrLn . ((x ++ " ") ++) . show)
-- > sink (Sink.distribute [(pr \"L"), (pr \"R")]) (S.enumerateFromTo 1 2)
-- L 1
-- R 1
-- L 2
-- R 2
-- @
--
-- This is the consumer side dual of the producer side 'sequence_' operation.
{-# INLINE distribute #-}
distribute :: Monad m => [Sink m a] -> Sink m a
distribute ss = Sink (\a -> Prelude.mapM_ (\(Sink f) -> f a) ss)

-- | Demultiplex to multiple consumers without collecting the results. Useful
-- to run different effectful computations depending on the value of the stream
-- elements, for example handling network packets of different types using
-- different handlers.
--
-- @
--
--                             |-------Sink m a
-- -----stream m a-----Map-----|
--                             |-------Sink m a
--                             |
--                                       ...
-- @
--
-- @
-- > let pr x = Sink.drainM (putStrLn . ((x ++ " ") ++) . show)
-- > let table = Data.Map.fromList [(1, pr \"One"), (2, pr \"Two")]
--   in Sink.sink (Sink.demux id table) (S.enumerateFromTo 1 100)
-- One 1
-- Two 2
-- @
{-
demux :: (Monad m, Ord k) => (a -> k) -> Map k (Sink m a) -> Sink m a
demux f kv = Sink step

    where

    step a =
        -- XXX should we raise an exception in Nothing case?
        -- Ideally we should enforce that it is a total map over k so that look
        -- up never fails
        case Map.lookup (f a) kv of
            Nothing -> return ()
            Just (Sink g) -> g a
-}

demux :: (Monad m, Ord k) => Map k (Sink m a) -> Sink m (a, k)
demux kv = Sink step

    where

    step (a, k) =
        -- XXX should we raise an exception in Nothing case?
        -- Ideally we should enforce that it is a total map over k so that look
        -- up never fails
        case Map.lookup k kv of
            Nothing -> return ()
            Just (Sink g) -> g a

-- | Split elements in the input stream into two parts using a monadic unzip
-- function, direct each part to a different sink.
--
-- @
--
--                           |-------Sink m b
-- -----Stream m a----(b,c)--|
--                           |-------Sink m c
-- @
-- @
-- > let pr x = Sink.drainM (putStrLn . ((x ++ " ") ++) . show)
--   in Sink.sink (Sink.unzip return (pr \"L") (pr \"R")) (S.value (1,2))
-- L 1
-- R 2
-- @
{-# INLINE unzipM #-}
unzipM :: Monad m => (a -> m (b,c)) -> Sink m b -> Sink m c -> Sink m a
unzipM f (Sink stepB) (Sink stepC) =
    Sink (f >=> (\(b, c) -> stepB b >> stepC c))

-- | Same as 'unzipM' but with a pure unzip function.
{-# INLINE unzip #-}
unzip :: Monad m => (a -> (b,c)) -> Sink m b -> Sink m c -> Sink m a
unzip f = unzipM (return . f)

------------------------------------------------------------------------------
-- Input transformation
------------------------------------------------------------------------------

-- | Map a pure function on the input of a 'Sink'.
{-# INLINABLE lmap #-}
lmap :: (a -> b) -> Sink m b -> Sink m a
lmap f (Sink step) = Sink (step . f)

-- | Map a monadic function on the input of a 'Sink'.
{-# INLINABLE lmapM #-}
lmapM :: Monad m => (a -> m b) -> Sink m b -> Sink m a
lmapM f (Sink step) = Sink (f >=> step)

-- | Filter the input of a 'Sink' using a pure predicate function.
{-# INLINABLE lfilter #-}
lfilter :: Monad m => (a -> Bool) -> Sink m a -> Sink m a
lfilter f (Sink step) = Sink (\a -> when (f a) $ step a)

-- | Filter the input of a 'Sink' using a monadic predicate function.
{-# INLINABLE lfilterM #-}
lfilterM :: Monad m => (a -> m Bool) -> Sink m a -> Sink m a
lfilterM f (Sink step) = Sink (\a -> f a >>= \use -> when use $ step a)

------------------------------------------------------------------------------
-- Sinks
------------------------------------------------------------------------------

-- | Drain all input, running the effects and discarding the results.
drain :: Monad m => Sink m a
drain = Sink (\_ -> return ())

-- |
-- > drainM f = lmapM f drain
--
-- Drain all input after passing it through a monadic function.
{-# INLINABLE drainM #-}
drainM ::  Monad m => (a -> m b) -> Sink m a
drainM f = Sink (void . f)
