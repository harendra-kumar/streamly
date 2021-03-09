-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Generate
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
--               (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Prefer unfolds ("Streamly.Internal.Data.Unfold") over the combinators in
-- this module. They are more powerful and efficient as they can be transformed
-- and composed on the input side efficiently and they can fuse in nested
-- operations (e.g.  concatUnfold). All the combinators in this module can be
-- expressed using unfolds with the same efficiency.
--
-- Operations in this module that are not in "Streamly.Internal.Data.Unfold":
-- generate, times, fromPrimIORef.
--
-- We should plan to replace this module with "Streamly.Internal.Data.Unfold"
-- in future.

-- A few combinators in this module have been adapted from the vector package
-- (c) Roman Leshchinskiy. See the notes in specific combinators.
--
module Streamly.Internal.Data.Stream.StreamD.Generate
  (
    -- * Primitives
      nil
    , nilM
    , cons
    , consM

    -- * From 'Unfold'
    , unfold

    -- * Unfolding
    , unfoldr
    , unfoldrM

    -- * From Values
    , value
    , valueM
    , repeat
    , repeatM
    , replicate
    , replicateM

    -- * Enumeration
    , enumerateFromStepIntegral
    , enumerateFromIntegral
    , enumerateFromThenIntegral
    , enumerateFromToIntegral
    , enumerateFromThenToIntegral

    , enumerateFromStepNum
    , numFrom
    , numFromThen
    , enumerateFromToFractional
    , enumerateFromThenToFractional

    -- * Time Enumeration
    , times

    -- * From Generators
    -- | Generate a monadic stream from a seed.
    , fromIndices
    , fromIndicesM
    , generate
    , generateM

    -- * Iteration
    , iterate
    , iterateM

    -- * From Containers
    -- | Transform an input structure into a stream.

    -- Note: Direct style stream does not support @fromFoldable@.
    , fromList
    , fromListM

    -- * Conversions
    , fromStreamK
    , toStreamK
    , fromStreamD
    , toStreamD
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Internal.Data.Time.Clock
    (Clock(Monotonic), asyncClock, readClock)
import Streamly.Internal.Data.Time.Units
    (toAbsTime, AbsTime, toRelTime64, RelTime64)

import Prelude hiding (iterate, repeat, replicate, takeWhile)
import Streamly.Internal.Data.Stream.StreamD.Type
import Streamly.Internal.Data.SVar

------------------------------------------------------------------------------
-- Primitives
------------------------------------------------------------------------------

-- | An empty 'Stream'.
{-# INLINE_NORMAL nil #-}
nil :: Monad m => Stream m a
nil = Stream (\_ _ -> return Stop) ()

-- XXX implement in terms of consM?
-- cons x = consM (return x)
--
-- | Can fuse but has O(n^2) complexity.
{-# INLINE_NORMAL cons #-}
cons :: Monad m => a -> Stream m a -> Stream m a
cons x (Stream step state) = Stream step1 Nothing
    where
    {-# INLINE_LATE step1 #-}
    step1 _ Nothing   = return $ Yield x (Just state)
    step1 gst (Just st) = do
        r <- step gst st
        return $
          case r of
            Yield a s -> Yield a (Just s)
            Skip  s   -> Skip (Just s)
            Stop      -> Stop

------------------------------------------------------------------------------
-- Unfolding
------------------------------------------------------------------------------

-- Adapted from vector package
{-# INLINE_NORMAL unfoldrM #-}
unfoldrM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Stream m a
unfoldrM next state = Stream step state
  where
    {-# INLINE_LATE step #-}
    step _ st = do
        r <- next st
        return $ case r of
            Just (x, s) -> Yield x s
            Nothing     -> Stop

{-# INLINE_LATE unfoldr #-}
unfoldr :: Monad m => (s -> Maybe (a, s)) -> s -> Stream m a
unfoldr f = unfoldrM (return . f)

------------------------------------------------------------------------------
-- From values
------------------------------------------------------------------------------

{-# INLINE_NORMAL repeatM #-}
repeatM :: Monad m => m a -> Stream m a
repeatM x = Stream (\_ _ -> x >>= \r -> return $ Yield r ()) ()

{-# INLINE_NORMAL repeat #-}
repeat :: Monad m => a -> Stream m a
repeat x = Stream (\_ _ -> return $ Yield x ()) ()

-- Adapted from the vector package
{-# INLINE_NORMAL replicateM #-}
replicateM :: forall m a. Monad m => Int -> m a -> Stream m a
replicateM n p = Stream step n
  where
    {-# INLINE_LATE step #-}
    step _ (i :: Int)
      | i <= 0    = return Stop
      | otherwise = do
          x <- p
          return $ Yield x (i - 1)

{-# INLINE_NORMAL replicate #-}
replicate :: Monad m => Int -> a -> Stream m a
replicate n x = replicateM n (return x)

------------------------------------------------------------------------------
-- Enumeration
------------------------------------------------------------------------------

-- This would not work properly for floats, therefore we put an Integral
-- constraint.
-- | Can be used to enumerate unbounded integrals. This does not check for
-- overflow or underflow for bounded integrals.
{-# INLINE_NORMAL enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: (Integral a, Monad m) => a -> a -> Stream m a
enumerateFromStepIntegral from stride =
    from `seq` stride `seq` Stream step from
    where
        {-# INLINE_LATE step #-}
        step _ !x = return $ Yield x $! (x + stride)

data EnumState a = EnumInit | EnumYield a a a | EnumStop

{-# INLINE_NORMAL enumerateFromThenToIntegralUp #-}
enumerateFromThenToIntegralUp
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenToIntegralUp from next to = Stream step EnumInit
    where
    {-# INLINE_LATE step #-}
    step _ EnumInit =
        return $
            if to < next
            then if to < from
                 then Stop
                 else Yield from EnumStop
            else -- from <= next <= to
                let stride = next - from
                in Skip $ EnumYield from stride (to - stride)

    step _ (EnumYield x stride toMinus) =
        return $
            if x > toMinus
            then Yield x EnumStop
            else Yield x $ EnumYield (x + stride) stride toMinus

    step _ EnumStop = return Stop

{-# INLINE_NORMAL enumerateFromThenToIntegralDn #-}
enumerateFromThenToIntegralDn
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenToIntegralDn from next to = Stream step EnumInit
    where
    {-# INLINE_LATE step #-}
    step _ EnumInit =
        return $ if to > next
            then if to > from
                 then Stop
                 else Yield from EnumStop
            else -- from >= next >= to
                let stride = next - from
                in Skip $ EnumYield from stride (to - stride)

    step _ (EnumYield x stride toMinus) =
        return $
            if x < toMinus
            then Yield x EnumStop
            else Yield x $ EnumYield (x + stride) stride toMinus

    step _ EnumStop = return Stop

{-# INLINE_NORMAL enumerateFromThenToIntegral #-}
enumerateFromThenToIntegral
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenToIntegral from next to
    | next >= from = enumerateFromThenToIntegralUp from next to
    | otherwise    = enumerateFromThenToIntegralDn from next to

{-# INLINE_NORMAL enumerateFromThenIntegral #-}
enumerateFromThenIntegral
    :: (Monad m, Integral a, Bounded a)
    => a -> a -> Stream m a
enumerateFromThenIntegral from next =
    if next > from
    then enumerateFromThenToIntegralUp from next maxBound
    else enumerateFromThenToIntegralDn from next minBound

-- We are assuming that "to" is constrained by the type to be within
-- max/min bounds.
{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: (Monad m, Integral a) => a -> a -> Stream m a
enumerateFromToIntegral from to =
    takeWhile (<= to) $ enumerateFromStepIntegral from 1

{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: (Monad m, Integral a, Bounded a) => a -> Stream m a
enumerateFromIntegral from = enumerateFromToIntegral from maxBound

-- We cannot write a general function for Num.  The only way to write code
-- portable between the two is to use a 'Real' constraint and convert between
-- Fractional and Integral using fromRational which is horribly slow.
{-# INLINE_NORMAL enumerateFromToFractional #-}
enumerateFromToFractional
    :: (Monad m, Fractional a, Ord a)
    => a -> a -> Stream m a
enumerateFromToFractional from to =
    takeWhile (<= to + 1 / 2) $ enumerateFromStepNum from 1

{-# INLINE_NORMAL enumerateFromThenToFractional #-}
enumerateFromThenToFractional
    :: (Monad m, Fractional a, Ord a)
    => a -> a -> a -> Stream m a
enumerateFromThenToFractional from next to =
    takeWhile predicate $ numFromThen from next
    where
    mid = (next - from) / 2
    predicate | next >= from  = (<= to + mid)
              | otherwise     = (>= to + mid)

-- For floating point numbers if the increment is less than the precision then
-- it just gets lost. Therefore we cannot always increment it correctly by just
-- repeated addition.
-- 9007199254740992 + 1 + 1 :: Double => 9.007199254740992e15
-- 9007199254740992 + 2     :: Double => 9.007199254740994e15

-- Instead we accumulate the increment counter and compute the increment
-- every time before adding it to the starting number.
--
-- This works for Integrals as well as floating point numbers, but
-- enumerateFromStepIntegral is faster for integrals.
{-# INLINE_NORMAL enumerateFromStepNum #-}
enumerateFromStepNum :: (Monad m, Num a) => a -> a -> Stream m a
enumerateFromStepNum from stride = Stream step 0
    where
    {-# INLINE_LATE step #-}
    step _ !i = return $ (Yield $! (from + i * stride)) $! (i + 1)

{-# INLINE_NORMAL numFrom #-}
numFrom :: (Monad m, Num a) => a -> Stream m a
numFrom from = enumerateFromStepNum from 1

{-# INLINE_NORMAL numFromThen #-}
numFromThen :: (Monad m, Num a) => a -> a -> Stream m a
numFromThen from next = enumerateFromStepNum from (next - from)

------------------------------------------------------------------------------
-- Time Enumeration
------------------------------------------------------------------------------

{-# INLINE_NORMAL times #-}
times :: MonadAsync m => Double -> Stream m (AbsTime, RelTime64)
times g = Stream step Nothing

    where

    {-# INLINE_LATE step #-}
    step _ Nothing = do
        clock <- liftIO $ asyncClock Monotonic g
        a <- liftIO $ readClock clock
        return $ Skip $ Just (clock, a)

    step _ s@(Just (clock, t0)) = do
        a <- liftIO $ readClock clock
        -- XXX we can perhaps use an AbsTime64 using a 64 bit Int for
        -- efficiency.  or maybe we can use a representation using Double for
        -- floating precision time
        return $ Yield (toAbsTime t0, toRelTime64 (a - t0)) s

-------------------------------------------------------------------------------
-- From Generators
-------------------------------------------------------------------------------

{-# INLINE_NORMAL fromIndicesM #-}
fromIndicesM :: Monad m => (Int -> m a) -> Stream m a
fromIndicesM gen = Stream step 0
  where
    {-# INLINE_LATE step #-}
    step _ i = do
       x <- gen i
       return $ Yield x (i + 1)

{-# INLINE fromIndices #-}
fromIndices :: Monad m => (Int -> a) -> Stream m a
fromIndices gen = fromIndicesM (return . gen)

-- Adapted from the vector package
{-# INLINE_NORMAL generateM #-}
generateM :: Monad m => Int -> (Int -> m a) -> Stream m a
generateM n gen = n `seq` Stream step 0
  where
    {-# INLINE_LATE step #-}
    step _ i | i < n     = do
                           x <- gen i
                           return $ Yield x (i + 1)
             | otherwise = return Stop

{-# INLINE generate #-}
generate :: Monad m => Int -> (Int -> a) -> Stream m a
generate n gen = generateM n (return . gen)

-------------------------------------------------------------------------------
-- Iteration
-------------------------------------------------------------------------------

{-# INLINE_NORMAL iterateM #-}
iterateM :: Monad m => (a -> m a) -> m a -> Stream m a
iterateM step = Stream (\_ st -> st >>= \x -> return $ Yield x (step x))

{-# INLINE_NORMAL iterate #-}
iterate :: Monad m => (a -> a) -> a -> Stream m a
iterate step st = iterateM (return . step) (return st)

-------------------------------------------------------------------------------
-- From containers
-------------------------------------------------------------------------------

-- XXX we need the MonadAsync constraint because of a rewrite rule.
-- | Convert a list of monadic actions to a 'Stream'
{-# INLINE_LATE fromListM #-}
fromListM :: MonadAsync m => [m a] -> Stream m a
fromListM = Stream step
  where
    {-# INLINE_LATE step #-}
    step _ (m:ms) = m >>= \x -> return $ Yield x ms
    step _ []     = return Stop
