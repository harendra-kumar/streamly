-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Transform
-- Copyright   : (c) 2018 Composewell Technologies
--               (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- "Streamly.Internal.Data.Pipe" might ultimately replace this module.

-- A few functions in this module have been adapted from the vector package
-- (c) Roman Leshchinskiy. See the notes in specific combinators.

module Streamly.Internal.Data.Stream.StreamD.Transform
    (
    -- * Piping
    -- | Pass through a 'Pipe'.
      transform

    -- * Mapping
    -- | Stateless one-to-one maps.
    , map
    , mapM
    , sequence

    -- * Mapping Effects
    , tap
    , tapOffsetEvery
    , tapRate
    , pollCounts

    -- * Folding
    , foldrS
    , foldrT
    , foldlS
    , foldlT

    -- * Scanning By 'Fold'
    , postscanOnce -- XXX rename to postscan
    , scanOnce     -- XXX rename to scan

    -- * Scanning
    -- | Left scans. Stateful, mostly one-to-one maps.
    , scanlM'
    , scanlMAfter'
    , scanl'
    , scanlM
    , scanl
    , scanl1M'
    , scanl1'
    , scanl1M
    , scanl1

    , prescanl'
    , prescanlM'

    , postscanl
    , postscanlM
    , postscanl'
    , postscanlM'
    , postscanlMAfter'

    , postscanlx'
    , postscanlMx'
    , scanlMx'
    , scanlx'

    -- * Filtering
    -- | Produce a subset of the stream.
    , filter
    , filterM
    , deleteBy
    , uniq

    -- * Trimming
    -- | Produce a subset of the stream trimmed at ends.
    , take
    , takeByTime
    , takeWhile
    , takeWhileM
    , drop
    , dropByTime
    , dropWhile
    , dropWhileM

    -- * Inserting Elements
    -- | Produce a superset of the stream.
    , insertBy
    , intersperse
    , intersperseM
    , intersperseSuffix
    , intersperseSuffixBySpan

    -- * Inserting Side Effects
    , intersperseM_
    , intersperseSuffix_

    -- * Reordering
    -- | Produce strictly the same set but reordered.
    , reverse
    , reverse'

    -- * Position Indexing
    , indexed
    , indexedR

    -- * Searching
    , findIndices

    -- * Rolling map
    -- | Map using the previous element.
    , rollingMap
    , rollingMapM

    -- * Maybe Streams
    , mapMaybe
    , mapMaybeM
    )
where

#include "inline.hs"

import Control.Concurrent (killThread, threadDelay)
import Control.Exception (AsyncException)
import Control.Monad (void, when)
import Control.Monad.Catch (MonadCatch, throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.IORef (newIORef, mkWeakIORef)
import Data.Maybe (fromJust, isJust)
import Foreign.Storable (Storable(..))
import GHC.Types (SPEC(..))
import qualified Control.Monad.Catch as MC

import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Pipe.Types (Pipe(..), PipeState(..))
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units
       (TimeUnit64, toRelTime64, diffAbsTime64)

import qualified Streamly.Internal.Data.Array.Foreign.Types as A
import qualified Streamly.Internal.Data.Fold.Types as FL
import qualified Streamly.Internal.Data.IORef.Prim as Prim
import qualified Streamly.Internal.Data.Pipe.Types as Pipe
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.StreamD.Generate as Generate
import qualified Streamly.Internal.Data.Stream.StreamD.Nesting as Nesting

import Prelude hiding
       ( drop, dropWhile, filter, map, mapM, reverse
       , scanl, scanl1, sequence, take, takeWhile)

import Streamly.Internal.Data.Stream.StreamD.Type
import Streamly.Internal.Data.SVar

------------------------------------------------------------------------------
-- Piping
------------------------------------------------------------------------------

{-# INLINE_NORMAL transform #-}
transform :: Monad m => Pipe m a b -> Stream m a -> Stream m b
transform (Pipe pstep1 pstep2 pstate) (Stream step state) =
    Stream step' (Consume pstate, state)

  where

    {-# INLINE_LATE step' #-}

    step' gst (Consume pst, st) = pst `seq` do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                res <- pstep1 pst x
                case res of
                    Pipe.Yield b pst' -> return $ Yield b (pst', s)
                    Pipe.Continue pst' -> return $ Skip (pst', s)
            Skip s -> return $ Skip (Consume pst, s)
            Stop   -> return Stop

    step' _ (Produce pst, st) = pst `seq` do
        res <- pstep2 pst
        case res of
            Pipe.Yield b pst' -> return $ Yield b (pst', st)
            Pipe.Continue pst' -> return $ Skip (pst', st)

------------------------------------------------------------------------------
-- Transformation Folds
------------------------------------------------------------------------------

{-# INLINE_NORMAL foldlT #-}
foldlT :: (Monad m, Monad (s m), MonadTrans s)
    => (s m b -> a -> s m b) -> s m b -> Stream m a -> s m b
foldlT fstep begin (Stream step state) = go SPEC begin state
  where
    go !_ acc st = do
        r <- lift $ step defState st
        case r of
            Yield x s -> go SPEC (fstep acc x) s
            Skip s -> go SPEC acc s
            Stop   -> acc

-- Note, this is going to have horrible performance, because of the nature of
-- the stream type (i.e. direct stream vs CPS). Its only for reference, it is
-- likely be practically unusable.
{-# INLINE_NORMAL foldlS #-}
foldlS :: Monad m
    => (Stream m b -> a -> Stream m b) -> Stream m b -> Stream m a -> Stream m b
foldlS fstep begin (Stream step state) = Stream step' (Left (state, begin))
  where
    step' gst (Left (st, acc)) = do
        r <- step (adaptState gst) st
        return $ case r of
            Yield x s -> Skip (Left (s, fstep acc x))
            Skip s -> Skip (Left (s, acc))
            Stop   -> Skip (Right acc)

    step' gst (Right (Stream stp stt)) = do
        r <- stp (adaptState gst) stt
        return $ case r of
            Yield x s -> Yield x (Right (Stream stp s))
            Skip s -> Skip (Right (Stream stp s))
            Stop   -> Stop

------------------------------------------------------------------------------
-- Transformation by Mapping
------------------------------------------------------------------------------

{-# INLINE_NORMAL sequence #-}
sequence :: Monad m => Stream m (m a) -> Stream m a
sequence (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' gst st = do
         r <- step (adaptState gst) st
         case r of
             Yield x s -> x >>= \a -> return (Yield a s)
             Skip s    -> return $ Skip s
             Stop      -> return Stop

------------------------------------------------------------------------------
-- Mapping side effects
------------------------------------------------------------------------------

data TapState fs st a
    = TapInit | Tapping !fs st | TapDone st

-- XXX Multiple yield points
{-# INLINE tap #-}
tap :: Monad m => Fold m a b -> Stream m a -> Stream m a
tap (Fold fstep initial extract) (Stream step state) = Stream step' TapInit

    where

    step' _ TapInit = do
        res <- initial
        return
            $ Skip
            $ case res of
                  FL.Partial s -> Tapping s state
                  FL.Done _ -> TapDone state
    step' gst (Tapping acc st) = do
        r <- step gst st
        case r of
            Yield x s -> do
                res <- fstep acc x
                return
                    $ Yield x
                    $ case res of
                          FL.Partial fs -> Tapping fs s
                          FL.Done _ -> TapDone s
            Skip s -> return $ Skip (Tapping acc s)
            Stop -> do
                void $ extract acc
                return Stop
    step' gst (TapDone st) = do
        r <- step gst st
        return
            $ case r of
                  Yield x s -> Yield x (TapDone s)
                  Skip s -> Skip (TapDone s)
                  Stop -> Stop

data TapOffState fs s a
    = TapOffInit
    | TapOffTapping !fs s Int
    | TapOffDone s

-- XXX Multiple yield points
{-# INLINE_NORMAL tapOffsetEvery #-}
tapOffsetEvery :: Monad m
    => Int -> Int -> Fold m a b -> Stream m a -> Stream m a
tapOffsetEvery offset n (Fold fstep initial extract) (Stream step state) =
    Stream step' TapOffInit

    where

    {-# INLINE_LATE step' #-}
    step' _ TapOffInit = do
        res <- initial
        return
            $ Skip
            $ case res of
                  FL.Partial s -> TapOffTapping s state (offset `mod` n)
                  FL.Done _ -> TapOffDone state
    step' gst (TapOffTapping acc st count) = do
        r <- step gst st
        case r of
            Yield x s -> do
                next <-
                    if count <= 0
                    then do
                        res <- fstep acc x
                        return
                            $ case res of
                                  FL.Partial sres ->
                                    TapOffTapping sres s (n - 1)
                                  FL.Done _ -> TapOffDone s
                    else return $ TapOffTapping acc s (count - 1)
                return $ Yield x next
            Skip s -> return $ Skip (TapOffTapping acc s count)
            Stop -> do
                void $ extract acc
                return Stop
    step' gst (TapOffDone st) = do
        r <- step gst st
        return
            $ case r of
                  Yield x s -> Yield x (TapOffDone s)
                  Skip s -> Skip (TapOffDone s)
                  Stop -> Stop

{-# INLINE_NORMAL pollCounts #-}
pollCounts
    :: MonadAsync m
    => (a -> Bool)
    -> (Stream m Int -> Stream m Int)
    -> Fold m Int b
    -> Stream m a
    -> Stream m a
pollCounts predicate transf fld (Stream step state) = Stream step' Nothing
  where

    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        -- As long as we are using an "Int" for counts lockfree reads from
        -- Var should work correctly on both 32-bit and 64-bit machines.
        -- However, an Int on a 32-bit machine may overflow quickly.
        countVar <- liftIO $ Prim.newIORef (0 :: Int)
        tid <- forkManaged
            $ void $ foldOnce fld
            $ transf $ Prim.toStreamD countVar
        return $ Skip (Just (countVar, tid, state))

    step' gst (Just (countVar, tid, st)) = do
        r <- step gst st
        case r of
            Yield x s -> do
                when (predicate x) $ liftIO $ Prim.modifyIORef' countVar (+ 1)
                return $ Yield x (Just (countVar, tid, s))
            Skip s -> return $ Skip (Just (countVar, tid, s))
            Stop -> do
                liftIO $ killThread tid
                return Stop

{-# INLINE_NORMAL tapRate #-}
tapRate ::
       (MonadAsync m, MonadCatch m)
    => Double
    -> (Int -> m b)
    -> Stream m a
    -> Stream m a
tapRate samplingRate action (Stream step state) = Stream step' Nothing
  where
    {-# NOINLINE loop #-}
    loop countVar prev = do
        i <-
            MC.catch
                (do liftIO $ threadDelay (round $ samplingRate * 1000000)
                    i <- liftIO $ Prim.readIORef countVar
                    let !diff = i - prev
                    void $ action diff
                    return i)
                (\(e :: AsyncException) -> do
                     i <- liftIO $ Prim.readIORef countVar
                     let !diff = i - prev
                     void $ action diff
                     throwM (MC.toException e))
        loop countVar i

    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        countVar <- liftIO $ Prim.newIORef 0
        tid <- fork $ loop countVar 0
        ref <- liftIO $ newIORef ()
        _ <- liftIO $ mkWeakIORef ref (killThread tid)
        return $ Skip (Just (countVar, tid, state, ref))

    step' gst (Just (countVar, tid, st, ref)) = do
        r <- step gst st
        case r of
            Yield x s -> do
                liftIO $ Prim.modifyIORef' countVar (+ 1)
                return $ Yield x (Just (countVar, tid, s, ref))
            Skip s -> return $ Skip (Just (countVar, tid, s, ref))
            Stop -> do
                liftIO $ killThread tid
                return Stop

------------------------------------------------------------------------------
-- Scanning with a Fold
------------------------------------------------------------------------------

data ScanState s f = ScanInit s | ScanDo s !f | ScanDone

{-# INLINE_NORMAL postscanOnce #-}
postscanOnce :: Monad m => FL.Fold m a b -> Stream m a -> Stream m b
postscanOnce (FL.Fold fstep initial extract) (Stream sstep state) =
    Stream step (ScanInit state)

    where

    {-# INLINE_LATE step #-}
    step _ (ScanInit st) = do
        res <- initial
        return
            $ case res of
                  FL.Partial fs -> Skip $ ScanDo st fs
                  FL.Done b -> Yield b ScanDone
    step gst (ScanDo st fs) = do
        res <- sstep (adaptState gst) st
        case res of
            Yield x s -> do
                r <- fstep fs x
                case r of
                    FL.Partial fs1 -> do
                        !b <- extract fs1
                        return $ Yield b $ ScanDo s fs1
                    FL.Done b -> return $ Yield b ScanDone
            Skip s -> return $ Skip $ ScanDo s fs
            Stop -> return Stop
    step _ ScanDone = return Stop

{-# INLINE scanOnce #-}
scanOnce :: Monad m
    => FL.Fold m a b -> Stream m a -> Stream m b
scanOnce (FL.Fold fstep initial extract) (Stream sstep state) =
    Stream step (ScanInit state)

    where

    {-# INLINE_LATE step #-}
    step _ (ScanInit st) = do
        res <- initial
        case res of
            FL.Partial fs -> do
                !b <- extract fs
                return $ Yield b $ ScanDo st fs
            FL.Done b -> return $ Yield b ScanDone
    step gst (ScanDo st fs) = do
        res <- sstep (adaptState gst) st
        case res of
            Yield x s -> do
                r <- fstep fs x
                case r of
                    FL.Partial fs1 -> do
                        !b <- extract fs1
                        return $ Yield b $ ScanDo s fs1
                    FL.Done b -> return $ Yield b ScanDone
            Skip s -> return $ Skip $ ScanDo s fs
            Stop -> return Stop
    step _ ScanDone = return Stop

------------------------------------------------------------------------------
-- Scanning - Prescans
------------------------------------------------------------------------------

-- Adapted from the vector package.
--
-- XXX Is a prescan useful, discarding the last step does not sound useful?  I
-- am not sure about the utility of this function, so this is implemented but
-- not exposed. We can expose it if someone provides good reasons why this is
-- useful.
--
-- XXX We have to execute the stream one step ahead to know that we are at the
-- last step.  The vector implementation of prescan executes the last fold step
-- but does not yield the result. This means we have executed the effect but
-- discarded value. This does not sound right. In this implementation we are
-- not executing the last fold step.
{-# INLINE_NORMAL prescanlM' #-}
prescanlM' :: Monad m => (b -> a -> m b) -> m b -> Stream m a -> Stream m b
prescanlM' f mz (Stream step state) = Stream step' (state, mz)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, prev) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                acc <- prev
                return $ Yield acc (s, f acc x)
            Skip s -> return $ Skip (s, prev)
            Stop   -> return Stop

{-# INLINE prescanl' #-}
prescanl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> Stream m b
prescanl' f z = prescanlM' (\a b -> return (f a b)) (return z)

------------------------------------------------------------------------------
-- Monolithic postscans (postscan followed by a map)
------------------------------------------------------------------------------

-- The performance of a modular postscan followed by a map seems to be
-- equivalent to this monolithic scan followed by map therefore we may not need
-- this implementation. We just have it for performance comparison and in case
-- modular version does not perform well in some situation.
--
{-# INLINE_NORMAL postscanlMx' #-}
postscanlMx' :: Monad m
    => (x -> a -> m x) -> m x -> (x -> m b) -> Stream m a -> Stream m b
postscanlMx' fstep begin done (Stream step state) = do
    Stream step' (state, begin)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, acc) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                old <- acc
                y <- fstep old x
                v <- done y
                v `seq` y `seq` return (Yield v (s, return y))
            Skip s -> return $ Skip (s, acc)
            Stop   -> return Stop

{-# INLINE_NORMAL postscanlx' #-}
postscanlx' :: Monad m
    => (x -> a -> x) -> x -> (x -> b) -> Stream m a -> Stream m b
postscanlx' fstep begin done s =
    postscanlMx' (\b a -> return (fstep b a)) (return begin) (return . done) s

-- XXX do we need consM strict to evaluate the begin value?
{-# INLINE scanlMx' #-}
scanlMx' :: Monad m
    => (x -> a -> m x) -> m x -> (x -> m b) -> Stream m a -> Stream m b
scanlMx' fstep begin done s =
    (begin >>= \x -> x `seq` done x) `consM` postscanlMx' fstep begin done s

{-# INLINE scanlx' #-}
scanlx' :: Monad m
    => (x -> a -> x) -> x -> (x -> b) -> Stream m a -> Stream m b
scanlx' fstep begin done s =
    scanlMx' (\b a -> return (fstep b a)) (return begin) (return . done) s

------------------------------------------------------------------------------
-- postscans
------------------------------------------------------------------------------

-- Adapted from the vector package.
{-# INLINE_NORMAL postscanlM' #-}
postscanlM' :: Monad m => (b -> a -> m b) -> m b -> Stream m a -> Stream m b
postscanlM' fstep begin (Stream step state) =
    Stream step' Nothing
  where
    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        !x <- begin
        return $ Skip (Just (state, x))

    step' gst (Just (st, acc)) =  do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                !y <- fstep acc x
                return $ Yield y (Just (s, y))
            Skip s -> return $ Skip (Just (s, acc))
            Stop   -> return Stop

{-# INLINE_NORMAL postscanl' #-}
postscanl' :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
postscanl' f seed = postscanlM' (\a b -> return (f a b)) (return seed)

-- We can possibly have the "done" function as a Maybe to provide an option to
-- emit or not emit the accumulator when the stream stops.
--
-- TBD: use a single Yield point
--
{-# INLINE_NORMAL postscanlMAfter' #-}
postscanlMAfter' :: Monad m
    => (b -> a -> m b) -> m b -> (b -> m b) -> Stream m a -> Stream m b
postscanlMAfter' fstep initial done (Stream step1 state1) = do
    Stream step (Just (state1, initial))

    where

    {-# INLINE_LATE step #-}
    step gst (Just (st, acc)) = do
        r <- step1 (adaptState gst) st
        case r of
            Yield x s -> do
                old <- acc
                y <- fstep old x
                y `seq` return (Yield y (Just (s, return y)))
            Skip s -> return $ Skip $ Just (s, acc)
            Stop -> acc >>= done >>= \res -> return (Yield res Nothing)
    step _ Nothing = return Stop

{-# INLINE_NORMAL postscanlM #-}
postscanlM :: Monad m => (b -> a -> m b) -> m b -> Stream m a -> Stream m b
postscanlM fstep begin (Stream step state) = Stream step' Nothing
  where
    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        r <- begin
        return $ Skip (Just (state, r))

    step' gst (Just (st, acc)) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                y <- fstep acc x
                return (Yield y (Just (s, y)))
            Skip s -> return $ Skip (Just (s, acc))
            Stop   -> return Stop

{-# INLINE_NORMAL postscanl #-}
postscanl :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
postscanl f seed = postscanlM (\a b -> return (f a b)) (return seed)

{-# INLINE_NORMAL scanlM' #-}
scanlM' :: Monad m => (b -> a -> m b) -> m b -> Stream m a -> Stream m b
scanlM' fstep begin (Stream step state) = Stream step' Nothing
  where
    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        !x <- begin
        return $ Yield x (Just (state, x))
    step' gst (Just (st, acc)) =  do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                !y <- fstep acc x
                return $ Yield y (Just (s, y))
            Skip s -> return $ Skip (Just (s, acc))
            Stop   -> return Stop

{-# INLINE scanlMAfter' #-}
scanlMAfter' :: Monad m
    => (b -> a -> m b) -> m b -> (b -> m b) -> Stream m a -> Stream m b
scanlMAfter' fstep initial done s =
    (initial >>= \x -> x `seq` return x) `consM`
        postscanlMAfter' fstep initial done s

{-# INLINE scanl' #-}
scanl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> Stream m b
scanl' f seed = scanlM' (\a b -> return (f a b)) (return seed)

{-# INLINE_NORMAL scanlM #-}
scanlM :: Monad m => (b -> a -> m b) -> m b -> Stream m a -> Stream m b
scanlM fstep begin (Stream step state) = Stream step' Nothing
  where
    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        x <- begin
        return $ Yield x (Just (state, x))
    step' gst (Just (st, acc)) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                y <- fstep acc x
                return $ Yield y (Just (s, y))
            Skip s -> return $ Skip (Just (s, acc))
            Stop   -> return $ Stop

{-# INLINE scanl #-}
scanl :: Monad m => (b -> a -> b) -> b -> Stream m a -> Stream m b
scanl f seed = scanlM (\a b -> return (f a b)) (return seed)

-- Adapted from the vector package
{-# INLINE_NORMAL scanl1M #-}
scanl1M :: Monad m => (a -> a -> m a) -> Stream m a -> Stream m a
scanl1M fstep (Stream step state) = Stream step' (state, Nothing)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, Nothing) = do
        r <- step gst st
        case r of
            Yield x s -> return $ Yield x (s, Just x)
            Skip s -> return $ Skip (s, Nothing)
            Stop   -> return Stop

    step' gst (st, Just acc) = do
        r <- step gst st
        case r of
            Yield y s -> do
                z <- fstep acc y
                return $ Yield z (s, Just z)
            Skip s -> return $ Skip (s, Just acc)
            Stop   -> return Stop

{-# INLINE scanl1 #-}
scanl1 :: Monad m => (a -> a -> a) -> Stream m a -> Stream m a
scanl1 f = scanl1M (\x y -> return (f x y))

-- Adapted from the vector package
{-# INLINE_NORMAL scanl1M' #-}
scanl1M' :: Monad m => (a -> a -> m a) -> Stream m a -> Stream m a
scanl1M' fstep (Stream step state) = Stream step' (state, Nothing)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, Nothing) = do
        r <- step gst st
        case r of
            Yield x s -> x `seq` return $ Yield x (s, Just x)
            Skip s -> return $ Skip (s, Nothing)
            Stop   -> return Stop

    step' gst (st, Just acc) = acc `seq` do
        r <- step gst st
        case r of
            Yield y s -> do
                z <- fstep acc y
                z `seq` return $ Yield z (s, Just z)
            Skip s -> return $ Skip (s, Just acc)
            Stop   -> return Stop

{-# INLINE scanl1' #-}
scanl1' :: Monad m => (a -> a -> a) -> Stream m a -> Stream m a
scanl1' f = scanl1M' (\x y -> return (f x y))

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

-- Adapted from the vector package
{-# INLINE_NORMAL filterM #-}
filterM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
filterM f (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- step gst st
        case r of
            Yield x s -> do
                b <- f x
                return $ if b
                         then Yield x s
                         else Skip s
            Skip s -> return $ Skip s
            Stop   -> return Stop

{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
filter f = filterM (return . f)

-- Adapted from the vector package
{-# INLINE_NORMAL uniq #-}
uniq :: (Eq a, Monad m) => Stream m a -> Stream m a
uniq (Stream step state) = Stream step' (Nothing, state)
  where
    {-# INLINE_LATE step' #-}
    step' gst (Nothing, st) = do
        r <- step gst st
        case r of
            Yield x s -> return $ Yield x (Just x, s)
            Skip  s   -> return $ Skip  (Nothing, s)
            Stop      -> return Stop
    step' gst (Just x, st)  = do
         r <- step gst st
         case r of
             Yield y s | x == y   -> return $ Skip (Just x, s)
                       | otherwise -> return $ Yield y (Just y, s)
             Skip  s   -> return $ Skip (Just x, s)
             Stop      -> return Stop

{-# INLINE_NORMAL deleteBy #-}
deleteBy :: Monad m => (a -> a -> Bool) -> a -> Stream m a -> Stream m a
deleteBy eq x (Stream step state) = Stream step' (state, False)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, False) = do
        r <- step gst st
        case r of
            Yield y s -> return $
                if eq x y then Skip (s, True) else Yield y (s, False)
            Skip s -> return $ Skip (s, False)
            Stop   -> return Stop

    step' gst (st, True) = do
        r <- step gst st
        case r of
            Yield y s -> return $ Yield y (s, True)
            Skip s -> return $ Skip (s, True)
            Stop   -> return Stop

------------------------------------------------------------------------------
-- Trimming
------------------------------------------------------------------------------

-- XXX using getTime in the loop can be pretty expensive especially for
-- computations where iterations are lightweight. We have the following
-- options:
--
-- 1) Run a timeout thread updating a flag asynchronously and check that
-- flag here, that way we can have a cheap termination check.
--
-- 2) Use COARSE clock to get time with lower resolution but more efficiently.
--
-- 3) Use rdtscp/rdtsc to get time directly from the processor, compute the
-- termination value of rdtsc in the beginning and then in each iteration just
-- get rdtsc and check if we should terminate.
--
data TakeByTime st s
    = TakeByTimeInit st
    | TakeByTimeCheck st s
    | TakeByTimeYield st s

{-# INLINE_NORMAL takeByTime #-}
takeByTime :: (MonadIO m, TimeUnit64 t) => t -> Stream m a -> Stream m a
takeByTime duration (Stream step1 state1) = Stream step (TakeByTimeInit state1)
    where

    lim = toRelTime64 duration

    {-# INLINE_LATE step #-}
    step _ (TakeByTimeInit _) | lim == 0 = return Stop
    step _ (TakeByTimeInit st) = do
        t0 <- liftIO $ getTime Monotonic
        return $ Skip (TakeByTimeYield st t0)
    step _ (TakeByTimeCheck st t0) = do
        t <- liftIO $ getTime Monotonic
        return $
            if diffAbsTime64 t t0 > lim
            then Stop
            else Skip (TakeByTimeYield st t0)
    step gst (TakeByTimeYield st t0) = do
        r <- step1 gst st
        return $ case r of
             Yield x s -> Yield x (TakeByTimeCheck s t0)
             Skip s -> Skip (TakeByTimeCheck s t0)
             Stop -> Stop

{-# INLINE_NORMAL dropByTime #-}
dropByTime :: (MonadAsync m, TimeUnit64 t) => t -> Stream m a -> Stream m a
dropByTime duration =
    map snd
        . dropWhile (\(t, _) -> t <= duration64)
        . Nesting.zipWith (,) relTimes

    where

    relTimes = map snd (Generate.times 0.01)
    duration64 = toRelTime64 duration

-- Adapted from the vector package
{-# INLINE_NORMAL drop #-}
drop :: Monad m => Int -> Stream m a -> Stream m a
drop n (Stream step state) = Stream step' (state, Just n)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, Just i)
      | i > 0 = do
          r <- step gst st
          return $
            case r of
              Yield _ s -> Skip (s, Just (i - 1))
              Skip s    -> Skip (s, Just i)
              Stop      -> Stop
      | otherwise = return $ Skip (st, Nothing)

    step' gst (st, Nothing) = do
      r <- step gst st
      return $
        case r of
          Yield x s -> Yield x (s, Nothing)
          Skip  s   -> Skip (s, Nothing)
          Stop      -> Stop

-- Adapted from the vector package
data DropWhileState s a
    = DropWhileDrop s
    | DropWhileYield a s
    | DropWhileNext s

{-# INLINE_NORMAL dropWhileM #-}
dropWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
dropWhileM f (Stream step state) = Stream step' (DropWhileDrop state)
  where
    {-# INLINE_LATE step' #-}
    step' gst (DropWhileDrop st) = do
        r <- step gst st
        case r of
            Yield x s -> do
                b <- f x
                if b
                then return $ Skip (DropWhileDrop s)
                else return $ Skip (DropWhileYield x s)
            Skip s -> return $ Skip (DropWhileDrop s)
            Stop -> return Stop

    step' gst (DropWhileNext st) =  do
        r <- step gst st
        case r of
            Yield x s -> return $ Skip (DropWhileYield x s)
            Skip s    -> return $ Skip (DropWhileNext s)
            Stop      -> return Stop

    step' _ (DropWhileYield x st) = return $ Yield x (DropWhileNext st)

{-# INLINE dropWhile #-}
dropWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
dropWhile f = dropWhileM (return . f)

------------------------------------------------------------------------------
-- Inserting Elements
------------------------------------------------------------------------------

{-# INLINE_NORMAL insertBy #-}
insertBy :: Monad m => (a -> a -> Ordering) -> a -> Stream m a -> Stream m a
insertBy cmp a (Stream step state) = Stream step' (state, False, Nothing)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, False, _) = do
        r <- step gst st
        case r of
            Yield x s -> case cmp a x of
                GT -> return $ Yield x (s, False, Nothing)
                _  -> return $ Yield a (s, True, Just x)
            Skip s -> return $ Skip (s, False, Nothing)
            Stop   -> return $ Yield a (st, True, Nothing)

    step' _ (_, True, Nothing) = return Stop

    step' gst (st, True, Just prev) = do
        r <- step gst st
        case r of
            Yield x s -> return $ Yield prev (s, True, Just x)
            Skip s    -> return $ Skip (s, True, Just prev)
            Stop      -> return $ Yield prev (st, True, Nothing)

data LoopState x s = FirstYield s
                   | InterspersingYield s
                   | YieldAndCarry x s

{-# INLINE_NORMAL intersperseM #-}
intersperseM :: Monad m => m a -> Stream m a -> Stream m a
intersperseM m (Stream step state) = Stream step' (FirstYield state)
  where
    {-# INLINE_LATE step' #-}
    step' gst (FirstYield st) = do
        r <- step gst st
        return $
            case r of
                Yield x s -> Skip (YieldAndCarry x s)
                Skip s -> Skip (FirstYield s)
                Stop -> Stop

    step' gst (InterspersingYield st) = do
        r <- step gst st
        case r of
            Yield x s -> do
                a <- m
                return $ Yield a (YieldAndCarry x s)
            Skip s -> return $ Skip $ InterspersingYield s
            Stop -> return Stop

    step' _ (YieldAndCarry x st) = return $ Yield x (InterspersingYield st)

{-# INLINE intersperse #-}
intersperse :: Monad m => a -> Stream m a -> Stream m a
intersperse a = intersperseM (return a)

{-# INLINE_NORMAL intersperseM_ #-}
intersperseM_ :: Monad m => m b -> Stream m a -> Stream m a
intersperseM_ m (Stream step1 state1) = Stream step (Left (pure (), state1))
  where
    {-# INLINE_LATE step #-}
    step gst (Left (eff, st)) = do
        r <- step1 gst st
        case r of
            Yield x s -> eff >> return (Yield x (Right s))
            Skip s -> return $ Skip (Left (eff, s))
            Stop -> return Stop

    step _ (Right st) = return $ Skip $ Left (void m, st)

data SuffixState s a
    = SuffixElem s
    | SuffixSuffix s
    | SuffixYield a (SuffixState s a)

{-# INLINE_NORMAL intersperseSuffix #-}
intersperseSuffix :: forall m a. Monad m => m a -> Stream m a -> Stream m a
intersperseSuffix action (Stream step state) = Stream step' (SuffixElem state)
    where
    {-# INLINE_LATE step' #-}
    step' gst (SuffixElem st) = do
        r <- step gst st
        return $ case r of
            Yield x s -> Skip (SuffixYield x (SuffixSuffix s))
            Skip s -> Skip (SuffixElem s)
            Stop -> Stop

    step' _ (SuffixSuffix st) = do
        action >>= \r -> return $ Skip (SuffixYield r (SuffixElem st))

    step' _ (SuffixYield x next) = return $ Yield x next

{-# INLINE_NORMAL intersperseSuffix_ #-}
intersperseSuffix_ :: Monad m => m b -> Stream m a -> Stream m a
intersperseSuffix_ m (Stream step1 state1) = Stream step (Left state1)
  where
    {-# INLINE_LATE step #-}
    step gst (Left st) = do
        r <- step1 gst st
        case r of
            Yield x s -> return $ Yield x (Right s)
            Skip s -> return $ Skip $ Left s
            Stop -> return Stop

    step _ (Right st) = m >> return (Skip (Left st))

data SuffixSpanState s a
    = SuffixSpanElem s Int
    | SuffixSpanSuffix s
    | SuffixSpanYield a (SuffixSpanState s a)
    | SuffixSpanLast
    | SuffixSpanStop

-- | intersperse after every n items
{-# INLINE_NORMAL intersperseSuffixBySpan #-}
intersperseSuffixBySpan :: forall m a. Monad m
    => Int -> m a -> Stream m a -> Stream m a
intersperseSuffixBySpan n action (Stream step state) =
    Stream step' (SuffixSpanElem state n)
    where
    {-# INLINE_LATE step' #-}
    step' gst (SuffixSpanElem st i) | i > 0 = do
        r <- step gst st
        return $ case r of
            Yield x s -> Skip (SuffixSpanYield x (SuffixSpanElem s (i - 1)))
            Skip s -> Skip (SuffixSpanElem s i)
            Stop -> if i == n then Stop else Skip SuffixSpanLast
    step' _ (SuffixSpanElem st _) = return $ Skip (SuffixSpanSuffix st)

    step' _ (SuffixSpanSuffix st) = do
        action >>= \r -> return $ Skip (SuffixSpanYield r (SuffixSpanElem st n))

    step' _ (SuffixSpanLast) = do
        action >>= \r -> return $ Skip (SuffixSpanYield r SuffixSpanStop)

    step' _ (SuffixSpanYield x next) = return $ Yield x next

    step' _ (SuffixSpanStop) = return Stop

------------------------------------------------------------------------------
-- Reordering
------------------------------------------------------------------------------

-- We can implement reverse as:
--
-- > reverse = foldlS (flip cons) nil
--
-- However, this implementation is unusable because of the horrible performance
-- of cons. So we just convert it to a list first and then stream from the
-- list.
--
-- XXX Maybe we can use an Array instead of a list here?
{-# INLINE_NORMAL reverse #-}
reverse :: Monad m => Stream m a -> Stream m a
reverse m = Stream step Nothing
    where
    {-# INLINE_LATE step #-}
    step _ Nothing = do
        xs <- foldl' (flip (:)) [] m
        return $ Skip (Just xs)
    step _ (Just (x:xs)) = return $ Yield x (Just xs)
    step _ (Just []) = return Stop

-- Much faster reverse for Storables
{-# INLINE_NORMAL reverse' #-}
reverse' :: forall m a. (MonadIO m, Storable a) => Stream m a -> Stream m a
{-
-- This commented implementation copies the whole stream into one single array
-- and then streams from that array, this has exactly the same performance as
-- the chunked code that follows.  Though this could be problematic due to
-- unbounded large allocations. However, if we use an idiomatic implementation
-- of arraysOf instead of the custom implementation then the chunked code
-- becomes worse by 6 times. Need to investigate if that can be improved.
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (Ptr, plusPtr)
reverse' m = Stream step Nothing
    where
    {-# INLINE_LATE step #-}
    step _ Nothing = do
        arr <- A.fromStreamD m
        let p = A.aEnd arr `plusPtr` negate (sizeOf (undefined :: a))
        return $ Skip $ Just (A.aStart arr, p)

    step _ (Just (start, p)) | p < unsafeForeignPtrToPtr start = return Stop

    step _ (Just (start, p)) = do
        let !x = A.unsafeInlineIO $ do
                    r <- peek p
                    touchForeignPtr start
                    return r
            next = p `plusPtr` negate (sizeOf (undefined :: a))
        return $ Yield x (Just (start, next))
-}
reverse' =
          A.flattenArraysRev -- concatUnfold A.readRev
        . fromStreamK
        . K.reverse
        . toStreamK
        . A.arraysOf A.defaultChunkSize

------------------------------------------------------------------------------
-- Position Indexing
------------------------------------------------------------------------------

-- Adapted from the vector package
{-# INLINE_NORMAL indexed #-}
indexed :: Monad m => Stream m a -> Stream m (Int, a)
indexed (Stream step state) = Stream step' (state, 0)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, i) = i `seq` do
         r <- step (adaptState gst) st
         case r of
             Yield x s -> return $ Yield (i, x) (s, i+1)
             Skip    s -> return $ Skip (s, i)
             Stop      -> return Stop

-- Adapted from the vector package
{-# INLINE_NORMAL indexedR #-}
indexedR :: Monad m => Int -> Stream m a -> Stream m (Int, a)
indexedR m (Stream step state) = Stream step' (state, m)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, i) = i `seq` do
         r <- step (adaptState gst) st
         case r of
             Yield x s -> let i' = i - 1
                          in return $ Yield (i, x) (s, i')
             Skip    s -> return $ Skip (s, i)
             Stop      -> return Stop

------------------------------------------------------------------------------
-- Searching
------------------------------------------------------------------------------

{-# INLINE_NORMAL findIndices #-}
findIndices :: Monad m => (a -> Bool) -> Stream m a -> Stream m Int
findIndices p (Stream step state) = Stream step' (state, 0)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, i) = i `seq` do
      r <- step (adaptState gst) st
      return $ case r of
          Yield x s -> if p x then Yield i (s, i+1) else Skip (s, i+1)
          Skip s -> Skip (s, i)
          Stop   -> Stop

------------------------------------------------------------------------------
-- Rolling map
------------------------------------------------------------------------------

data RollingMapState s a = RollingMapInit s | RollingMapGo s a

{-# INLINE rollingMapM #-}
rollingMapM :: Monad m => (a -> a -> m b) -> Stream m a -> Stream m b
rollingMapM f (Stream step1 state1) = Stream step (RollingMapInit state1)
    where
    step gst (RollingMapInit st) = do
        r <- step1 (adaptState gst) st
        return $ case r of
            Yield x s -> Skip $ RollingMapGo s x
            Skip s -> Skip $ RollingMapInit s
            Stop   -> Stop

    step gst (RollingMapGo s1 x1) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield x s -> do
                !res <- f x x1
                return $ Yield res $ RollingMapGo s x
            Skip s -> return $ Skip $ RollingMapGo s x1
            Stop   -> return $ Stop

{-# INLINE rollingMap #-}
rollingMap :: Monad m => (a -> a -> b) -> Stream m a -> Stream m b
rollingMap f = rollingMapM (\x y -> return $ f x y)

------------------------------------------------------------------------------
-- Maybe Streams
------------------------------------------------------------------------------

-- XXX Will this always fuse properly?
{-# INLINE_NORMAL mapMaybe #-}
mapMaybe :: Monad m => (a -> Maybe b) -> Stream m a -> Stream m b
mapMaybe f = fmap fromJust . filter isJust . map f

{-# INLINE_NORMAL mapMaybeM #-}
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Stream m a -> Stream m b
mapMaybeM f = fmap fromJust . filter isJust . mapM f
