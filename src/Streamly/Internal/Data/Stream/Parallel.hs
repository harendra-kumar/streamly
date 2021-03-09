{-# LANGUAGE UndecidableInstances #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.Parallel
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Internal.Data.Stream.Parallel
    (
    -- * Parallel Stream Type
      ParallelT
    , Parallel
    , parallely

    -- * Merge Concurrently
    , parallel
    , parallelFst
    , parallelMin

    -- * Evaluate Concurrently
    , mkParallel
    , mkParallelD
    , mkParallelK

    -- * Tap Concurrently
    , tapAsync
    , tapAsyncF
    , distributeAsync_

    -- * Callbacks
    , newCallbackStream
    )
where

import Control.Concurrent (myThreadId, takeMVar)
import Control.Monad (when)
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Catch (MonadThrow, throwM)
-- import Control.Monad.Error.Class   (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Functor (void)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromJust)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import Prelude hiding (map)

import qualified Data.Set as Set

import Streamly.Internal.Data.Fold.Types (Fold)
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))
import Streamly.Internal.Data.Stream.StreamK
       (IsStream(..), Stream, mkStream, foldStream, foldStreamShared, adapt)

import Streamly.Internal.Data.SVar

import qualified Streamly.Internal.Data.Stream.StreamK as K (withLocal)
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.SVar as SVar

#include "Instances.hs"

--
-- $setup
-- >>> :m
-- >>> import Prelude hiding (map)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import Streamly.Internal.Data.Stream.IsStream as Stream

-------------------------------------------------------------------------------
-- Parallel
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- StreamK based worker routines
-------------------------------------------------------------------------------

{-# NOINLINE runOne #-}
runOne
    :: MonadIO m
    => State Stream m a -> Stream m a -> Maybe WorkerInfo -> m ()
runOne st m0 winfo =
    case getYieldLimit st of
        Nothing -> go m0
        Just _  -> runOneLimited st m0 winfo

    where

    go m = do
        liftIO $ decrementBufferLimit sv
        foldStreamShared st yieldk single stop m

    sv = fromJust $ streamVar st

    stop = liftIO $ do
        incrementBufferLimit sv
        sendStop sv winfo
    sendit a = liftIO $ void $ send sv (ChildYield a)
    single a = sendit a >> (liftIO $ sendStop sv winfo)
    yieldk a r = sendit a >> go r

runOneLimited
    :: MonadIO m
    => State Stream m a -> Stream m a -> Maybe WorkerInfo -> m ()
runOneLimited st m0 winfo = go m0

    where

    go m = do
        yieldLimitOk <- liftIO $ decrementYieldLimit sv
        if yieldLimitOk
        then do
            liftIO $ decrementBufferLimit sv
            foldStreamShared st yieldk single stop m
        else do
            liftIO $ cleanupSVarFromWorker sv
            liftIO $ sendStop sv winfo

    sv = fromJust $ streamVar st

    stop = liftIO $ do
        incrementBufferLimit sv
        incrementYieldLimit sv
        sendStop sv winfo
    sendit a = liftIO $ void $ send sv (ChildYield a)
    single a = sendit a >> (liftIO $ sendStop sv winfo)
    yieldk a r = sendit a >> go r

-------------------------------------------------------------------------------
-- Consing and appending a stream in parallel style
-------------------------------------------------------------------------------

-- Note that consing and appending requires StreamK as it would not scale well
-- with StreamD unless we are only consing a very small number of streams or
-- elements in a stream. StreamK allows us to manipulate control flow in a way
-- which StreamD cannot allow. StreamK can make a jump without having to
-- remember the past state.

{-# NOINLINE forkSVarPar #-}
forkSVarPar :: (IsStream t, MonadAsync m)
    => SVarStopStyle -> t m a -> t m a -> t m a
forkSVarPar ss m r = mkStream $ \st yld sng stp -> do
    sv <- newParallelVar ss st
    pushWorkerPar sv (runOne st{streamVar = Just sv} $ toStream m)
    case ss of
        StopBy -> liftIO $ do
            set <- readIORef (workerThreads sv)
            writeIORef (svarStopBy sv) $ Set.elemAt 0 set
        _ -> return ()
    pushWorkerPar sv (runOne st{streamVar = Just sv} $ toStream r)
    foldStream st yld sng stp (SVar.fromSVar sv)

{-# INLINE joinStreamVarPar #-}
joinStreamVarPar :: (IsStream t, MonadAsync m)
    => SVarStyle -> SVarStopStyle -> t m a -> t m a -> t m a
joinStreamVarPar style ss m1 m2 = mkStream $ \st yld sng stp ->
    case streamVar st of
        Just sv | svarStyle sv == style && svarStopStyle sv == ss -> do
            -- Here, WE ARE IN THE WORKER/PRODUCER THREAD, we know that because
            -- the SVar exists. We are running under runOne and the output we
            -- produce ultimately will be sent to the SVar by runOne.
            --
            -- If we came here the worker/runOne is evaluating a `parallel`
            -- combinator. In this case, we always fork a new worker for the
            -- first component (m1) in the parallel composition and continue to
            -- evaluate the second component (m2) in the current worker thread.
            --
            -- When m1 is serially composed, the worker would evaluate it
            -- without any further forks and the resulting output is sent to
            -- the SVar and the evaluation terminates. If m1 is a `parallel`
            -- composition of two streams the worker would again recurses here.
            --
            -- Similarly, when m2 is serially composed it gets evaluated here
            -- and the resulting output is sent to the SVar by the runOne
            -- wrapper. When m2 is composed with `parallel` it will again
            -- recurse here and so on until it finally terminates.
            --
            -- When we create a right associated expression using `parallel`,
            -- then m1 would always terminate without further forks or
            -- recursion into this routine, therefore, the worker returns
            -- immediately after evaluating it. And m2 would continue to
            -- fork/recurse, therefore, the current thread always recurses and
            -- forks new workers one after the other.  This is a tail recursive
            -- style execution, m2, the recursive expression always executed at
            -- the tail.
            --
            -- When the expression is left associated, the worker spawned would
            -- get the forking/recursing responsibility and then again the
            -- worker spawned by that worker would fork, thus creating layer
            -- over layer of workers and a chain of threads leading to a very
            -- inefficient execution.
            pushWorkerPar sv (runOne st $ toStream m1)
            foldStreamShared st yld sng stp m2
        _ ->
            -- Here WE ARE IN THE CONSUMER THREAD, we create a new SVar, fork
            -- worker threads to execute m1 and m2 and this thread starts
            -- pulling the stream from the SVar.
            foldStreamShared st yld sng stp (forkSVarPar ss m1 m2)

-------------------------------------------------------------------------------
-- User facing APIs
-------------------------------------------------------------------------------

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using parallel.
{-# INLINE consMParallel #-}
{-# SPECIALIZE consMParallel :: IO a -> ParallelT IO a -> ParallelT IO a #-}
consMParallel :: MonadAsync m => m a -> ParallelT m a -> ParallelT m a
consMParallel m r = fromStream $ K.valueM m `parallel` (toStream r)

infixr 6 `parallel`

-- | Execute two streams concurrently and merge their outputs.  For example, if
-- stream @a@ is a serial stream consisting of @a1, a2, a3@ and stream @b@ is a
-- serial stream consisting of @b1, b2, b3@ then stream @a `parallel` b@ may
-- produce @a1, b1, a2, b2, a3, b3@ or @a1, a2, b1, a3, b2, b3@ or some other
-- combination depending on the rate at which the two streams produce elements.
-- However, the relative order of outputs from a single stream e.g. @a1, a2,
-- a3@ would remain the same in the resulting stream. The effects in the two
-- streams may occur concurrently.
--
-- To run single actions (instead of streams) in parallel wrap them into
-- singleton streams.  The following trivial example is semantically equivalent
-- to running the action @putStrLn "hello"@ in the current thread:
--
-- >>> Stream.toList $ Stream.valueM (putStrLn "hello") `Stream.parallel` Stream.nil
-- hello
-- [()]
--
-- Run two actions concurrently:
--
-- >>> import Control.Concurrent (threadDelay)
-- >>> Stream.toList $ Stream.valueM (putStrLn "hello") `Stream.parallel` Stream.valueM (threadDelay 100000 >> putStrLn "world")
-- hello
-- world
-- [(),()]
--
-- Run effects concurrently, disregarding their outputs:
--
-- >>> Stream.toList $ nilM (putStrLn "hello") `parallel` Stream.nilM (threadDelay 100000 >> putStrLn "world")
-- hello
-- world
-- []
--
-- Run an effectful action, and a pure effect without any output, concurrently:
--
-- >>> Stream.toList $ Stream.valueM (return 1) `Stream.parallel` Stream.nilM (putStrLn "world")
-- world
-- [1]
--
-- Note that 'parallel' is a polymorphic version of the @Semigroup@ operation
-- @<>@ of 'ParallelT'.
--
-- `nilM` is currently @Internal@.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE parallel #-}
parallel :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
parallel = joinStreamVarPar ParallelVar StopNone

-- This is a co-parallel like combinator for streams, where first stream is the
-- main stream and the rest are just supporting it, when the first ends
-- everything ends.
--
-- | Like `parallel` but stops the output as soon as the first stream stops.
--
-- /Pre-release/
{-# INLINE parallelFst #-}
parallelFst :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
parallelFst = joinStreamVarPar ParallelVar StopBy

-- This is a race like combinator for streams.
--
-- | Like `parallel` but stops the output as soon as any of the two streams
-- stops.
--
-- /Pre-release/
{-# INLINE parallelMin #-}
parallelMin :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
parallelMin = joinStreamVarPar ParallelVar StopAny

------------------------------------------------------------------------------
-- Convert a stream to parallel
------------------------------------------------------------------------------

-- | Like 'mkParallel' but uses StreamK internally.
--
-- /Pre-release/
--
mkParallelK :: (IsStream t, MonadAsync m) => t m a -> t m a
mkParallelK m = mkStream $ \st yld sng stp -> do
    sv <- newParallelVar StopNone (adaptState st)
    -- pushWorkerPar sv (runOne st{streamVar = Just sv} $ toStream m)
    SVar.toSVarParallel st sv $ D.toStreamD m
    foldStream st yld sng stp $ SVar.fromSVar sv

-------------------------------------------------------------------------------
-- Concurrent application and fold
-------------------------------------------------------------------------------

-- | Same as 'mkParallel' but for StreamD stream.
--
{-# INLINE_NORMAL mkParallelD #-}
mkParallelD :: MonadAsync m => D.Stream m a -> D.Stream m a
mkParallelD m = D.Stream step Nothing
    where

    step gst Nothing = do
        sv <- newParallelVar StopNone gst
        SVar.toSVarParallel gst sv m
        -- XXX use unfold instead?
        return $ Skip $ Just $ SVar.fromSVarD sv

    step gst (Just (D.UnStream step1 st)) = do
        r <- step1 gst st
        return $ case r of
            Yield a s -> Yield a (Just $ D.Stream step1 s)
            Skip s    -> Skip (Just $ D.Stream step1 s)
            Stop      -> Stop

-- Compare with mkAsync. mkAsync uses an Async style SVar whereas this uses a
-- parallel style SVar for evaluation. Currently, parallel style cannot use
-- rate control whereas Async style can use rate control. In async style SVar
-- the worker thread terminates when the buffer is full whereas in Parallel
-- style it blocks.
--
-- | Make the stream producer and consumer run concurrently by introducing a
-- buffer between them. The producer thread evaluates the input stream until
-- the buffer fills, it blocks if the buffer is full until there is space in
-- the buffer. The consumer consumes the stream lazily from the buffer.
--
-- @mkParallel = D.fromStreamD . mkParallelD . D.toStreamD@
--
-- /Pre-release/
--
{-# INLINE_NORMAL mkParallel #-}
mkParallel :: (K.IsStream t, MonadAsync m) => t m a -> t m a
mkParallel = D.fromStreamD . mkParallelD . D.toStreamD

-------------------------------------------------------------------------------
-- Concurrent tap
-------------------------------------------------------------------------------

-- NOTE: In regular pull style streams, the consumer stream is pulling elements
-- from the SVar and we have several workers producing elements and pushing to
-- SVar. In case of folds, we, the parent stream driving the fold, are the
-- stream producing worker, we start an SVar and start pushing to the SVar, the
-- fold on the other side of the SVar is the consumer stream.
--
-- In the pull stream case exceptions are propagated from the producing workers
-- to the consumer stream, the exceptions are propagated on the same channel as
-- the produced stream elements. However, in case of push style folds the
-- current stream itself is the worker and the fold is the consumer, in this
-- case we have to propagate the exceptions from the consumer to the producer.
-- This is reverse of the pull case and we need a reverse direction channel
-- to propagate the exception.
--
-- | Redirect a copy of the stream to a supplied fold and run it concurrently
-- in an independent thread. The fold may buffer some elements. The buffer size
-- is determined by the prevailing 'maxBuffer' setting.
--
-- @
--               Stream m a -> m b
--                       |
-- -----stream m a ---------------stream m a-----
--
-- @
--
-- @
-- > S.drain $ S.tapAsync (S.mapM_ print) (S.enumerateFromTo 1 2)
-- 1
-- 2
-- @
--
-- Exceptions from the concurrently running fold are propagated to the current
-- computation.  Note that, because of buffering in the fold, exceptions may be
-- delayed and may not correspond to the current element being processed in the
-- parent stream, but we guarantee that before the parent stream stops the tap
-- finishes and all exceptions from it are drained.
--
--
-- Compare with 'tap'.
--
-- /Pre-release/
{-# INLINE tapAsync #-}
tapAsync :: (IsStream t, MonadAsync m) => (t m a -> m b) -> t m a -> t m a
tapAsync f m = mkStream $ \st yld sng stp -> do
    sv <- SVar.newFoldSVar st f
    foldStreamShared st yld sng stp (SVar.teeToSVar sv m)

data TapState fs st a = TapInit | Tapping !fs st | TapDone st

-- | Like 'tapAsync' but uses a 'Fold' instead of a fold function.
--
{-# INLINE_NORMAL tapAsyncF #-}
tapAsyncF :: MonadAsync m => Fold m a b -> D.Stream m a -> D.Stream m a
tapAsyncF f (D.Stream step1 state1) = D.Stream step TapInit
    where

    drainFold svr = do
            -- In general, a Stop event would come equipped with the result
            -- of the fold. It is not used here but it would be useful in
            -- applicative and distribute.
            done <- SVar.fromConsumer svr
            when (not done) $ do
                liftIO $ withDiagMVar svr "teeToSVar: waiting to drain"
                       $ takeMVar (outputDoorBellFromConsumer svr)
                drainFold svr

    stopFold svr = do
            liftIO $ sendStop svr Nothing
            -- drain/wait until a stop event arrives from the fold.
            drainFold svr

    {-# INLINE_LATE step #-}
    step gst TapInit = do
        sv <- SVar.newFoldSVarF gst f
        return $ Skip (Tapping sv state1)

    step gst (Tapping sv st) = do
        r <- step1 gst st
        case r of
            Yield a s ->  do
                done <- SVar.pushToFold sv a
                if done
                then do
                    -- XXX we do not need to wait synchronously here
                    stopFold sv
                    return $ Yield a (TapDone s)
                else return $ Yield a (Tapping sv s)
            Skip s -> return $ Skip (Tapping sv s)
            Stop -> do
                stopFold sv
                return Stop

    step gst (TapDone st) = do
        r <- step1 gst st
        return $ case r of
            Yield a s -> Yield a (TapDone s)
            Skip s    -> Skip (TapDone s)
            Stop      -> Stop

-- | Concurrently distribute a stream to a collection of fold functions,
-- discarding the outputs of the folds.
--
-- @
-- > Stream.drain $ Stream.distributeAsync_ [Stream.mapM_ print, Stream.mapM_ print] (Stream.enumerateFromTo 1 2)
-- 1
-- 2
-- 1
-- 2
--
-- @
--
-- @
-- distributeAsync_ = flip (foldr tapAsync)
-- @
--
-- /Pre-release/
--
{-# INLINE distributeAsync_ #-}
distributeAsync_ :: (Foldable f, IsStream t, MonadAsync m)
    => f (t m a -> m b) -> t m a -> t m a
distributeAsync_ = flip (foldr tapAsync)

------------------------------------------------------------------------------
-- ParallelT
------------------------------------------------------------------------------

-- | Async composition with strict concurrent execution of all streams.
--
-- The 'Semigroup' instance of 'ParallelT' executes both the streams
-- concurrently without any delay or without waiting for the consumer demand
-- and /merges/ the results as they arrive. If the consumer does not consume
-- the results, they are buffered upto a configured maximum, controlled by the
-- 'maxBuffer' primitive. If the buffer becomes full the concurrent tasks will
-- block until there is space in the buffer.
--
-- Both 'WAsyncT' and 'ParallelT', evaluate the constituent streams fairly in a
-- round robin fashion. The key difference is that 'WAsyncT' might wait for the
-- consumer demand before it executes the tasks whereas 'ParallelT' starts
-- executing all the tasks immediately without waiting for the consumer demand.
-- For 'WAsyncT' the 'maxThreads' limit applies whereas for 'ParallelT' it does
-- not apply. In other words, 'WAsyncT' can be lazy whereas 'ParallelT' is
-- strict.
--
-- 'ParallelT' is useful for cases when the streams are required to be
-- evaluated simultaneously irrespective of how the consumer consumes them e.g.
-- when we want to race two tasks and want to start both strictly at the same
-- time or if we have timers in the parallel tasks and our results depend on
-- the timers being started at the same time. If we do not have such
-- requirements then 'AsyncT' or 'AheadT' are recommended as they can be more
-- efficient than 'ParallelT'.
--
-- @
-- main = (S.'toList' . S.'parallely' $ (S.fromFoldable [1,2]) \<> (S.fromFoldable [3,4])) >>= print
-- @
-- @
-- [1,3,2,4]
-- @
--
-- When streams with more than one element are merged, it yields whichever
-- stream yields first without any bias, unlike the 'Async' style streams.
--
-- Any exceptions generated by a constituent stream are propagated to the
-- output stream. The output and exceptions from a single stream are guaranteed
-- to arrive in the same order in the resulting stream as they were generated
-- in the input stream. However, the relative ordering of elements from
-- different streams in the resulting stream can vary depending on scheduling
-- and generation delays.
--
-- Similarly, the 'Monad' instance of 'ParallelT' runs /all/ iterations
-- of the loop concurrently.
--
-- @
-- import qualified "Streamly.Prelude" as S
-- import Control.Concurrent
--
-- main = S.'drain' . S.'parallely' $ do
--     n <- return 3 \<\> return 2 \<\> return 1
--     S.valueM $ do
--          threadDelay (n * 1000000)
--          myThreadId >>= \\tid -> putStrLn (show tid ++ ": Delay " ++ show n)
-- @
-- @
-- ThreadId 40: Delay 1
-- ThreadId 39: Delay 2
-- ThreadId 38: Delay 3
-- @
--
-- Note that parallel composition can only combine a finite number of
-- streams as it needs to retain state for each unfinished stream.
--
-- /Since: 0.1.0 ("Streamly")/
--
-- /Since: 0.7.0 (maxBuffer applies to ParallelT streams)/
--
-- @since 0.8.0
newtype ParallelT m a = ParallelT {getParallelT :: Stream m a}
    deriving (MonadTrans)

-- | A parallely composing IO stream of elements of type @a@.
-- See 'ParallelT' documentation for more details.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
type Parallel = ParallelT IO

-- | Fix the type of a polymorphic stream as 'ParallelT'.
--
-- /Since: 0.1.0 ("Streamly")/
--
-- @since 0.8.0
parallely :: IsStream t => ParallelT m a -> t m a
parallely = adapt

instance IsStream ParallelT where
    toStream = getParallelT
    fromStream = ParallelT

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> ParallelT IO a -> ParallelT IO a #-}
    consM = consMParallel

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> ParallelT IO a -> ParallelT IO a #-}
    (|:) = consM

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

{-# INLINE mappendParallel #-}
{-# SPECIALIZE mappendParallel :: ParallelT IO a -> ParallelT IO a -> ParallelT IO a #-}
mappendParallel :: MonadAsync m => ParallelT m a -> ParallelT m a -> ParallelT m a
mappendParallel m1 m2 = fromStream $ parallel (toStream m1) (toStream m2)

instance MonadAsync m => Semigroup (ParallelT m a) where
    (<>) = mappendParallel

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance MonadAsync m => Monoid (ParallelT m a) where
    mempty = K.nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

{-# INLINE apParallel #-}
{-# SPECIALIZE apParallel :: ParallelT IO (a -> b) -> ParallelT IO a -> ParallelT IO b #-}
apParallel :: MonadAsync m => ParallelT m (a -> b) -> ParallelT m a -> ParallelT m b
apParallel (ParallelT m1) (ParallelT m2) =
    let f x1 = K.concatMapBy parallel (pure . x1) m2
    in ParallelT $ K.concatMapBy parallel f m1

instance (Monad m, MonadAsync m) => Applicative (ParallelT m) where
    {-# INLINE pure #-}
    pure = ParallelT . K.value
    {-# INLINE (<*>) #-}
    (<*>) = apParallel

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

{-# INLINE bindParallel #-}
{-# SPECIALIZE bindParallel :: ParallelT IO a -> (a -> ParallelT IO b) -> ParallelT IO b #-}
bindParallel :: MonadAsync m => ParallelT m a -> (a -> ParallelT m b) -> ParallelT m b
bindParallel m f = fromStream $ K.bindWith parallel (K.adapt m) (\a -> K.adapt $ f a)

instance MonadAsync m => Monad (ParallelT m) where
    return = pure
    (>>=) = bindParallel

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_COMMON_INSTANCES(ParallelT, MONADPARALLEL)

-------------------------------------------------------------------------------
-- From callback
-------------------------------------------------------------------------------

-- Note: we can use another API with two callbacks stop and yield if we want
-- the callback to be able to indicate end of stream.
--
-- | Generates a callback and a stream pair. The callback returned is used to
-- queue values to the stream.  The stream is infinite, there is no way for the
-- callback to indicate that it is done now.
--
-- /Pre-release/
--
{-# INLINE_NORMAL newCallbackStream #-}
newCallbackStream :: (K.IsStream t, MonadAsync m) => m (a -> m (), t m a)
newCallbackStream = do
    sv <- newParallelVar StopNone defState

    -- XXX Add our own thread-id to the SVar as we can not know the callback's
    -- thread-id and the callback is not run in a managed worker. We need to
    -- handle this better.
    liftIO myThreadId >>= modifyThread sv

    let callback a = liftIO $ void $ send sv (ChildYield a)
    -- XXX we can return an SVar and then the consumer can unfold from the
    -- SVar?
    return (callback, D.fromStreamD (SVar.fromSVarD sv))
