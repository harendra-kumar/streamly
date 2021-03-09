{-# LANGUAGE UndecidableInstances #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.Async
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Internal.Data.Stream.Async
    (
      AsyncT
    , Async
    , asyncly
    , async
    , (<|)             --deprecated
    , mkAsync
    , mkAsyncK

    , WAsyncT
    , WAsync
    , wAsyncly
    , wAsync
    )
where

import Control.Concurrent (myThreadId)
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Concurrent.MVar (newEmptyMVar)
-- import Control.Monad.Error.Class   (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Concurrent.Queue.MichaelScott (LinkedQueue, newQ, nullQ, tryPopR)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Maybe (fromJust)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif

import Prelude hiding (map)
import qualified Data.Set as S

import Streamly.Internal.Data.Atomics (atomicModifyIORefCAS)
import Streamly.Internal.Data.Stream.SVar (fromSVar, fromSVarD)
import Streamly.Internal.Data.SVar
import Streamly.Internal.Data.Stream.StreamK.Type
       (IsStream(..), Stream, mkStream, foldStream, adapt, foldStreamShared)

import qualified Streamly.Internal.Data.Stream.StreamK as K (withLocal)
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D

#include "Instances.hs"

-------------------------------------------------------------------------------
-- Async
-------------------------------------------------------------------------------

data WorkerStatus = Continue | Suspend

{-# INLINE workLoopLIFO #-}
workLoopLIFO
    :: (MonadIO m, MonadBaseControl IO m)
    => IORef [(RunInIO m, Stream m a)]
    -> State Stream m a
    -> SVar Stream m a
    -> Maybe WorkerInfo
    -> m ()
workLoopLIFO q st sv winfo = run

    where

    stop = liftIO $ sendStop sv winfo
    run = do
        work <- dequeue
        case work of
            Nothing -> stop
            Just (RunInIO runin, m) -> do
                -- XXX when we finish we need to send the monadic state back to
                -- the parent so that the state can be merged back. We capture
                -- and return the state in the stop continuation.
                --
                -- Instead of using the run function we can just restore the
                -- monad state here. That way it can work easily for
                -- distributed case as well.
                r <- liftIO $ runin $
                        foldStreamShared st yieldk single (return Continue) m
                res <- restoreM r
                case res of
                    Continue -> run
                    Suspend -> stop

    single a = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        return $ if res then Continue else Suspend

    yieldk a r = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        if res
        then foldStreamShared st yieldk single (return Continue) r
        else do
            runInIO <- captureMonadState
            liftIO $ enqueueLIFO sv q (runInIO, r)
            return Suspend

    dequeue = liftIO $ atomicModifyIORefCAS q $ \case
                [] -> ([], Nothing)
                x : xs -> (xs, Just x)

-- We duplicate workLoop for yield limit and no limit cases because it has
-- around 40% performance overhead in the worst case.
--
-- XXX we can pass yinfo directly as an argument here so that we do not have to
-- make a check every time.
{-# INLINE workLoopLIFOLimited #-}
workLoopLIFOLimited
    :: forall m a. (MonadIO m, MonadBaseControl IO m)
    => IORef [(RunInIO m, Stream m a)]
    -> State Stream m a
    -> SVar Stream m a
    -> Maybe WorkerInfo
    -> m ()
workLoopLIFOLimited q st sv winfo = run

    where

    incrContinue = liftIO (incrementYieldLimit sv) >> return Continue
    stop = liftIO $ sendStop sv winfo
    run = do
        work <- dequeue
        case work of
            Nothing -> stop
            Just (RunInIO runin, m) -> do
                -- XXX This is just a best effort minimization of concurrency
                -- to the yield limit. If the stream is made of concurrent
                -- streams we do not reserve the yield limit in the constituent
                -- streams before executing the action. This can be done
                -- though, by sharing the yield limit ref with downstream
                -- actions via state passing. Just a todo.
                yieldLimitOk <- liftIO $ decrementYieldLimit sv
                if yieldLimitOk
                then do
                    r <- liftIO $ runin $
                            foldStreamShared st yieldk single incrContinue m
                    res <- restoreM r
                    case res of
                        Continue -> run
                        Suspend -> stop
                -- Avoid any side effects, undo the yield limit decrement if we
                -- never yielded anything.
                else liftIO $ do
                    enqueueLIFO sv q (RunInIO runin, m)
                    incrementYieldLimit sv
                    sendStop sv winfo

    single a = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        return $ if res then Continue else Suspend

    -- XXX can we pass on the yield limit downstream to limit the concurrency
    -- of constituent streams.
    yieldk a r = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        yieldLimitOk <- liftIO $ decrementYieldLimit sv
        if res && yieldLimitOk
        then foldStreamShared st yieldk single incrContinue r
        else do
            runInIO <- captureMonadState
            liftIO $ incrementYieldLimit sv
            liftIO $ enqueueLIFO sv q (runInIO, r)
            return Suspend

    dequeue = liftIO $ atomicModifyIORefCAS q $ \case
                [] -> ([], Nothing)
                x : xs -> (xs, Just x)

-------------------------------------------------------------------------------
-- WAsync
-------------------------------------------------------------------------------

-- XXX we can remove sv as it is derivable from st

{-# INLINE workLoopFIFO #-}
workLoopFIFO
    :: (MonadIO m, MonadBaseControl IO m)
    => LinkedQueue (RunInIO m, Stream m a)
    -> State Stream m a
    -> SVar Stream m a
    -> Maybe WorkerInfo
    -> m ()
workLoopFIFO q st sv winfo = run

    where

    stop = liftIO $ sendStop sv winfo
    run = do
        work <- liftIO $ tryPopR q
        case work of
            Nothing -> stop
            Just (RunInIO runin, m) -> do
                r <- liftIO $ runin $
                        foldStreamShared st yieldk single (return Continue) m
                res <- restoreM r
                case res of
                    Continue -> run
                    Suspend -> stop

    single a = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        return $ if res then Continue else Suspend

    -- XXX in general we would like to yield "n" elements from a single stream
    -- before moving on to the next. Single element granularity could be too
    -- expensive in certain cases. Similarly, we can use time limit for
    -- yielding.
    yieldk a r = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        runInIO <- captureMonadState
        liftIO $ enqueueFIFO sv q (runInIO, r)
        return $ if res then Continue else Suspend

{-# INLINE workLoopFIFOLimited #-}
workLoopFIFOLimited
    :: forall m a. (MonadIO m, MonadBaseControl IO m)
    => LinkedQueue (RunInIO m, Stream m a)
    -> State Stream m a
    -> SVar Stream m a
    -> Maybe WorkerInfo
    -> m ()
workLoopFIFOLimited q st sv winfo = run

    where

    stop = liftIO $ sendStop sv winfo
    incrContinue = liftIO (incrementYieldLimit sv) >> return Continue
    run = do
        work <- liftIO $ tryPopR q
        case work of
            Nothing -> stop
            Just (RunInIO runin, m) -> do
                yieldLimitOk <- liftIO $ decrementYieldLimit sv
                if yieldLimitOk
                then do
                    r <- liftIO $ runin $
                            foldStreamShared st yieldk single incrContinue m
                    res <- restoreM r
                    case res of
                        Continue -> run
                        Suspend -> stop
                else liftIO $ do
                    enqueueFIFO sv q (RunInIO runin, m)
                    incrementYieldLimit sv
                    sendStop sv winfo

    single a = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        return $ if res then Continue else Suspend

    yieldk a r = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        runInIO <- captureMonadState
        liftIO $ enqueueFIFO sv q (runInIO, r)
        yieldLimitOk <- liftIO $ decrementYieldLimit sv
        if res && yieldLimitOk
        then return Continue
        else liftIO $ do
            incrementYieldLimit sv
            return Suspend

-------------------------------------------------------------------------------
-- SVar creation
-- This code belongs in SVar.hs but is kept here for perf reasons
-------------------------------------------------------------------------------

-- XXX we have this function in this file because passing runStreamLIFO as a
-- function argument to this function results in a perf degradation of more
-- than 10%.  Need to investigate what the root cause is.
-- Interestingly, the same thing does not make any difference for Ahead.
getLifoSVar :: forall m a. MonadAsync m
    => State Stream m a -> RunInIO m -> IO (SVar Stream m a)
getLifoSVar st mrun = do
    outQ    <- newIORef ([], 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    wfw     <- newIORef False
    running <- newIORef S.empty
    q       <- newIORef ([] :: [(RunInIO m, Stream m a)])
    yl      <- case getYieldLimit st of
                Nothing -> return Nothing
                Just x -> Just <$> newIORef x
    rateInfo <- getYieldRateInfo st

    stats <- newSVarStats
    tid <- myThreadId

    let isWorkFinished _ = null <$> readIORef q

    let isWorkFinishedLimited sv = do
            yieldsDone <-
                    case remainingWork sv of
                        Just ref -> do
                            n <- readIORef ref
                            return (n <= 0)
                        Nothing -> return False
            qEmpty <- null <$> readIORef q
            return $ qEmpty || yieldsDone

    let getSVar :: SVar Stream m a
            -> (SVar Stream m a -> m [ChildEvent a])
            -> (SVar Stream m a -> m Bool)
            -> (SVar Stream m a -> IO Bool)
            -> (IORef [(RunInIO m, Stream m a)]
                -> State Stream m a
                -> SVar Stream m a
                -> Maybe WorkerInfo
                -> m())
            -> SVar Stream m a
        getSVar sv readOutput postProc workDone wloop = SVar
            { outputQueue      = outQ
            , outputQueueFromConsumer = undefined
            , remainingWork    = yl
            , maxBufferLimit   = getMaxBuffer st
            , pushBufferSpace  = undefined
            , pushBufferPolicy = undefined
            , pushBufferMVar   = undefined
            , maxWorkerLimit   = min (getMaxThreads st) (getMaxBuffer st)
            , yieldRateInfo    = rateInfo
            , outputDoorBell   = outQMv
            , outputDoorBellFromConsumer = undefined
            , readOutputQ      = readOutput sv
            , postProcess      = postProc sv
            , workerThreads    = running
            , workLoop         = wloop q st{streamVar = Just sv} sv
            , enqueue          = enqueueLIFO sv q
            , isWorkDone       = workDone sv
            , isQueueDone      = workDone sv
            , needDoorBell     = wfw
            , svarStyle        = AsyncVar
            , svarStopStyle    = StopNone
            , svarStopBy       = undefined
            , svarMrun         = mrun
            , workerCount      = active
            , accountThread    = delThread sv
            , workerStopMVar   = undefined
            , svarRef          = Nothing
            , svarInspectMode  = getInspectMode st
            , svarCreator      = tid
            , aheadWorkQueue   = undefined
            , outputHeap       = undefined
            , svarStats        = stats
            }

    let sv =
            case getStreamRate st of
                Nothing ->
                    case getYieldLimit st of
                        Nothing -> getSVar sv readOutputQBounded
                                              postProcessBounded
                                              isWorkFinished
                                              workLoopLIFO
                        Just _  -> getSVar sv readOutputQBounded
                                              postProcessBounded
                                              isWorkFinishedLimited
                                              workLoopLIFOLimited
                Just _  ->
                    case getYieldLimit st of
                        Nothing -> getSVar sv readOutputQPaced
                                              postProcessPaced
                                              isWorkFinished
                                              workLoopLIFO
                        Just _  -> getSVar sv readOutputQPaced
                                              postProcessPaced
                                              isWorkFinishedLimited
                                              workLoopLIFOLimited
     in return sv

getFifoSVar :: forall m a. MonadAsync m
    => State Stream m a -> RunInIO m -> IO (SVar Stream m a)
getFifoSVar st mrun = do
    outQ    <- newIORef ([], 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    wfw     <- newIORef False
    running <- newIORef S.empty
    q       <- newQ
    yl      <- case getYieldLimit st of
                Nothing -> return Nothing
                Just x -> Just <$> newIORef x
    rateInfo <- getYieldRateInfo st

    stats <- newSVarStats
    tid <- myThreadId

    let isWorkFinished _ = nullQ q
    let isWorkFinishedLimited sv = do
            yieldsDone <-
                    case remainingWork sv of
                        Just ref -> do
                            n <- readIORef ref
                            return (n <= 0)
                        Nothing -> return False
            qEmpty <- nullQ q
            return $ qEmpty || yieldsDone

    let getSVar :: SVar Stream m a
            -> (SVar Stream m a -> m [ChildEvent a])
            -> (SVar Stream m a -> m Bool)
            -> (SVar Stream m a -> IO Bool)
            -> (LinkedQueue (RunInIO m, Stream m a)
                -> State Stream m a
                -> SVar Stream m a
                -> Maybe WorkerInfo
                -> m())
            -> SVar Stream m a
        getSVar sv readOutput postProc workDone wloop = SVar
            { outputQueue      = outQ
            , outputQueueFromConsumer = undefined
            , remainingWork    = yl
            , maxBufferLimit   = getMaxBuffer st
            , pushBufferSpace  = undefined
            , pushBufferPolicy = undefined
            , pushBufferMVar   = undefined
            , maxWorkerLimit   = min (getMaxThreads st) (getMaxBuffer st)
            , yieldRateInfo    = rateInfo
            , outputDoorBell   = outQMv
            , outputDoorBellFromConsumer = undefined
            , readOutputQ      = readOutput sv
            , postProcess      = postProc sv
            , workerThreads    = running
            , workLoop         = wloop q st{streamVar = Just sv} sv
            , enqueue          = enqueueFIFO sv q
            , isWorkDone       = workDone sv
            , isQueueDone      = workDone sv
            , needDoorBell     = wfw
            , svarStyle        = WAsyncVar
            , svarStopStyle    = StopNone
            , svarStopBy       = undefined
            , svarMrun         = mrun
            , workerCount      = active
            , accountThread    = delThread sv
            , workerStopMVar   = undefined
            , svarRef          = Nothing
            , svarInspectMode  = getInspectMode st
            , svarCreator      = tid
            , aheadWorkQueue   = undefined
            , outputHeap       = undefined
            , svarStats        = stats
            }

    let sv =
            case getStreamRate st of
                Nothing ->
                    case getYieldLimit st of
                        Nothing -> getSVar sv readOutputQBounded
                                              postProcessBounded
                                              isWorkFinished
                                              workLoopFIFO
                        Just _  -> getSVar sv readOutputQBounded
                                              postProcessBounded
                                              isWorkFinishedLimited
                                              workLoopFIFOLimited
                Just _  ->
                    case getYieldLimit st of
                        Nothing -> getSVar sv readOutputQPaced
                                              postProcessPaced
                                              isWorkFinished
                                              workLoopFIFO
                        Just _  -> getSVar sv readOutputQPaced
                                              postProcessPaced
                                              isWorkFinishedLimited
                                              workLoopFIFOLimited
     in return sv

{-# INLINABLE newAsyncVar #-}
newAsyncVar :: MonadAsync m
    => State Stream m a -> Stream m a -> m (SVar Stream m a)
newAsyncVar st m = do
    mrun <- captureMonadState
    sv <- liftIO $ getLifoSVar st mrun
    sendFirstWorker sv m

-- | Generate a stream asynchronously to keep it buffered, lazily consume
-- from the buffer.
--
-- /Pre-release/
--
{-# INLINABLE mkAsyncK #-}
mkAsyncK :: (IsStream t, MonadAsync m) => t m a -> t m a
mkAsyncK m = mkStream $ \st yld sng stp -> do
    sv <- newAsyncVar (adaptState st) (toStream m)
    foldStream st yld sng stp $ fromSVar sv

{-# INLINE_NORMAL mkAsyncD #-}
mkAsyncD :: MonadAsync m => D.Stream m a -> D.Stream m a
mkAsyncD m = D.Stream step Nothing
    where

    step gst Nothing = do
        sv <- newAsyncVar gst (D.fromStreamD m)
        return $ D.Skip $ Just $ fromSVarD sv

    step gst (Just (D.UnStream step1 st)) = do
        r <- step1 gst st
        return $ case r of
            D.Yield a s -> D.Yield a (Just $ D.Stream step1 s)
            D.Skip s    -> D.Skip (Just $ D.Stream step1 s)
            D.Stop      -> D.Stop

-- /Since: 0.8.0 ("Streamly.Prelude")/
--
-- This is slightly faster than the CPS version above
--
-- | Make the stream producer and consumer run concurrently by introducing a
-- buffer between them. The producer thread evaluates the input stream until
-- the buffer fills, it terminates if the buffer is full and a worker thread is
-- kicked off again to evaluate the remaining stream when there is space in the
-- buffer.  The consumer consumes the stream lazily from the buffer.
--
-- /Pre-release/
--
{-# INLINE_NORMAL mkAsync #-}
mkAsync :: (K.IsStream t, MonadAsync m) => t m a -> t m a
mkAsync = D.fromStreamD . mkAsyncD . D.toStreamD

-- | Create a new SVar and enqueue one stream computation on it.
{-# INLINABLE newWAsyncVar #-}
newWAsyncVar :: MonadAsync m
    => State Stream m a -> Stream m a -> m (SVar Stream m a)
newWAsyncVar st m = do
    mrun <- captureMonadState
    sv <- liftIO $ getFifoSVar st mrun
    sendFirstWorker sv m

------------------------------------------------------------------------------
-- Running streams concurrently
------------------------------------------------------------------------------

-- Concurrency rate control.
--
-- Our objective is to create more threads on demand if the consumer is running
-- faster than us. As soon as we encounter a concurrent composition we create a
-- push pull pair of threads. We use an SVar for communication between the
-- consumer, pulling from the SVar and the producer who is pushing to the SVar.
-- The producer creates more threads if the SVar drains and becomes empty, that
-- is the consumer is running faster.
--
-- XXX Note 1: This mechanism can be problematic if the initial production
-- latency is high, we may end up creating too many threads. So we need some
-- way to monitor and use the latency as well. Having a limit on the dispatches
-- (programmer controlled) may also help.
--
-- TBD Note 2: We may want to run computations at the lower level of the
-- composition tree serially even when they are composed using a parallel
-- combinator. We can use 'serial' in place of 'async' and 'wSerial' in
-- place of 'wAsync'. If we find that an SVar immediately above a computation
-- gets drained empty we can switch to parallelizing the computation.  For that
-- we can use a state flag to fork the rest of the computation at any point of
-- time inside the Monad bind operation if the consumer is running at a faster
-- speed.
--
-- TBD Note 3: the binary operation ('parallel') composition allows us to
-- dispatch a chunkSize of only 1.  If we have to dispatch in arbitrary
-- chunksizes we will need to compose the parallel actions using a data
-- constructor (A Free container) instead so that we can divide it in chunks of
-- arbitrary size before dispatching. If the stream is composed of
-- hierarchically composed grains of different sizes then we can always switch
-- to a desired granularity depending on the consumer speed.
--
-- TBD Note 4: for pure work (when we are not in the IO monad) we can divide it
-- into just the number of CPUs.

-- | Join two computations on the currently running 'SVar' queue for concurrent
-- execution.  When we are using parallel composition, an SVar is passed around
-- as a state variable. We try to schedule a new parallel computation on the
-- SVar passed to us. The first time, when no SVar exists, a new SVar is
-- created.  Subsequently, 'joinStreamVarAsync' may get called when a computation
-- already scheduled on the SVar is further evaluated. For example, when (a
-- `parallel` b) is evaluated it calls a 'joinStreamVarAsync' to put 'a' and 'b' on
-- the current scheduler queue.
--
-- The 'SVarStyle' required by the current composition context is passed as one
-- of the parameters.  If the scheduling and composition style of the new
-- computation being scheduled is different than the style of the current SVar,
-- then we create a new SVar and schedule it on that.  The newly created SVar
-- joins as one of the computations on the current SVar queue.
--
-- Cases when we need to switch to a new SVar:
--
-- * (x `parallel` y) `parallel` (t `parallel` u) -- all of them get scheduled on the same SVar
-- * (x `parallel` y) `parallel` (t `async` u) -- @t@ and @u@ get scheduled on a new child SVar
--   because of the scheduling policy change.
-- * if we 'adapt' a stream of type 'async' to a stream of type
--   'Parallel', we create a new SVar at the transitioning bind.
-- * When the stream is switching from disjunctive composition to conjunctive
--   composition and vice-versa we create a new SVar to isolate the scheduling
--   of the two.

forkSVarAsync :: (IsStream t, MonadAsync m)
    => SVarStyle -> t m a -> t m a -> t m a
forkSVarAsync style m1 m2 = mkStream $ \st yld sng stp -> do
    sv <- case style of
        AsyncVar -> newAsyncVar st (concurrently (toStream m1) (toStream m2))
        WAsyncVar -> newWAsyncVar st (concurrently (toStream m1) (toStream m2))
        _ -> error "illegal svar type"
    foldStream st yld sng stp $ fromSVar sv
    where
    concurrently ma mb = mkStream $ \st yld sng stp -> do
        runInIO <- captureMonadState
        liftIO $ enqueue (fromJust $ streamVar st) $ (runInIO, mb)
        foldStreamShared st yld sng stp ma

{-# INLINE joinStreamVarAsync #-}
joinStreamVarAsync :: (IsStream t, MonadAsync m)
    => SVarStyle -> t m a -> t m a -> t m a
joinStreamVarAsync style m1 m2 = mkStream $ \st yld sng stp ->
    case streamVar st of
        Just sv | svarStyle sv == style -> do
            runInIO <- captureMonadState
            liftIO $ enqueue sv $ (runInIO, toStream m2)
            foldStreamShared st yld sng stp m1
        _ -> foldStreamShared st yld sng stp (forkSVarAsync style m1 m2)

------------------------------------------------------------------------------
-- Semigroup and Monoid style compositions for parallel actions
------------------------------------------------------------------------------

infixr 6 `async`

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'AsyncT'.
-- Merges two streams possibly concurrently, preferring the
-- elements from the left one when available.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE async #-}
async :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
async = joinStreamVarAsync AsyncVar

-- | Same as 'async'.
--
-- @since 0.1.0
{-# DEPRECATED (<|) "Please use 'async' instead." #-}
{-# INLINE (<|) #-}
(<|) :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
(<|) = async

-- IMPORTANT: using a monomorphically typed and SPECIALIZED consMAsync makes a
-- huge difference in the performance of consM in IsStream instance even we
-- have a SPECIALIZE in the instance.
--
-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using async.
{-# INLINE consMAsync #-}
{-# SPECIALIZE consMAsync :: IO a -> AsyncT IO a -> AsyncT IO a #-}
consMAsync :: MonadAsync m => m a -> AsyncT m a -> AsyncT m a
consMAsync m r = fromStream $ K.yieldM m `async` (toStream r)

------------------------------------------------------------------------------
-- AsyncT
------------------------------------------------------------------------------

-- | The 'Semigroup' operation (@<>@) for 'AsyncT' merges two streams
-- concurrently with priority given to the first stream. In @s1 <> s2 <> s3
-- ...@ the streams s1, s2 and s3 are scheduled for execution in that order.
-- Multiple scheduled streams may be executed concurrently and the elements
-- generated by them are served to the consumer as and when they become
-- available. This behavior is similar to the scheduling and execution behavior
-- of actions in a single async stream.
--
-- Since only a finite number of streams are executed concurrently, this
-- operation can be used to fold an infinite lazy container of streams.
--
-- @
-- import qualified "Streamly.Prelude" as S
-- import Control.Concurrent
--
-- main = (S.toList . S.'asyncly' $ (S.fromList [1,2]) \<> (S.fromList [3,4])) >>= print
-- @
-- @
-- [1,2,3,4]
-- @
--
-- Any exceptions generated by a constituent stream are propagated to the
-- output stream. The output and exceptions from a single stream are guaranteed
-- to arrive in the same order in the resulting stream as they were generated
-- in the input stream. However, the relative ordering of elements from
-- different streams in the resulting stream can vary depending on scheduling
-- and generation delays.
--
-- Similarly, the monad instance of 'AsyncT' /may/ run each iteration
-- concurrently based on demand.  More concurrent iterations are started only
-- if the previous iterations are not able to produce enough output for the
-- consumer.
--
-- @
-- main = S.'drain' . S.'asyncly' $ do
--     n <- return 3 \<\> return 2 \<\> return 1
--     S.yieldM $ do
--          threadDelay (n * 1000000)
--          myThreadId >>= \\tid -> putStrLn (show tid ++ ": Delay " ++ show n)
-- @
-- @
-- ThreadId 40: Delay 1
-- ThreadId 39: Delay 2
-- ThreadId 38: Delay 3
-- @
--
-- /Since: 0.1.0 ("Streamly")/
--
-- @since 0.8.0
newtype AsyncT m a = AsyncT {getAsyncT :: Stream m a}
    deriving (MonadTrans)

-- | A demand driven left biased parallely composing IO stream of elements of
-- type @a@.  See 'AsyncT' documentation for more details.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
type Async = AsyncT IO

-- | Fix the type of a polymorphic stream as 'AsyncT'.
--
-- /Since: 0.1.0 ("Streamly")/
--
-- @since 0.8.0
asyncly :: IsStream t => AsyncT m a -> t m a
asyncly = adapt

instance IsStream AsyncT where
    toStream = getAsyncT
    fromStream = AsyncT
    consM = consMAsync
    (|:) = consMAsync

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- Monomorphically typed version of "async" for better performance of Semigroup
-- instance.
{-# INLINE mappendAsync #-}
{-# SPECIALIZE mappendAsync :: AsyncT IO a -> AsyncT IO a -> AsyncT IO a #-}
mappendAsync :: MonadAsync m => AsyncT m a -> AsyncT m a -> AsyncT m a
mappendAsync m1 m2 = fromStream $ async (toStream m1) (toStream m2)

instance MonadAsync m => Semigroup (AsyncT m a) where
    (<>) = mappendAsync

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance MonadAsync m => Monoid (AsyncT m a) where
    mempty = K.nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

{-# INLINE apAsync #-}
{-# SPECIALIZE apAsync :: AsyncT IO (a -> b) -> AsyncT IO a -> AsyncT IO b #-}
apAsync :: MonadAsync m => AsyncT m (a -> b) -> AsyncT m a -> AsyncT m b
apAsync (AsyncT m1) (AsyncT m2) =
    let f x1 = K.concatMapBy async (pure . x1) m2
    in AsyncT $ K.concatMapBy async f m1

instance (Monad m, MonadAsync m) => Applicative (AsyncT m) where
    {-# INLINE pure #-}
    pure = AsyncT . K.value
    {-# INLINE (<*>) #-}
    (<*>) = apAsync

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

-- GHC: if we change the implementation of bindWith with arguments in a
-- different order we see a significant performance degradation (~2x).
{-# INLINE bindAsync #-}
{-# SPECIALIZE bindAsync :: AsyncT IO a -> (a -> AsyncT IO b) -> AsyncT IO b #-}
bindAsync :: MonadAsync m => AsyncT m a -> (a -> AsyncT m b) -> AsyncT m b
bindAsync m f = fromStream $ K.bindWith async (adapt m) (\a -> adapt $ f a)

-- GHC: if we specify arguments in the definition of (>>=) we see a significant
-- performance degradation (~2x).
instance MonadAsync m => Monad (AsyncT m) where
    return = pure
    (>>=) = bindAsync

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_COMMON_INSTANCES(AsyncT, MONADPARALLEL)

------------------------------------------------------------------------------
-- WAsyncT
------------------------------------------------------------------------------

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using wAsync.
{-# INLINE consMWAsync #-}
{-# SPECIALIZE consMWAsync :: IO a -> WAsyncT IO a -> WAsyncT IO a #-}
consMWAsync :: MonadAsync m => m a -> WAsyncT m a -> WAsyncT m a
consMWAsync m r = fromStream $ K.yieldM m `wAsync` (toStream r)

infixr 6 `wAsync`

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'WAsyncT'.
-- Merges two streams concurrently choosing elements from both fairly.
--
-- @since 0.8.0
--
-- /Since: 0.2.0 ("Streamly")/
{-# INLINE wAsync #-}
wAsync :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
wAsync = joinStreamVarAsync WAsyncVar

-- | 'WAsyncT' is similar to 'WSerialT' but with concurrent execution.
-- The 'Semigroup' operation (@<>@) for 'WAsyncT' merges two streams
-- concurrently interleaving the actions from both the streams.  In @s1
-- <> s2 <> s3 ...@, the individual actions from streams @s1@, @s2@ and @s3@
-- are scheduled for execution in a round-robin fashion.  Multiple scheduled
-- actions may be executed concurrently, the results from concurrent executions
-- are consumed in the order in which they become available.
--
--
-- The @W@ in the name stands for @wide@ or breadth wise scheduling in
-- contrast to the depth wise scheduling behavior of 'AsyncT'.
--
-- @
-- import qualified "Streamly.Prelude" as S
-- import Control.Concurrent
--
-- main = (S.toList . S.'wAsyncly' . S.maxThreads 1 $ (S.fromList [1,2]) \<> (S.fromList [3,4])) >>= print
-- @
-- @
-- [1,3,2,4]
-- @
--
-- For this example, we are using @maxThreads 1@ so that concurrent thread
-- scheduling does not affect the results and make them unpredictable. Let's
-- now take a more general example:
--
-- @
-- main = (S.toList . S.'wAsyncly' . S.maxThreads 1 $ (S.fromList [1,2,3]) \<> (S.fromList [4,5,6]) \<> (S.fromList [7,8,9])) >>= print
-- @
-- @
-- [1,4,2,7,5,3,8,6,9]
-- @
--
-- This is how the execution of the above stream proceeds:
--
-- 1. The scheduler queue is initialized with @[S.fromList [1,2,3],
-- (S.fromList [4,5,6]) \<> (S.fromList [7,8,9])]@ assuming the head of the
-- queue is represented by the  rightmost item.
-- 2. @S.fromList [1,2,3]@ is executed, yielding the element @1@ and putting
-- @[2,3]@ at the back of the scheduler queue. The scheduler queue now looks
-- like @[(S.fromList [4,5,6]) \<> (S.fromList [7,8,9]), S.fromList [2,3]]@.
-- 3. Now @(S.fromList [4,5,6]) \<> (S.fromList [7,8,9])@ is picked up for
-- execution, @S.fromList [7,8,9]@ is added at the back of the queue and
-- @S.fromList [4,5,6]@ is executed, yielding the element @4@ and adding
-- @S.fromList [5,6]@ at the back of the queue. The queue now looks like
-- @[S.fromList [2,3], S.fromList [7,8,9], S.fromList [5,6]]@.
-- 4. Note that the scheduler queue expands by one more stream component in
-- every pass because one more @<>@ is broken down into two components. At this
-- point there are no more @<>@ operations to be broken down further and the
-- queue has reached its maximum size. Now these streams are scheduled in
-- round-robin fashion yielding @[2,7,5,3,8,6,9]@.
--
-- As we see above, in a right associated expression composed with @<>@, only
-- one @<>@ operation is broken down into two components in one execution,
-- therefore, if we have @n@ streams composed using @<>@ it will take @n@
-- scheduler passes to expand the whole expression.  By the time @n-th@
-- component is added to the scheduler queue, the first component would have
-- received @n@ scheduler passes.
--
-- Since all streams get interleaved, this operation is not suitable for
-- folding an infinite lazy container of infinite size streams.  However, if
-- the streams are small, the streams on the left may get finished before more
-- streams are added to the scheduler queue from the right side of the
-- expression, so it may be possible to fold an infinite lazy container of
-- streams. For example, if the streams are of size @n@ then at most @n@
-- streams would be in the scheduler queue at a time.
--
-- Note that 'WSerialT' and 'WAsyncT' differ in their scheduling behavior,
-- therefore the output of 'WAsyncT' even with a single thread of execution is
-- not the same as that of 'WSerialT' See notes in 'WSerialT' for details about
-- its scheduling behavior.
--
-- Any exceptions generated by a constituent stream are propagated to the
-- output stream. The output and exceptions from a single stream are guaranteed
-- to arrive in the same order in the resulting stream as they were generated
-- in the input stream. However, the relative ordering of elements from
-- different streams in the resulting stream can vary depending on scheduling
-- and generation delays.
--
-- Similarly, the 'Monad' instance of 'WAsyncT' runs /all/ iterations fairly
-- concurrently using a round robin scheduling.
--
-- @
-- main = S.'drain' . S.'wAsyncly' $ do
--     n <- return 3 \<\> return 2 \<\> return 1
--     S.yieldM $ do
--          threadDelay (n * 1000000)
--          myThreadId >>= \\tid -> putStrLn (show tid ++ ": Delay " ++ show n)
-- @
-- @
-- ThreadId 40: Delay 1
-- ThreadId 39: Delay 2
-- ThreadId 38: Delay 3
-- @
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
newtype WAsyncT m a = WAsyncT {getWAsyncT :: Stream m a}
    deriving (MonadTrans)

-- | A round robin parallely composing IO stream of elements of type @a@.
-- See 'WAsyncT' documentation for more details.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
type WAsync = WAsyncT IO

-- | Fix the type of a polymorphic stream as 'WAsyncT'.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
wAsyncly :: IsStream t => WAsyncT m a -> t m a
wAsyncly = adapt

instance IsStream WAsyncT where
    toStream = getWAsyncT
    fromStream = WAsyncT
    consM = consMWAsync
    (|:) = consMWAsync

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

{-# INLINE mappendWAsync #-}
{-# SPECIALIZE mappendWAsync :: WAsyncT IO a -> WAsyncT IO a -> WAsyncT IO a #-}
mappendWAsync :: MonadAsync m => WAsyncT m a -> WAsyncT m a -> WAsyncT m a
mappendWAsync m1 m2 = fromStream $ wAsync (toStream m1) (toStream m2)

instance MonadAsync m => Semigroup (WAsyncT m a) where
    (<>) = mappendWAsync

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance MonadAsync m => Monoid (WAsyncT m a) where
    mempty = K.nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

{-# INLINE apWAsync #-}
{-# SPECIALIZE apWAsync :: WAsyncT IO (a -> b) -> WAsyncT IO a -> WAsyncT IO b #-}
apWAsync :: MonadAsync m => WAsyncT m (a -> b) -> WAsyncT m a -> WAsyncT m b
apWAsync (WAsyncT m1) (WAsyncT m2) =
    let f x1 = K.concatMapBy wAsync (pure . x1) m2
    in WAsyncT $ K.concatMapBy wAsync f m1

-- GHC: if we specify arguments in the definition of (<*>) we see a significant
-- performance degradation (~2x).
instance (Monad m, MonadAsync m) => Applicative (WAsyncT m) where
    pure = WAsyncT . K.value
    (<*>) = apWAsync

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

-- GHC: if we change the implementation of bindWith with arguments in a
-- different order we see a significant performance degradation (~2x).
{-# INLINE bindWAsync #-}
{-# SPECIALIZE bindWAsync :: WAsyncT IO a -> (a -> WAsyncT IO b) -> WAsyncT IO b #-}
bindWAsync :: MonadAsync m => WAsyncT m a -> (a -> WAsyncT m b) -> WAsyncT m b
bindWAsync m f = fromStream $ K.bindWith wAsync (adapt m) (\a -> adapt $ f a)

-- GHC: if we specify arguments in the definition of (>>=) we see a significant
-- performance degradation (~2x).
instance MonadAsync m => Monad (WAsyncT m) where
    return = pure
    (>>=) = bindWAsync

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_COMMON_INSTANCES(WAsyncT, MONADPARALLEL)
