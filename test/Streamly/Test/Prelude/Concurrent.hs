-- |
-- Module      : Streamly.Test.Prelude.Concurrent
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE OverloadedLists #-}

module Streamly.Test.Prelude.Concurrent where

import Control.Concurrent (MVar, takeMVar, threadDelay, putMVar, newEmptyMVar)
import Control.Exception
       (BlockedIndefinitelyOnMVar(..), catches,
        BlockedIndefinitelyOnSTM(..), Handler(..))
import Control.Monad (void, when, forM_, replicateM_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState, get, modify, runStateT
                           , StateT(..), evalStateT)
import Data.Foldable (fold)
import Data.IORef (readIORef, modifyIORef, newIORef)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup, (<>))
#endif
import GHC.Word (Word8)
import Test.Hspec.QuickCheck
import Test.Hspec as H
import Test.QuickCheck
       (Property, withMaxSuccess)
import Test.QuickCheck.Monadic (monadicIO, run)

import Streamly.Prelude hiding (fold, replicate, replicateM, reverse)
import qualified Streamly.Prelude as S

import Streamly.Test.Common
import Streamly.Test.Prelude.Common

-------------------------------------------------------------------------------
-- Concurrent generation
-------------------------------------------------------------------------------

mvarExcHandler :: String -> BlockedIndefinitelyOnMVar -> IO ()
mvarExcHandler label BlockedIndefinitelyOnMVar =
    error $ label <> " " <> "BlockedIndefinitelyOnMVar\n"

stmExcHandler :: String -> BlockedIndefinitelyOnSTM -> IO ()
stmExcHandler label BlockedIndefinitelyOnSTM =
    error $ label <> " " <> "BlockedIndefinitelyOnSTM\n"

dbgMVar :: String -> IO () -> IO ()
dbgMVar label action =
    action `catches` [ Handler (mvarExcHandler label)
                     , Handler (stmExcHandler label)
                     ]

-- | first n actions takeMVar and the last action performs putMVar n times
mvarSequenceOp :: MVar () -> Word8 -> Word8 -> IO Word8
mvarSequenceOp mv n x = do
    let msg = show x <> "/" <> show n
    if x < n
    then dbgMVar ("take mvarSequenceOp " <> msg) (takeMVar mv) >>  return x
    else dbgMVar ("put mvarSequenceOp" <> msg)
            (replicateM_ (fromIntegral n) (putMVar mv ())) >> return x

concurrentMapM
    :: ([Word8] -> t IO Word8)
    -> ([Word8] -> [Word8] -> Bool)
    -> (Word8 -> MVar () -> t IO Word8 -> SerialT IO Word8)
    -> Word8
    -> Property
concurrentMapM constr eq op n =
    monadicIO $ do
        let list = [0..n]
        stream <- run $ do
            mv <- newEmptyMVar :: IO (MVar ())
            (S.toList . op n mv) (constr list)
        listEquals eq stream list

concurrentFromFoldable
    :: IsStream t
    => ([Word8] -> [Word8] -> Bool)
    -> (t IO Word8 -> SerialT IO Word8)
    -> Word8
    -> Property
concurrentFromFoldable eq op n =
    monadicIO $ do
        let list = [0..n]
        stream <- run $ do
            mv <- newEmptyMVar :: IO (MVar ())
            (S.toList . op) (S.fromFoldableM (fmap (mvarSequenceOp mv n) list))
        listEquals eq stream list

sourceUnfoldrM :: IsStream t => MVar () -> Word8 -> t IO Word8
sourceUnfoldrM mv n = S.unfoldrM step 0
    where
    -- argument must be integer to avoid overflow of word8 at 255
    step :: Int -> IO (Maybe (Word8, Int))
    step cnt = do
        let msg = show cnt <> "/" <> show n
        if cnt > fromIntegral n
        then return Nothing
        else do
            dbgMVar ("put sourceUnfoldrM " <> msg) (putMVar mv ())
            return (Just (fromIntegral cnt, cnt + 1))

-- Note that this test is not guaranteed to succeed, because there is no
-- guarantee of parallelism in case of Async/Ahead streams.
concurrentUnfoldrM
    :: IsStream t
    => ([Word8] -> [Word8] -> Bool)
    -> (t IO Word8 -> SerialT IO Word8)
    -> Word8
    -> Property
concurrentUnfoldrM eq op n =
    monadicIO $ do
        -- XXX we should test empty list case as well
        let list = [0..n]
        stream <- run $ do
            -- putStrLn $ "concurrentUnfoldrM: " <> show n
            mv <- newEmptyMVar :: IO (MVar ())
            cnt <- newIORef 0
            -- since unfoldr happens in parallel with the stream processing we
            -- can do two takeMVar in one iteration. If it is not parallel then
            -- this will not work and the test will fail.
            S.toList $ do
                x <- op (sourceUnfoldrM mv n)
                -- results may not be yielded in order, in case of
                -- Async/WAsync/Parallel. So we use an increasing count
                -- instead.
                i <- S.valueM $ readIORef cnt
                S.valueM $ modifyIORef cnt (+1)
                let msg = show i <> "/" <> show n
                S.valueM $
                    when (even i) $ do
                        dbgMVar ("first take concurrentUnfoldrM " <> msg)
                                (takeMVar mv)
                        when (n > i) $
                            dbgMVar ("second take concurrentUnfoldrM " <> msg)
                                     (takeMVar mv)
                return x
        listEquals eq stream list

concurrentOps
    :: IsStream t
    => ([Word8] -> t IO Word8)
    -> String
    -> ([Word8] -> [Word8] -> Bool)
    -> (t IO Word8 -> SerialT IO Word8)
    -> Spec
concurrentOps constr desc eq t = do
    let prop1 d p = prop d $ withMaxSuccess maxTestCount p

    prop1 (desc <> " fromFoldableM") $ concurrentFromFoldable eq t
    prop1 (desc <> " unfoldrM") $ concurrentUnfoldrM eq t
    -- we pass it the length of the stream n and an mvar mv.
    -- The stream is [0..n]. The threads communicate in such a way that the
    -- actions coming first in the stream are dependent on the last action. So
    -- if the stream is not processed concurrently it will block forever.
    -- Note that if the size of the stream is bigger than the thread limit
    -- then it will block even if it is concurrent.
    prop1 (desc <> " mapM") $
        concurrentMapM constr eq $ \n mv stream ->
            t $ S.mapM (mvarSequenceOp mv n) stream

-------------------------------------------------------------------------------
-- Concurrent Application
-------------------------------------------------------------------------------

concurrentApplication :: IsStream t
    => ([Word8] -> [Word8] -> Bool)
    -> (t IO Word8 -> SerialT IO Word8)
    -> Word8
    -> Property
concurrentApplication eq t n = withMaxSuccess maxTestCount $
    monadicIO $ do
        -- XXX we should test empty list case as well
        let list = [0..n]
        stream <- run $ do
            -- putStrLn $ "concurrentApplication: " <> show n
            mv <- newEmptyMVar :: IO (MVar ())
            -- since unfoldr happens in parallel with the stream processing we
            -- can do two takeMVar in one iteration. If it is not parallel then
            -- this will not work and the test will fail.
            (S.toList . t) $
                sourceUnfoldrM mv n |&
                    S.mapM (\x -> do
                        let msg = show x <> "/" <> show n
                        when (even x) $ do
                            dbgMVar ("first take concurrentApp " <> msg)
                                    (takeMVar mv)
                            when (n > x) $
                                dbgMVar ("second take concurrentApp " <> msg)
                                         (takeMVar mv)
                        return x)
        listEquals eq stream list

sourceUnfoldrM1 :: IsStream t => Word8 -> t IO Word8
sourceUnfoldrM1 n = S.unfoldrM step 0
    where
    -- argument must be integer to avoid overflow of word8 at 255
    step :: Int -> IO (Maybe (Word8, Int))
    step cnt =
        if cnt > fromIntegral n
        then return Nothing
        else return (Just (fromIntegral cnt, cnt + 1))

concurrentFoldlApplication :: Word8 -> Property
concurrentFoldlApplication n =
    monadicIO $ do
        -- XXX we should test empty list case as well
        let list = [0..n]
        stream <- run $
            sourceUnfoldrM1 n |&. S.foldlM' (\xs x -> return (x : xs)) (return [])
        listEquals (==) (reverse stream) list

concurrentFoldrApplication :: Word8 -> Property
concurrentFoldrApplication n =
    monadicIO $ do
        -- XXX we should test empty list case as well
        let list = [0..n]
        stream <- run $
            sourceUnfoldrM1 n |&. S.foldrM (\x xs -> xs >>= return . (x :))
                                           (return [])
        listEquals (==) stream list

-- Each snapshot carries an independent state. Multiple parallel tasks should
-- not affect each other's state. This is especially important when we run
-- multiple tasks in a single thread.
snapshot :: (IsStream t, MonadAsync m, MonadState Int m) => t m ()
snapshot =
    -- We deliberately use a replicate count 1 here, because a lower count
    -- catches problems that a higher count doesn't.
    S.replicateM 1 $ do
        -- Even though we modify the state here it should not reflect in other
        -- parallel tasks, it is local to each concurrent task.
        modify (+1) >> get >>= liftIO . (`shouldSatisfy` (==1))
        modify (+1) >> get >>= liftIO . (`shouldSatisfy` (==2))

snapshot1 :: (IsStream t, MonadAsync m, MonadState Int m) => t m ()
snapshot1 = S.replicateM 1000 $
    modify (+1) >> get >>= liftIO . (`shouldSatisfy` (==2))

snapshot2 :: (IsStream t, MonadAsync m, MonadState Int m) => t m ()
snapshot2 = S.replicateM 1000 $
    modify (+1) >> get >>= liftIO . (`shouldSatisfy` (==2))

stateComp
    :: ( IsStream t
       , MonadAsync m
       , Semigroup (t m ())
       , MonadIO (t m)
       , MonadState Int m
       , MonadState Int (t m)
       )
    => t m ()
stateComp = do
    -- Each task in a concurrent composition inherits the state and maintains
    -- its own modifications to it, not affecting the parent computation.
    snapshot <> (modify (+1) >> (snapshot1 <> snapshot2))
    -- The above modify statement does not affect our state because that is
    -- used in a parallel composition. In a serial composition it will affect
    -- our state.
    get >>= liftIO . (`shouldSatisfy` (== (0 :: Int)))

monadicStateSnapshot
    :: ( IsStream t
       , Semigroup (t (StateT Int IO) ())
       , MonadIO (t (StateT Int IO))
       , MonadState Int (t (StateT Int IO))
       )
    => (t (StateT Int IO) () -> SerialT (StateT Int IO) ()) -> IO ()
monadicStateSnapshot t = void $ runStateT (S.drain $ t stateComp) 0

stateCompOp
    :: (   AsyncT (StateT Int IO) ()
        -> AsyncT (StateT Int IO) ()
        -> AsyncT (StateT Int IO) ()
       )
    -> SerialT (StateT Int IO) ()
stateCompOp op = do
    -- Each task in a concurrent composition inherits the state and maintains
    -- its own modifications to it, not affecting the parent computation.
    asyncly (snapshot `op` (modify (+1) >> (snapshot1 `op` snapshot2)))
    -- The above modify statement does not affect our state because that is
    -- used in a parallel composition. In a serial composition it will affect
    -- our state.
    get >>= liftIO . (`shouldSatisfy` (== (0 :: Int)))

checkMonadicStateTransfer
    :: (IsStream t1, IsStream t2)
    => (    t1 (StateT Int IO) ()
        ->  t2 (StateT Int IO) ()
        ->  SerialT (StateT Int IO) a3 )
    -> IO ()
checkMonadicStateTransfer op = evalStateT str (0 :: Int)
  where
    str =
        S.drain $
        maxBuffer 1 $
        (serially $ S.mapM snapshoti $ S.fromList [1..10]) `op`
        (serially $ S.mapM snapshoti $ S.fromList [1..10])
    snapshoti y = do
        modify (+ 1)
        x <- get
        lift1 $ x `shouldBe` y
    lift1 m = StateT $ \s -> do
        a <- m
        return (a, s)

monadicStateSnapshotOp
    :: (   AsyncT (StateT Int IO) ()
        -> AsyncT (StateT Int IO) ()
        -> AsyncT (StateT Int IO) ()
       )
    -> IO ()
monadicStateSnapshotOp op = void $ runStateT (S.drain $ stateCompOp op) 0

takeInfinite :: IsStream t => (t IO Int -> SerialT IO Int) -> Spec
takeInfinite t =
    it "take 1" $
        S.drain (t $ S.take 1 $ S.repeatM (print "hello" >> return (1::Int)))
        `shouldReturn` ()

main :: IO ()
main = hspec
    $ H.parallel
#ifdef COVERAGE_BUILD
    $ modifyMaxSuccess (const 10)
#endif
    $ do
    -- We can have these in Test.Prelude, but I think it's unnecessary.
    let serialOps :: IsStream t => ((SerialT IO a -> t IO a) -> Spec) -> Spec
        serialOps spec = mapOps spec $ makeOps serially
#ifndef COVERAGE_BUILD
            <> [("rate AvgRate 0.00000001", serially . avgRate 0.00000001)]
            <> [("maxBuffer -1", serially . maxBuffer (-1))]
#endif

    let aheadOps :: IsStream t => ((AheadT IO a -> t IO a) -> Spec) -> Spec
        aheadOps spec = mapOps spec $ makeOps aheadly
#ifndef COVERAGE_BUILD
              <> [("maxBuffer (-1)", aheadly . maxBuffer (-1))]
#endif

    let asyncOps :: IsStream t => ((AsyncT IO a -> t IO a) -> Spec) -> Spec
        asyncOps spec = mapOps spec $ makeOps asyncly
#ifndef COVERAGE_BUILD
            <> [("maxBuffer (-1)", asyncly . maxBuffer (-1))]
#endif

    -- For concurrent application test we need a buffer of at least size 2 to
    -- allow two threads to run.
#ifndef COVERAGE_BUILD
    let makeConcurrentAppOps :: IsStream t
            => (t m a -> c) -> [(String, t m a -> c)]
#endif
        makeConcurrentAppOps t = makeCommonOps t ++
            [
#ifndef COVERAGE_BUILD
              ("maxBuffer 2", t . maxBuffer 2)
#endif
            ]

#ifndef COVERAGE_BUILD
    let parallelCommonOps :: IsStream t => [(String, ParallelT m a -> t m a)]
#else
    let parallelCommonOps :: [(String, ParallelT m a -> t m a)]
#endif
        parallelCommonOps = []
#ifndef COVERAGE_BUILD
            <> [("rate AvgRate 0.00000001", parallely . avgRate 0.00000001)]
            <> [("maxBuffer (-1)", parallely . maxBuffer (-1))]
#endif

    let parallelConcurrentAppOps :: IsStream t
            => ((ParallelT IO a -> t IO a) -> Spec) -> Spec
        parallelConcurrentAppOps spec =
            mapOps spec $ makeConcurrentAppOps parallely <> parallelCommonOps

    -- These tests won't work with maxBuffer or maxThreads set to 1, so we
    -- exclude those cases from these.
#ifndef COVERAGE_BUILD
    let mkOps :: IsStream t => (t m a -> c) -> [(String, t m a -> c)]
#else
    let mkOps :: t -> [(String, t)]
    -- let mkOps :: (t m a -> c) -> [(String, t m a -> c)]
#endif
        mkOps t =
            [ ("default", t)
#ifndef COVERAGE_BUILD
            , ("rate Nothing", t . rate Nothing)
            , ("maxBuffer 0", t . maxBuffer 0)
            , ("maxThreads 0", t . maxThreads 0)
            , ("maxThreads 0", t . maxThreads (-1))
#endif
            ]

    let forOps ops spec = forM_ ops (\(desc, f) -> describe desc $ spec f)
    describe "Stream concurrent operations" $ do
        forOps (mkOps aheadly)   $ concurrentOps S.fromFoldable "aheadly" (==)
        forOps (mkOps asyncly)   $ concurrentOps S.fromFoldable "asyncly" sortEq
        forOps (mkOps wAsyncly)  $ concurrentOps S.fromFoldable "wAsyncly" sortEq
        forOps (mkOps parallely) $ concurrentOps S.fromFoldable "parallely" sortEq

        forOps (mkOps aheadly)   $ concurrentOps folded "aheadly folded" (==)
        forOps (mkOps asyncly)   $ concurrentOps folded "asyncly folded" sortEq
        forOps (mkOps wAsyncly)  $ concurrentOps folded "wAsyncly folded" sortEq
        forOps (mkOps parallely) $ concurrentOps folded "parallely folded" sortEq

    describe "Concurrent application" $ do
        serialOps $ prop "serial" . concurrentApplication (==)
        asyncOps  $ prop "async" . concurrentApplication sortEq
        aheadOps  $ prop "ahead" . concurrentApplication (==)

        parallelConcurrentAppOps $
            prop "parallel" . concurrentApplication sortEq

        prop "concurrent foldr application" $ withMaxSuccess maxTestCount
            concurrentFoldrApplication
        prop "concurrent foldl application" $ withMaxSuccess maxTestCount
            concurrentFoldlApplication

    describe "take on infinite concurrent stream" $ takeInfinite asyncly
    describe "take on infinite concurrent stream" $ takeInfinite wAsyncly
    describe "take on infinite concurrent stream" $ takeInfinite aheadly

    ---------------------------------------------------------------------------
    -- Monadic state transfer in concurrent tasks
    ---------------------------------------------------------------------------

    describe "Monadic state transfer in concurrent tasks" $ do
        -- XXX Can we write better test cases to hit every case?
        it "async: state is saved and used if the work is partially enqueued"
            (checkMonadicStateTransfer async)
        it "wAsync: state is saved and used if the work is partially enqueued"
            (checkMonadicStateTransfer wAsync)
        it "ahead: state is saved and used if the work is partially enqueued"
            (checkMonadicStateTransfer ahead)

    ---------------------------------------------------------------------------
    -- Monadic state snapshot in concurrent tasks
    ---------------------------------------------------------------------------

    describe "Monadic state snapshot in concurrent tasks" $ do
        it "asyncly maintains independent states in concurrent tasks"
            (monadicStateSnapshot asyncly)
        it "asyncly limited maintains independent states in concurrent tasks"
            (monadicStateSnapshot (asyncly . S.take 10000))

        it "wAsyncly maintains independent states in concurrent tasks"
            (monadicStateSnapshot wAsyncly)
        it "wAsyncly limited maintains independent states in concurrent tasks"
            (monadicStateSnapshot (wAsyncly . S.take 10000))

        it "aheadly maintains independent states in concurrent tasks"
            (monadicStateSnapshot aheadly)
        it "aheadly limited maintains independent states in concurrent tasks"
            (monadicStateSnapshot (aheadly . S.take 10000))
        it "parallely maintains independent states in concurrent tasks"
            (monadicStateSnapshot parallely)


        it "async maintains independent states in concurrent tasks"
            (monadicStateSnapshotOp async)
        it "ahead maintains independent states in concurrent tasks"
            (monadicStateSnapshotOp ahead)
        it "wAsync maintains independent states in concurrent tasks"
            (monadicStateSnapshotOp wAsync)
        it "parallel maintains independent states in concurrent tasks"
            (monadicStateSnapshotOp S.parallel)

    ---------------------------------------------------------------------------
    -- Slower tests are at the end
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- Thread limits
    ---------------------------------------------------------------------------

    it "asyncly crosses thread limit (2000 threads)" $
        S.drain (asyncly $ fold $
                   replicate 2000 $ S.valueM $ threadDelay 1000000)
        `shouldReturn` ()

    it "aheadly crosses thread limit (4000 threads)" $
        S.drain (aheadly $ fold $
                   replicate 4000 $ S.valueM $ threadDelay 1000000)
        `shouldReturn` ()

#ifdef DEVBUILD
    describe "restricts concurrency and cleans up extra tasks" $ do
        it "take 1 asyncly" $ checkCleanup 2 asyncly (S.take 1)
        it "take 1 wAsyncly" $ checkCleanup 2 wAsyncly (S.take 1)
        it "take 1 aheadly" $ checkCleanup 2 aheadly (S.take 1)

        it "takeWhile (< 0) asyncly" $ checkCleanup 2 asyncly (S.takeWhile (< 0))
        it "takeWhile (< 0) wAsyncly" $ checkCleanup 2 wAsyncly (S.takeWhile (< 0))
        it "takeWhile (< 0) aheadly" $ checkCleanup 2 aheadly (S.takeWhile (< 0))
#endif
