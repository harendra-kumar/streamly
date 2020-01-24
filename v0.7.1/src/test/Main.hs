{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, try, ErrorCall(..), catch, throw)
import Control.Monad (void)
import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad.Error.Class (throwError, MonadError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState, get, modify, runStateT, StateT)
import Control.Monad.Trans.Except (runExceptT, ExceptT)
import Data.Foldable (forM_, fold)
import Data.List (sort)
import System.Mem (performMajorGC)

import Data.IORef
import Test.Hspec as H

import Streamly
import Streamly.Prelude ((.:), nil)
import qualified Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as IP

singleton :: IsStream t => a -> t m a
singleton a = a .: nil

toListSerial :: SerialT IO a -> IO [a]
toListSerial = S.toList . serially

toListInterleaved :: WSerialT IO a -> IO [a]
toListInterleaved = S.toList . wSerially

toListAsync :: AsyncT IO a -> IO [a]
toListAsync = S.toList . asyncly

toListParallel :: WAsyncT IO a -> IO [a]
toListParallel = S.toList . wAsyncly

main :: IO ()
main = hspec $ do
    parallelTests

    describe "restricts concurrency and cleans up extra tasks" $ do
        it "take 1 asyncly" $ checkCleanup 2 asyncly (S.take 1)
        it "take 1 wAsyncly" $ checkCleanup 2 wAsyncly (S.take 1)
        it "take 1 aheadly" $ checkCleanup 2 aheadly (S.take 1)

        it "takeWhile (< 0) asyncly" $ checkCleanup 2 asyncly (S.takeWhile (< 0))
        it "takeWhile (< 0) wAsyncly" $ checkCleanup 2 wAsyncly (S.takeWhile (< 0))
        it "takeWhile (< 0) aheadly" $ checkCleanup 2 aheadly (S.takeWhile (< 0))

#ifdef DEVBUILD
    let timed :: (IsStream t, Monad (t IO)) => Int -> t IO Int
        timed x = S.yieldM (threadDelay (x * 100000)) >> return x

    -- These are not run parallely because the timing gets affected
    -- unpredictably when other tests are running on the same machine.
    --
    -- Also, they fail intermittently due to scheduling delays, so not run on
    -- CI machines.
    describe "Nested parallel and serial compositions" $ do
        let t = timed
            p = wAsyncly
            s = serially
        {-
        -- This is not correct, the result can also be [4,4,8,0,8,0,2,2]
        -- because of parallelism of [8,0] and [8,0].
        it "Nest <|>, <>, <|> (1)" $
            let t = timed
             in toListSerial (
                    ((t 8 <|> t 4) <> (t 2 <|> t 0))
                <|> ((t 8 <|> t 4) <> (t 2 <|> t 0)))
            `shouldReturn` ([4,4,8,8,0,0,2,2])
        -}
        it "Nest <|>, <>, <|> (2)" $
            (S.toList . wAsyncly) (
                   s (p (t 4 <> t 8) <> p (t 1 <> t 2))
                <> s (p (t 4 <> t 8) <> p (t 1 <> t 2)))
            `shouldReturn` ([4,4,8,8,1,1,2,2])
        -- FIXME: These two keep failing intermittently on Mac OS X
        -- Need to examine and fix the tests.
        {-
        it "Nest <|>, <=>, <|> (1)" $
            let t = timed
             in toListSerial (
                    ((t 8 <|> t 4) <=> (t 2 <|> t 0))
                <|> ((t 9 <|> t 4) <=> (t 2 <|> t 0)))
            `shouldReturn` ([4,4,0,0,8,2,9,2])
        it "Nest <|>, <=>, <|> (2)" $
            let t = timed
             in toListSerial (
                    ((t 4 <|> t 8) <=> (t 1 <|> t 2))
                <|> ((t 4 <|> t 9) <=> (t 1 <|> t 2)))
            `shouldReturn` ([4,4,1,1,8,2,9,2])
        -}
        it "Nest <|>, <|>, <|>" $
            (S.toList . wAsyncly) (
                    ((t 4 <> t 8) <> (t 0 <> t 2))
                <> ((t 4 <> t 8) <> (t 0 <> t 2)))
            `shouldReturn` ([0,0,2,2,4,4,8,8])

        -- parallely fails on CI machines, may need more difference in times of
        -- the events, but that would make tests even slower.
        it "take 1 parallely" $ checkCleanup 3 parallely (S.take 1)
        it "takeWhile (< 0) parallely" $ checkCleanup 3 parallely (S.takeWhile (< 0))

        testFoldOpsCleanup "head" S.head
        testFoldOpsCleanup "null" S.null
        testFoldOpsCleanup "elem" (S.elem 0)
        testFoldOpsCleanup "notElem" (S.notElem 0)
        testFoldOpsCleanup "elemIndex" (S.elemIndex 0)
        -- S.lookup
        testFoldOpsCleanup "notElem" (S.notElem 0)
        testFoldOpsCleanup "find" (S.find (==0))
        testFoldOpsCleanup "findIndex" (S.findIndex (==0))
        testFoldOpsCleanup "all" (S.all (==1))
        testFoldOpsCleanup "any" (S.any (==0))
        testFoldOpsCleanup "and" (S.and . S.map (==1))
        testFoldOpsCleanup "or" (S.or . S.map (==0))
#endif

    ---------------------------------------------------------------------------
    -- Semigroup/Monoidal Composition strict ordering checks
    ---------------------------------------------------------------------------

    -- test both (<>) and mappend to make sure we are using correct instance
    -- for Monoid that is using the right version of semigroup. Instance
    -- deriving can cause us to pick wrong instances sometimes.

    describe "WSerial interleaved (<>) ordering check" $ interleaveCheck wSerially (<>)
    describe "WSerial interleaved mappend ordering check" $ interleaveCheck wSerially mappend

    -- describe "WAsync interleaved (<>) ordering check" $ interleaveCheck wAsyncly (<>)
    -- describe "WAsync interleaved mappend ordering check" $ interleaveCheck wAsyncly mappend

    describe "Async (<>) time order check" $ parallelCheck asyncly (<>)
    describe "Async mappend time order check" $ parallelCheck asyncly mappend

    -- XXX this keeps failing intermittently, need to investigate
    -- describe "WAsync (<>) time order check" $ parallelCheck wAsyncly (<>)
    -- describe "WAsync mappend time order check" $ parallelCheck wAsyncly mappend

    describe "Parallel (<>) time order check" $ parallelCheck parallely (<>)
    describe "Parallel mappend time order check" $ parallelCheck parallely mappend

checkCleanup :: IsStream t
    => Int
    -> (t IO Int -> SerialT IO Int)
    -> (t IO Int -> t IO Int)
    -> IO ()
checkCleanup d t op = do
    r <- newIORef (-1 :: Int)
    S.drain . serially $ do
        _ <- t $ op $ delay r 0 S.|: delay r 1 S.|: delay r 2 S.|: S.nil
        return ()
    performMajorGC
    threadDelay 500000
    res <- readIORef r
    res `shouldBe` 0
    where
    delay ref i = threadDelay (i*d*100000) >> writeIORef ref i >> return i

#ifdef DEVBUILD
checkCleanupFold :: IsStream t
    => (t IO Int -> SerialT IO Int)
    -> (SerialT IO Int -> IO (Maybe Int))
    -> IO ()
checkCleanupFold t op = do
    r <- newIORef (-1 :: Int)
    _ <- op $ t $ delay r 0 S.|: delay r 1 S.|: delay r 2 S.|: S.nil
    performMajorGC
    threadDelay 500000
    res <- readIORef r
    res `shouldBe` 0
    where
    delay ref i = threadDelay (i*200000) >> writeIORef ref i >> return i

testFoldOpsCleanup :: String -> (SerialT IO Int -> IO a) -> Spec
testFoldOpsCleanup name f = do
    let testOp op x = op x >> return Nothing
    it (name <> " asyncly") $ checkCleanupFold asyncly (testOp f)
    it (name <> " wAsyncly") $ checkCleanupFold wAsyncly (testOp f)
    it (name <> " aheadly") $ checkCleanupFold aheadly (testOp f)
    it (name <> " parallely") $ checkCleanupFold parallely (testOp f)
#endif

parallelTests :: SpecWith ()
parallelTests = H.parallel $ do
    describe "Runners" $ do
        -- XXX move these to property tests
        -- XXX use an IORef to store and check the side effects
        it "simple serially" $
            (S.drain . serially) (return (0 :: Int)) `shouldReturn` ()
        it "simple serially with IO" $
            (S.drain . serially) (S.yieldM $ putStrLn "hello") `shouldReturn` ()

    describe "Empty" $ -- do
        it "Monoid - mempty" $
            toListSerial mempty `shouldReturn` ([] :: [Int])
        -- it "Alternative - empty" $
        --     (toListSerial empty) `shouldReturn` ([] :: [Int])
        -- it "MonadPlus - mzero" $
        --     (toListSerial mzero) `shouldReturn` ([] :: [Int])

    ---------------------------------------------------------------------------
    -- Functor
    ---------------------------------------------------------------------------

    describe "Functor (fmap)" $ do
        -- XXX we should do these through property tests by using a
        -- construction via list fold construction method.
        it "fmap on composed (<>)" $
            toListSerial (fmap (+1) (return 1 <> return 2))
                `shouldReturn` ([2,3] :: [Int])

        it "fmap on composed (<>)" $
            sort <$> toListParallel (fmap (+1) (return 1 <> return 2))
                `shouldReturn` ([2,3] :: [Int])

    ---------------------------------------------------------------------------
    -- Applicative
    ---------------------------------------------------------------------------

    describe "Applicative" $ do
        -- XXX we should do these through property tests by using a
        -- construction via list fold construction method.
        it "Apply - serial composed first argument" $
            toListSerial ((,) <$> (return 1 <> return 2) <*> return 3)
                `shouldReturn` ([(1,3),(2,3)] :: [(Int, Int)])

        it "Apply - serial composed second argument" $
            toListSerial ((,) <$> return 1 <*> (return 2 <> return 3))
                `shouldReturn` ([(1,2),(1,3)] :: [(Int, Int)])

        it "Apply - parallel composed first argument" $
            sort <$> toListParallel ((,) <$> (return 1 <> return 2) <*> return 3)
                `shouldReturn` ([(1,3),(2,3)] :: [(Int, Int)])

        it "Apply - parallel composed second argument" $
            sort <$> toListParallel ((,) <$> return 1 <*> (return 2 <> return 3))
                `shouldReturn` ([(1,2),(1,3)] :: [(Int, Int)])

    ---------------------------------------------------------------------------
    -- Monoidal Compositions, multiset equality checks
    ---------------------------------------------------------------------------

    describe "Serial Composition" $ compose serially mempty id
    describe "Ahead Composition" $ compose aheadly mempty id
    describe "WSerial Composition" $ compose wSerially mempty sort
    describe "Async Composition" $ compose asyncly mempty sort
    describe "WAsync Composition" $ compose wAsyncly mempty sort
    describe "Parallel Composition" $ compose parallely mempty sort
    describe "Semigroup Composition for ZipSerial" $ compose zipSerially mempty id
    describe "Semigroup Composition for ZipAsync" $ compose zipAsyncly mempty id
    -- XXX need to check alternative compositions as well

    ---------------------------------------------------------------------------
    -- TBD Monoidal composition combinations
    ---------------------------------------------------------------------------

    -- TBD need more such combinations to be tested.
    describe "serial <> and serial <>" $
        composeAndComposeSimple serially serially (repeat [1 .. 9])
    describe "ahead <> and ahead <>" $
        composeAndComposeSimple aheadly aheadly (repeat [1 .. 9])
    describe "ahead <> and serial <>" $
        composeAndComposeSimple aheadly serially (repeat [1 .. 9])
    describe "serial <> and ahead <>" $
        composeAndComposeSimple serially aheadly (repeat [1 .. 9])

    describe "<> and <=>" $ composeAndComposeSimple
      serially
      wSerially
      [ [1 .. 9]
       , [1 .. 9]
       , [1, 3, 2, 4, 6, 5, 7, 9, 8]
       , [1, 3, 2, 4, 6, 5, 7, 9, 8]
      ]

    describe "<=> and <=>" $ composeAndComposeSimple
      wSerially
      wSerially
      [ [1, 4, 2, 7, 3, 5, 8, 6, 9]
       , [1, 7, 4, 8, 2, 9, 5, 3, 6]
       , [1, 4, 3, 7, 2, 6, 9, 5, 8]
       , [1, 7, 4, 9, 3, 8, 6, 2, 5]
      ]

    describe "<=> and <>" $ composeAndComposeSimple
      wSerially
      serially
      [ [1, 4, 2, 7, 3, 5, 8, 6, 9]
       , [1, 7, 4, 8, 2, 9, 5, 3, 6]
       , [1, 4, 2, 7, 3, 5, 8, 6, 9]
       , [1, 7, 4, 8, 2, 9, 5, 3, 6]
      ]

    ---------------------------------------------------------------------------
    -- Monoidal composition recursion loops
    ---------------------------------------------------------------------------

    describe "Serial loops" $ loops serially id reverse
    describe "Ahead loops" $ loops aheadly id reverse
    describe "Async parallel loops" $ loops asyncly sort sort
    describe "WAsync loops" $ loops wAsyncly sort sort
    describe "parallel loops" $ loops parallely sort sort

    ---------------------------------------------------------------------------
    -- Bind and monoidal composition combinations
    ---------------------------------------------------------------------------

    describe "Bind and compose Stream 1" $ bindAndComposeSimple serially serially
    describe "Bind and compose Stream 2" $ bindAndComposeSimple serially wSerially
    describe "Bind and compose Stream 3" $ bindAndComposeSimple serially asyncly
    describe "Bind and compose Stream 4" $ bindAndComposeSimple serially wAsyncly
    describe "Bind and compose Stream 5" $ bindAndComposeSimple serially parallely
    describe "Bind and compose Stream 6" $ bindAndComposeSimple serially aheadly

    describe "Bind and compose Ahead Stream 0" $ bindAndComposeSimple aheadly aheadly
    describe "Bind and compose Ahead Stream 1" $ bindAndComposeSimple aheadly serially
    describe "Bind and compose Ahead Stream 2" $ bindAndComposeSimple aheadly wSerially
    describe "Bind and compose Ahead Stream 3" $ bindAndComposeSimple aheadly asyncly
    describe "Bind and compose Ahead Stream 4" $ bindAndComposeSimple aheadly wAsyncly
    describe "Bind and compose Ahead Stream 5" $ bindAndComposeSimple aheadly parallely

    describe "Bind and compose Costream 1" $ bindAndComposeSimple wSerially serially
    describe "Bind and compose Costream 2" $ bindAndComposeSimple wSerially wSerially
    describe "Bind and compose Costream 3" $ bindAndComposeSimple wSerially asyncly
    describe "Bind and compose Costream 4" $ bindAndComposeSimple wSerially wAsyncly
    describe "Bind and compose Costream 5" $ bindAndComposeSimple wSerially parallely
    describe "Bind and compose Costream 6" $ bindAndComposeSimple wSerially aheadly

    describe "Bind and compose Async 1" $ bindAndComposeSimple asyncly serially
    describe "Bind and compose Async 2" $ bindAndComposeSimple asyncly wSerially
    describe "Bind and compose Async 3" $ bindAndComposeSimple asyncly asyncly
    describe "Bind and compose Async 4" $ bindAndComposeSimple asyncly wAsyncly
    describe "Bind and compose Async 5" $ bindAndComposeSimple asyncly parallely
    describe "Bind and compose Async 6" $ bindAndComposeSimple asyncly aheadly

    describe "Bind and compose WAsync 1" $ bindAndComposeSimple wAsyncly serially
    describe "Bind and compose WAsync 2" $ bindAndComposeSimple wAsyncly wSerially
    describe "Bind and compose WAsync 3" $ bindAndComposeSimple wAsyncly asyncly
    describe "Bind and compose WAsync 4" $ bindAndComposeSimple wAsyncly wAsyncly
    describe "Bind and compose WAsync 5" $ bindAndComposeSimple wAsyncly parallely
    describe "Bind and compose WAsync 6" $ bindAndComposeSimple wAsyncly aheadly

    describe "Bind and compose Parallel 1" $ bindAndComposeSimple parallely serially
    describe "Bind and compose Parallel 2" $ bindAndComposeSimple parallely wSerially
    describe "Bind and compose Parallel 3" $ bindAndComposeSimple parallely asyncly
    describe "Bind and compose Parallel 4" $ bindAndComposeSimple parallely wAsyncly
    describe "Bind and compose Parallel 5" $ bindAndComposeSimple parallely parallely
    describe "Bind and compose Parallel 6" $ bindAndComposeSimple parallely aheadly

    let fldr, fldl :: (IsStream t, Semigroup (t IO Int)) => [t IO Int] -> t IO Int
        fldr = foldr (<>) nil
        fldl = foldl (<>) nil

    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy serially serially k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy serially wSerially k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy serially asyncly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy serially wAsyncly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy serially parallely k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose serially aheadly" $ bindAndComposeHierarchy serially aheadly k

    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose aheadly serially" $ bindAndComposeHierarchy aheadly serially k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose aheadly wSerially" $ bindAndComposeHierarchy aheadly wSerially k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose aheadly asyncly" $ bindAndComposeHierarchy aheadly asyncly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose aheadly wAsyncly" $ bindAndComposeHierarchy aheadly wAsyncly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose aheadly parallely" $ bindAndComposeHierarchy aheadly parallely k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose serially aheadly" $ bindAndComposeHierarchy aheadly aheadly k

    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy wSerially serially k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy wSerially wSerially k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy wSerially asyncly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy wSerially wAsyncly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy wSerially parallely k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose wserially aheadly" $ bindAndComposeHierarchy wSerially aheadly k

    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy asyncly serially k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy asyncly wSerially k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy asyncly asyncly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy asyncly wAsyncly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy asyncly parallely k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose asyncly aheadly" $ bindAndComposeHierarchy asyncly aheadly k

    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy wAsyncly serially k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy wAsyncly wSerially k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy wAsyncly asyncly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy wAsyncly wAsyncly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy wAsyncly parallely k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose wAsyncly aheadly" $ bindAndComposeHierarchy wAsyncly aheadly k

    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy parallely serially k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy parallely wSerially k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy parallely asyncly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy parallely wAsyncly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy parallely parallely k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose parallely aheadly" $ bindAndComposeHierarchy parallely aheadly k

    -- Nest two lists using different styles of product compositions
    it "Nests two streams using monadic serial composition" nestTwoSerial
    it "Nests two streams using monadic ahead composition" nestTwoAhead
    it "Nests two streams using monadic interleaved composition" nestTwoInterleaved
    it "Nests two streams using monadic Async composition" nestTwoAsync
    it "Nests two streams using monadic WAsync composition" nestTwoWAsync
    it "Nests two streams using monadic parallel composition" nestTwoParallel

    it "Nests two streams using applicative serial composition" nestTwoSerialApp
    it "Nests two streams using applicative ahead composition" nestTwoAheadApp
    it "Nests two streams using applicative interleaved composition" nestTwoInterleavedApp
    it "Nests two streams using applicative Async composition" nestTwoAsyncApp
    it "Nests two streams using applicative WAsync composition" nestTwoWAsyncApp
    it "Nests two streams using applicative parallel composition" nestTwoParallelApp

    ---------------------------------------------------------------------------
    -- TBD Bind and Bind combinations
    ---------------------------------------------------------------------------

    -- TBD combine all binds and all compose in one example
    describe "Miscellaneous combined examples" mixedOps
    describe "Miscellaneous combined examples aheadly" mixedOpsAheadly
    describe "Simple MonadError and MonadThrow" simpleMonadError

    {-
    describe "Composed MonadError serially" $ composeWithMonadError serially
    describe "Composed MonadError wSerially" $ composeWithMonadError wSerially
    describe "Composed MonadError asyncly" $ composeWithMonadError asyncly
    describe "Composed MonadError wAsyncly" $ composeWithMonadError wAsyncly
    -}

    describe "Composed MonadThrow serially" $ composeWithMonadThrow serially
    describe "Composed MonadThrow wSerially" $ composeWithMonadThrow wSerially
    describe "Composed MonadThrow asyncly" $ composeWithMonadThrow asyncly
    describe "Composed MonadThrow wAsyncly" $ composeWithMonadThrow wAsyncly
    describe "Composed MonadThrow parallely" $ composeWithMonadThrow parallely
    describe "Composed MonadThrow aheadly" $ composeWithMonadThrow aheadly

    describe "take on infinite concurrent stream" $ takeInfinite asyncly
    describe "take on infinite concurrent stream" $ takeInfinite wAsyncly
    describe "take on infinite concurrent stream" $ takeInfinite aheadly

    ---------------------------------------------------------------------------
    -- Some ad-hoc tests that failed at times
    ---------------------------------------------------------------------------

    it "takes n from stream of streams" (takeCombined 1 aheadly)
    it "takes n from stream of streams" (takeCombined 2 asyncly)
    it "takes n from stream of streams" (takeCombined 3 wAsyncly)

    ---------------------------------------------------------------------------
    -- Left folds are strict enough
    ---------------------------------------------------------------------------

#ifdef DEVBUILD
    it "foldx is strict enough" checkFoldxStrictness
    it "scanx is strict enough" checkScanxStrictness
    it "foldxM is strict enough" (checkFoldMStrictness foldxMStrictCheck)
#endif
    it "foldl' is strict enough" checkFoldl'Strictness
    it "scanl' is strict enough" checkScanl'Strictness
    it "foldlM' is strict enough" (checkFoldMStrictness foldlM'StrictCheck)
    it "scanlM' is strict enough" (checkScanlMStrictness scanlM'StrictCheck)

    ---------------------------------------------------------------------------
    -- Right folds are lazy enough
    ---------------------------------------------------------------------------

    it "foldrM is lazy enough" checkFoldrLaziness

    ---------------------------------------------------------------------------
    -- Monadic state snapshot in concurrent tasks
    ---------------------------------------------------------------------------

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
        (monadicStateSnapshotOp Streamly.parallel)

    ---------------------------------------------------------------------------
    -- Slower tests are at the end
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- Thread limits
    ---------------------------------------------------------------------------

    it "asyncly crosses thread limit (2000 threads)" $
        S.drain (asyncly $ fold $
                   replicate 2000 $ S.yieldM $ threadDelay 1000000)
        `shouldReturn` ()

    it "aheadly crosses thread limit (4000 threads)" $
        S.drain (aheadly $ fold $
                   replicate 4000 $ S.yieldM $ threadDelay 1000000)
        `shouldReturn` ()

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

monadicStateSnapshotOp
    :: (   AsyncT (StateT Int IO) ()
        -> AsyncT (StateT Int IO) ()
        -> AsyncT (StateT Int IO) ()
       )
    -> IO ()
monadicStateSnapshotOp op = void $ runStateT (S.drain $ stateCompOp op) 0

takeCombined :: (Monad m, Semigroup (t m Int), Show a, Eq a, IsStream t)
    => Int -> (t m Int -> SerialT IO a) -> IO ()
takeCombined n t = do
    let constr = S.fromFoldable
    r <- (S.toList . t) $
            S.take n (constr ([] :: [Int]) <> constr ([] :: [Int]))
    r `shouldBe` []

checkFoldrLaziness :: IO ()
checkFoldrLaziness = do
    S.foldrM (\x xs -> if odd x then return True else xs)
             (return False) (S.fromList (2:4:5:undefined :: [Int]))
        `shouldReturn` True

    S.toList (IP.foldrS (\x xs -> if odd x then return True else xs)
                        (return False)
                        $ (S.fromList (2:4:5:undefined) :: SerialT IO Int))
        `shouldReturn` [True]

    S.toList (IP.foldrT (\x xs -> if odd x then return True else xs)
                        (return False)
                        $ (S.fromList (2:4:5:undefined) :: SerialT IO Int))
        `shouldReturn` [True]

#ifdef DEVBUILD
checkFoldxStrictness :: IO ()
checkFoldxStrictness = do
  let s = return (1 :: Int) `S.consM` error "failure"
  catch (S.foldx (\_ a -> if a == 1 then error "success" else "done")
                      "begin" id s)
    (\e -> case e of
            ErrorCall err -> return err
            _ -> throw e)
    `shouldReturn` "success"
#endif

checkFoldl'Strictness :: IO ()
checkFoldl'Strictness = do
  let s = return (1 :: Int) `S.consM` error "failure"
  catch (S.foldl' (\_ a -> if a == 1 then error "success" else "done")
                      "begin" s)
    (\e -> case e of
            ErrorCall err -> return err
            _ -> throw e)
    `shouldReturn` "success"

#ifdef DEVBUILD
checkScanxStrictness :: IO ()
checkScanxStrictness = do
  let s = return (1 :: Int) `S.consM` error "failure"
  catch
    (S.drain (
        S.scanx (\_ a ->
                    if a == 1
                    then error "success"
                    else "done")
                "begin" id s
        )
        >> return "finished"
    )
    (\e -> case e of
            ErrorCall err -> return err
            _ -> throw e)
    `shouldReturn` "success"
#endif

checkScanl'Strictness :: IO ()
checkScanl'Strictness = do
    let s = return (1 :: Int) `S.consM` error "failure"
    catch
        (S.drain
             (S.scanl'
                  (\_ a ->
                       if a == 1
                           then error "success"
                           else "done")
                  "begin"
                  s)
             >> return "finished"
        )
        (\e -> case e of
                ErrorCall err -> return err
                _ -> throw e)
        `shouldReturn` "success"

foldlM'StrictCheck :: IORef Int -> SerialT IO Int -> IO ()
foldlM'StrictCheck ref = S.foldlM' (\_ _ -> writeIORef ref 1) ()

#ifdef DEVBUILD
foldxMStrictCheck :: IORef Int -> SerialT IO Int -> IO ()
foldxMStrictCheck ref = S.foldxM (\_ _ -> writeIORef ref 1) (return ()) return
#endif

checkFoldMStrictness :: (IORef Int -> SerialT IO Int -> IO ()) -> IO ()
checkFoldMStrictness f = do
  ref <- newIORef 0
  let s = return 1 `S.consM` error "x"
  catch (f ref s) (\(_ :: ErrorCall) -> return ())
  readIORef ref `shouldReturn` 1

scanlM'StrictCheck :: IORef Int -> SerialT IO Int -> SerialT IO ()
scanlM'StrictCheck ref = S.scanlM' (\_ _ -> writeIORef ref 1) ()

checkScanlMStrictness :: (IORef Int -> SerialT IO Int -> SerialT IO ()) -> IO ()
checkScanlMStrictness f = do
  ref <- newIORef 0
  let s = return 1 `S.consM` error "x"
  catch (S.drain $ f ref s) (\(_ :: ErrorCall) -> return ())
  readIORef ref `shouldReturn` 1

takeInfinite :: IsStream t => (t IO Int -> SerialT IO Int) -> Spec
takeInfinite t =
    it "take 1" $
        S.drain (t $ S.take 1 $ S.repeatM (print "hello" >> return (1::Int)))
        `shouldReturn` ()

-- XXX need to test that we have promptly cleaned up everything after the error
-- XXX We can also check the output that we are expected to get before the
-- error occurs.

newtype ExampleException = ExampleException String deriving (Eq, Show)

instance Exception ExampleException

simpleMonadError :: Spec
simpleMonadError = do
{-
    it "simple runExceptT" $ do
        (runExceptT $ S.drain $ return ())
        `shouldReturn` (Right () :: Either String ())
    it "simple runExceptT with error" $ do
        (runExceptT $ S.drain $ throwError "E") `shouldReturn` Left "E"
        -}
    it "simple try" $
        try (S.drain $ return ())
        `shouldReturn` (Right () :: Either ExampleException ())
    it "simple try with throw error" $
        try (S.drain $ throwM $ ExampleException "E")
        `shouldReturn` (Left (ExampleException "E") :: Either ExampleException ())

composeWithMonadThrow
    :: ( IsStream t
       , Semigroup (t IO Int)
       , MonadThrow (t IO)
       )
    => (t IO Int -> SerialT IO Int) -> Spec
composeWithMonadThrow t = do
    it "Compose throwM, nil" $
        try (tl (throwM (ExampleException "E") <> S.nil))
        `shouldReturn` (Left (ExampleException "E") :: Either ExampleException [Int])
    it "Compose nil, throwM" $
        try (tl (S.nil <> throwM (ExampleException "E")))
        `shouldReturn` (Left (ExampleException "E") :: Either ExampleException [Int])
    oneLevelNestedSum "serially" serially
    oneLevelNestedSum "wSerially" wSerially
    oneLevelNestedSum "asyncly" asyncly
    oneLevelNestedSum "wAsyncly" wAsyncly
    -- XXX add two level nesting

    oneLevelNestedProduct "serially"   serially
    oneLevelNestedProduct "wSerially" wSerially
    oneLevelNestedProduct "asyncly" asyncly
    oneLevelNestedProduct "wAsyncly"  wAsyncly

    where
    tl = S.toList . t
    oneLevelNestedSum desc t1 =
        it ("One level nested sum " <> desc) $ do
            let nested = S.fromFoldable [1..10] <> throwM (ExampleException "E")
                         <> S.fromFoldable [1..10]
            try (tl (S.nil <> t1 nested <> S.fromFoldable [1..10]))
            `shouldReturn` (Left (ExampleException "E") :: Either ExampleException [Int])

    oneLevelNestedProduct desc t1 =
        it ("One level nested product" <> desc) $ do
            let s1 = t $ S.foldMapWith (<>) return [1..4]
                s2 = t1 $ S.foldMapWith (<>) return [5..8]
            try $ tl (do
                x <- adapt s1
                y <- s2
                if x + y > 10
                then throwM (ExampleException "E")
                else return (x + y)
                )
            `shouldReturn` (Left (ExampleException "E") :: Either ExampleException [Int])

_composeWithMonadError
    :: ( IsStream t
       , Semigroup (t (ExceptT String IO) Int)
       , MonadError String (t (ExceptT String IO))
       )
    => (t (ExceptT String IO) Int -> SerialT (ExceptT String IO) Int) -> Spec
_composeWithMonadError t = do
    let tl = S.toList . t
    it "Compose throwError, nil" $
        runExceptT (tl (throwError "E" <> S.nil)) `shouldReturn` Left "E"
    it "Compose nil, error" $
        runExceptT (tl (S.nil <> throwError "E")) `shouldReturn` Left "E"

nestTwoSerial :: Expectation
nestTwoSerial =
    let s1 = S.foldMapWith (<>) return [1..4]
        s2 = S.foldMapWith (<>) return [5..8]
    in toListSerial (do
        x <- s1
        y <- s2
        return (x + y)
        ) `shouldReturn` ([6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12] :: [Int])

nestTwoAhead :: Expectation
nestTwoAhead =
    let s1 = S.foldMapWith (<>) return [1..4]
        s2 = S.foldMapWith (<>) return [5..8]
    in (S.toList . aheadly) (do
        x <- s1
        y <- s2
        return (x + y)
        ) `shouldReturn` ([6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12] :: [Int])

nestTwoSerialApp :: Expectation
nestTwoSerialApp =
    let s1 = S.foldMapWith (<>) return [1..4]
        s2 = S.foldMapWith (<>) return [5..8]
    in toListSerial ((+) <$> s1 <*> s2)
        `shouldReturn` ([6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12] :: [Int])

nestTwoAheadApp :: Expectation
nestTwoAheadApp =
    let s1 = S.foldMapWith (<>) return [1..4]
        s2 = S.foldMapWith (<>) return [5..8]
    in (S.toList . aheadly) ((+) <$> s1 <*> s2)
        `shouldReturn` ([6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12] :: [Int])

nestTwoInterleaved :: Expectation
nestTwoInterleaved =
    let s1 = S.foldMapWith (<>) return [1..4]
        s2 = S.foldMapWith (<>) return [5..8]
    in toListInterleaved (do
        x <- s1
        y <- s2
        return (x + y)
        ) `shouldReturn` ([6,7,7,8,8,8,9,9,9,9,10,10,10,11,11,12] :: [Int])

nestTwoInterleavedApp :: Expectation
nestTwoInterleavedApp =
    let s1 = S.foldMapWith (<>) return [1..4]
        s2 = S.foldMapWith (<>) return [5..8]
    in toListInterleaved ((+) <$> s1 <*> s2)
        `shouldReturn` ([6,7,7,8,8,8,9,9,9,9,10,10,10,11,11,12] :: [Int])

nestTwoAsync :: Expectation
nestTwoAsync =
    let s1 = S.foldMapWith (<>) return [1..4]
        s2 = S.foldMapWith (<>) return [5..8]
    in sort <$> toListAsync (do
        x <- s1
        y <- s2
        return (x + y))
    `shouldReturn` sort ([6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12] :: [Int])

nestTwoAsyncApp :: Expectation
nestTwoAsyncApp =
    let s1 = S.foldMapWith (<>) return [1..4]
        s2 = S.foldMapWith (<>) return [5..8]
    in sort <$> toListAsync ((+) <$> s1 <*> s2)
        `shouldReturn` sort ([6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12] :: [Int])

nestTwoWAsync :: Expectation
nestTwoWAsync =
    let s1 = S.foldMapWith (<>) return [1..4]
        s2 = S.foldMapWith (<>) return [5..8]
    in sort <$> (S.toList . wAsyncly) (do
        x <- s1
        y <- s2
        return (x + y))
    `shouldReturn` sort ([6,7,7,8,8,8,9,9,9,9,10,10,10,11,11,12] :: [Int])

nestTwoParallel :: Expectation
nestTwoParallel =
    let s1 = S.foldMapWith (<>) return [1..4]
        s2 = S.foldMapWith (<>) return [5..8]
    in sort <$> (S.toList . parallely) (do
        x <- s1
        y <- s2
        return (x + y))
    `shouldReturn` sort ([6,7,7,8,8,8,9,9,9,9,10,10,10,11,11,12] :: [Int])

nestTwoWAsyncApp :: Expectation
nestTwoWAsyncApp =
    let s1 = S.foldMapWith (<>) return [1..4]
        s2 = S.foldMapWith (<>) return [5..8]
    in sort <$> (S.toList . wAsyncly) ((+) <$> s1 <*> s2)
        `shouldReturn` sort ([6,7,7,8,8,8,9,9,9,9,10,10,10,11,11,12] :: [Int])

nestTwoParallelApp :: Expectation
nestTwoParallelApp =
    let s1 = S.foldMapWith (<>) return [1..4]
        s2 = S.foldMapWith (<>) return [5..8]
    in sort <$> (S.toList . parallely) ((+) <$> s1 <*> s2)
        `shouldReturn` sort ([6,7,7,8,8,8,9,9,9,9,10,10,10,11,11,12] :: [Int])

interleaveCheck :: IsStream t
    => (t IO Int -> SerialT IO Int)
    -> (t IO Int -> t IO Int -> t IO Int)
    -> Spec
interleaveCheck t f =
    it "Interleave four" $
        (S.toList . t) ((singleton 0 `f` singleton 1) `f` (singleton 100 `f` singleton 101))
            `shouldReturn` [0, 100, 1, 101]

parallelCheck :: (IsStream t, Monad (t IO))
    => (t IO Int -> SerialT IO Int)
    -> (t IO Int -> t IO Int -> t IO Int)
    -> Spec
parallelCheck t f = do
    it "Parallel ordering left associated" $
        (S.toList . t) (((event 4 `f` event 3) `f` event 2) `f` event 1)
            `shouldReturn` [1..4]

    it "Parallel ordering right associated" $
        (S.toList . t) (event 4 `f` (event 3 `f` (event 2 `f` event 1)))
            `shouldReturn` [1..4]

    where event n = S.yieldM (threadDelay (n * 200000)) >> return n

compose :: (IsStream t, Semigroup (t IO Int))
    => (t IO Int -> SerialT IO Int) -> t IO Int -> ([Int] -> [Int]) -> Spec
compose t z srt = do
    -- XXX these should get covered by the property tests
    it "Compose mempty, mempty" $
        tl (z <> z) `shouldReturn` ([] :: [Int])
    it "Compose empty at the beginning" $
        tl (z <> singleton 1) `shouldReturn` [1]
    it "Compose empty at the end" $
        tl (singleton 1 <> z) `shouldReturn` [1]
    it "Compose two" $
        srt <$> tl (singleton 0 <> singleton 1)
            `shouldReturn` [0, 1]
    it "Compose many" $
        srt <$> tl (S.forEachWith (<>) [1..100] singleton)
            `shouldReturn` [1..100]

    -- These are not covered by the property tests
    it "Compose three - empty in the middle" $
        srt <$> tl (singleton 0 <> z <> singleton 1)
            `shouldReturn` [0, 1]
    it "Compose left associated" $
        srt <$> tl (((singleton 0 <> singleton 1) <> singleton 2) <> singleton 3)
            `shouldReturn` [0, 1, 2, 3]
    it "Compose right associated" $
        srt <$> tl (singleton 0 <> (singleton 1 <> (singleton 2 <> singleton 3)))
            `shouldReturn` [0, 1, 2, 3]
    it "Compose hierarchical (multiple levels)" $
        srt <$> tl (((singleton 0 <> singleton 1) <> (singleton 2 <> singleton 3))
                <> ((singleton 4 <> singleton 5) <> (singleton 6 <> singleton 7)))
            `shouldReturn` [0..7]
    where tl = S.toList . t

composeAndComposeSimple
    :: ( IsStream t1, Semigroup (t1 IO Int)
       , IsStream t2, Monoid (t2 IO Int), Monad (t2 IO)
#if __GLASGOW_HASKELL__ < 804
       , Semigroup (t2 IO Int)
#endif
       )
    => (t1 IO Int -> SerialT IO Int)
    -> (t2 IO Int -> t2 IO Int)
    -> [[Int]] -> Spec
composeAndComposeSimple t1 t2 answer = do
    let rfold = adapt . t2 . S.foldMapWith (<>) return
    it "Compose right associated outer expr, right folded inner" $
         (S.toList . t1) (rfold [1,2,3] <> (rfold [4,5,6] <> rfold [7,8,9]))
            `shouldReturn` head answer

    it "Compose left associated outer expr, right folded inner" $
         (S.toList . t1) ((rfold [1,2,3] <> rfold [4,5,6]) <> rfold [7,8,9])
            `shouldReturn` (answer !! 1)

    let lfold xs = adapt $ t2 $ foldl (<>) mempty $ fmap return xs
    it "Compose right associated outer expr, left folded inner" $
         (S.toList . t1) (lfold [1,2,3] <> (lfold [4,5,6] <> lfold [7,8,9]))
            `shouldReturn` (answer !! 2)

    it "Compose left associated outer expr, left folded inner" $
         (S.toList . t1) ((lfold [1,2,3] <> lfold [4,5,6]) <> lfold [7,8,9])
            `shouldReturn` (answer !! 3)

loops
    :: (IsStream t, Semigroup (t IO Int), Monad (t IO))
    => (t IO Int -> t IO Int)
    -> ([Int] -> [Int])
    -> ([Int] -> [Int])
    -> Spec
loops t tsrt hsrt = do
    it "Tail recursive loop" $ (tsrt <$> (S.toList . adapt) (loopTail 0))
            `shouldReturn` [0..3]

    it "Head recursive loop" $ (hsrt <$> (S.toList . adapt) (loopHead 0))
            `shouldReturn` [0..3]

    where
        loopHead x = do
            -- this print line is important for the test (causes a bind)
            S.yieldM $ putStrLn "LoopHead..."
            t $ (if x < 3 then loopHead (x + 1) else nil) <> return x

        loopTail x = do
            -- this print line is important for the test (causes a bind)
            S.yieldM $ putStrLn "LoopTail..."
            t $ return x <> (if x < 3 then loopTail (x + 1) else nil)

bindAndComposeSimple
    :: ( IsStream t1, IsStream t2, Semigroup (t2 IO Int), Monad (t2 IO))
    => (t1 IO Int -> SerialT IO Int)
    -> (t2 IO Int -> t2 IO Int)
    -> Spec
bindAndComposeSimple t1 t2 = do
    -- XXX need a bind in the body of forEachWith instead of a simple return
    it "Compose many (right fold) with bind" $
        (sort <$> (S.toList . t1)
                    (adapt . t2 $ S.forEachWith (<>) [1..10 :: Int] return))
            `shouldReturn` [1..10]

    it "Compose many (left fold) with bind" $
        let forL xs k = foldl (<>) nil $ fmap k xs
         in (sort <$> (S.toList . t1) (adapt . t2 $ forL [1..10 :: Int] return))
            `shouldReturn` [1..10]

bindAndComposeHierarchy
    :: ( IsStream t1, Monad (t1 IO)
       , IsStream t2, Monad (t2 IO))
    => (t1 IO Int -> SerialT IO Int)
    -> (t2 IO Int -> t2 IO Int)
    -> ([t2 IO Int] -> t2 IO Int)
    -> Spec
bindAndComposeHierarchy t1 t2 g =
    it "Bind and compose nested" $
        (sort <$> (S.toList . t1) bindComposeNested)
            `shouldReturn` (sort (
                   [12, 18]
                <> replicate 3 13
                <> replicate 3 17
                <> replicate 6 14
                <> replicate 6 16
                <> replicate 7 15) :: [Int])

    where

    -- bindComposeNested :: WAsyncT IO Int
    bindComposeNested =
        let c1 = tripleCompose (return 1) (return 2) (return 3)
            c2 = tripleCompose (return 4) (return 5) (return 6)
            c3 = tripleCompose (return 7) (return 8) (return 9)
            b = tripleBind c1 c2 c3
-- it seems to be causing a huge space leak in hspec so disabling this for now
--            c = tripleCompose b b b
--            m = tripleBind c c c
--         in m
         in b

    tripleCompose a b c = adapt . t2 $ g [a, b, c]
    tripleBind mx my mz =
        mx >>= \x -> my
           >>= \y -> mz
           >>= \z -> return (x + y + z)

mixedOps :: Spec
mixedOps =
    it "Compose many ops" $
        (sort <$> toListSerial composeMixed)
            `shouldReturn` ([8,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,11,11
                            ,11,11,11,11,11,11,11,11,12,12,12,12,12,13
                            ] :: [Int])
    where

    composeMixed :: SerialT IO Int
    composeMixed = do
        S.yieldM $ return ()
        S.yieldM $ putStr ""
        let x = 1
        let y = 2
        z <- do
                x1 <- wAsyncly $ return 1 <> return 2
                S.yieldM $ return ()
                S.yieldM $ putStr ""
                y1 <- asyncly $ return 1 <> return 2
                z1 <- do
                    x11 <- return 1 <> return 2
                    y11 <- asyncly $ return 1 <> return 2
                    z11 <- wSerially $ return 1 <> return 2
                    S.yieldM $ return ()
                    S.yieldM $ putStr ""
                    return (x11 + y11 + z11)
                return (x1 + y1 + z1)
        return (x + y + z)

mixedOpsAheadly :: Spec
mixedOpsAheadly =
    it "Compose many ops" $
        (sort <$> toListSerial composeMixed)
            `shouldReturn` ([8,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,11,11
                            ,11,11,11,11,11,11,11,11,12,12,12,12,12,13
                            ] :: [Int])
    where

    composeMixed :: SerialT IO Int
    composeMixed = do
        S.yieldM $ return ()
        S.yieldM $ putStr ""
        let x = 1
        let y = 2
        z <- do
                x1 <- wAsyncly $ return 1 <> return 2
                S.yieldM $ return ()
                S.yieldM $ putStr ""
                y1 <- aheadly $ return 1 <> return 2
                z1 <- do
                    x11 <- return 1 <> return 2
                    y11 <- aheadly $ return 1 <> return 2
                    z11 <- parallely $ return 1 <> return 2
                    S.yieldM $ return ()
                    S.yieldM $ putStr ""
                    return (x11 + y11 + z11)
                return (x1 + y1 + z1)
        return (x + y + z)
