{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Applicative (ZipList(..))
import Control.Concurrent (MVar, takeMVar, putMVar, newEmptyMVar)
import Control.Exception
       (BlockedIndefinitelyOnMVar(..), catches,
        BlockedIndefinitelyOnSTM(..), Handler(..))
import Control.Monad (when, forM_, replicateM, replicateM_)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))
import Data.IORef (readIORef, modifyIORef, newIORef, modifyIORef', IORef)
import Data.List
       (sort, foldl', scanl', findIndices, findIndex, elemIndices,
        elemIndex, find, insertBy, intersperse, foldl1', (\\),
        maximumBy, minimumBy, deleteBy, isPrefixOf, isSubsequenceOf,
        stripPrefix, intercalate)
import Data.Maybe (mapMaybe)
import GHC.Word (Word8)

import Test.Hspec.QuickCheck
import Test.QuickCheck
       (counterexample, Property, withMaxSuccess, forAll, choose, Gen,
       arbitrary, elements, frequency, listOf) --, listOf1, vectorOf, suchThat)
import Test.QuickCheck.Monadic (run, monadicIO, monitor, assert, PropertyM)

import Test.Hspec as H

import Streamly
import Streamly.Prelude ((.:), nil)
import Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL

-- Coverage build takes too long with default number of tests
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif

singleton :: IsStream t => a -> t m a
singleton a = a .: nil

sortEq :: Ord a => [a] -> [a] -> Bool
sortEq a b = sort a == sort b

equals
    :: (Show a, Monad m)
    => (a -> a -> Bool) -> a -> a -> PropertyM m ()
equals eq stream list = do
    when (not $ stream `eq` list) $
        monitor
            (counterexample $
             "stream " <> show stream
             <> "\nlist   " <> show list
            )
    assert (stream `eq` list)

listEquals
    :: (Show a, Eq a, MonadIO m)
    => ([a] -> [a] -> Bool) -> [a] -> [a] -> PropertyM m ()
listEquals eq stream list = do
    when (not $ stream `eq` list) $ liftIO $ putStrLn $
                  "stream " <> show stream
             <> "\nlist   " <> show list
             <> "\nstream \\\\ list " <> show (stream \\ list)
             <> "\nlist \\\\ stream " <> show (list \\ stream)
    when (not $ stream `eq` list) $
        monitor
            (counterexample $
                  "stream " <> show stream
             <> "\nlist   " <> show list
             <> "\nstream \\\\ list " <> show (stream \\ list)
             <> "\nlist \\\\ stream " <> show (list \\ stream)
             )
    assert (stream `eq` list)

-------------------------------------------------------------------------------
-- Construction operations
-------------------------------------------------------------------------------

constructWithLen
    :: (Show a, Eq a)
    => (Int -> t IO a)
    -> (Int -> [a])
    -> (t IO a -> SerialT IO a)
    -> Word8
    -> Property
constructWithLen mkStream mkList op len = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <- run $ (S.toList . op) (mkStream (fromIntegral len))
        let list = mkList (fromIntegral len)
        listEquals (==) stream list

constructWithLenM
    :: (Int -> t IO Int)
    -> (Int -> IO [Int])
    -> (t IO Int -> SerialT IO Int)
    -> Word8
    -> Property
constructWithLenM mkStream mkList op len = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <- run $ (S.toList . op) (mkStream (fromIntegral len))
        list <- run $ mkList (fromIntegral len)
        listEquals (==) stream list

constructWithReplicate, constructWithReplicateM, constructWithIntFromThenTo
    :: IsStream t
    => (t IO Int -> SerialT IO Int)
    -> Word8
    -> Property

constructWithReplicateM = constructWithLenM stream list
    where list = flip replicateM (return 1 :: IO Int)
          stream = flip S.replicateM (return 1 :: IO Int)

constructWithReplicate = constructWithLen stream list
    where list = flip replicate (1 :: Int)
          stream = flip S.replicate (1 :: Int)

constructWithIntFromThenTo op l =
    forAll (choose (minBound, maxBound)) $ \from ->
    forAll (choose (minBound, maxBound)) $ \next ->
    forAll (choose (minBound, maxBound)) $ \to ->
        let list len = take len [from,next..to]
            stream len = S.take len $ S.enumerateFromThenTo from next to
        in constructWithLen stream list op l

#if __GLASGOW_HASKELL__ >= 806
-- XXX try very small steps close to 0
constructWithDoubleFromThenTo
    :: IsStream t
    => (t IO Double -> SerialT IO Double)
    -> Word8
    -> Property
constructWithDoubleFromThenTo op l =
    forAll (choose (-9007199254740999,9007199254740999)) $ \from ->
    forAll (choose (-9007199254740999,9007199254740999)) $ \next ->
    forAll (choose (-9007199254740999,9007199254740999)) $ \to ->
        let list len = take len [from,next..to]
            stream len = S.take len $ S.enumerateFromThenTo from next to
        in constructWithLen stream list op l
#endif

constructWithIterate ::
       IsStream t => (t IO Int -> SerialT IO Int) -> Word8 -> Property
constructWithIterate op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <-
            run $
            (S.toList . op . S.take (fromIntegral len))
                (S.iterate (+ 1) (0 :: Int))
        let list = take (fromIntegral len) (iterate (+ 1) 0)
        listEquals (==) stream list

constructWithIterateM ::
       IsStream t => (t IO Int -> SerialT IO Int) -> Word8 -> Property
constructWithIterateM op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        mvl <- run (newIORef [] :: IO (IORef [Int]))
        let addM mv x y = modifyIORef' mv (++ [y + x]) >> return (y + x)
            list = take (fromIntegral len) (iterate (+ 1) 0)
        run $
            S.drain . op $
            S.take (fromIntegral len) $
            S.iterateM (addM mvl 1) (addM mvl 0 0 :: IO Int)
        streamEffect <- run $ readIORef mvl
        listEquals (==) streamEffect list

constructWithFromIndices ::
       IsStream t => (t IO Int -> SerialT IO Int) -> Word8 -> Property
constructWithFromIndices op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <-
            run $ (S.toList . op . S.take (fromIntegral len)) (S.fromIndices id)
        let list = take (fromIntegral len) (iterate (+ 1) 0)
        listEquals (==) stream list

constructWithFromIndicesM ::
       IsStream t => (t IO Int -> SerialT IO Int) -> Word8 -> Property
constructWithFromIndicesM op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        mvl <- run (newIORef [] :: IO (IORef [Int]))
        let addIndex mv i = modifyIORef' mv (++ [i]) >> return i
            list = take (fromIntegral len) (iterate (+ 1) 0)
        run $
            S.drain . op $
            S.take (fromIntegral len) $ S.fromIndicesM (addIndex mvl)
        streamEffect <- run $ readIORef mvl
        listEquals (==) streamEffect list

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
                i <- S.yieldM $ readIORef cnt
                S.yieldM $ modifyIORef cnt (+1)
                let msg = show i <> "/" <> show n
                S.yieldM $
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
            sourceUnfoldrM1 n |&. S.foldlM' (\xs x -> return (x : xs)) []
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

-------------------------------------------------------------------------------
-- Transformation operations
-------------------------------------------------------------------------------

transformCombineFromList
    :: Semigroup (t IO Int)
    => ([Int] -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> ([Int] -> [Int])
    -> (t IO Int -> SerialT IO Int)
    -> (t IO Int -> t IO Int)
    -> [Int]
    -> [Int]
    -> [Int]
    -> Property
transformCombineFromList constr eq listOp t op a b c =
    withMaxSuccess maxTestCount $
        monadicIO $ do
            stream <- run ((S.toList . t) $
                constr a <> op (constr b <> constr c))
            let list = a <> listOp (b <> c)
            listEquals eq stream list

-- XXX add tests for MonadReader and MonadError etc. In case an SVar is
-- accidentally passed through them.
--
-- This tests transform ops along with detecting illegal sharing of SVar across
-- conurrent streams. These tests work for all stream types whereas
-- transformCombineOpsOrdered work only for ordered stream types i.e. excluding
-- the Async type.
transformCombineOpsCommon
    :: (IsStream t, Semigroup (t IO Int))
    => ([Int] -> t IO Int)
    -> String
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> Spec
transformCombineOpsCommon constr desc eq t = do
    let transform = transformCombineFromList constr eq

    -- Filtering
    prop (desc <> " filter False") $
        transform (filter (const False)) t (S.filter (const False))
    prop (desc <> " filter True") $
        transform (filter (const True)) t (S.filter (const True))
    prop (desc <> " filter even") $
        transform (filter even) t (S.filter even)

    prop (desc <> " filterM False") $
        transform (filter (const False)) t (S.filterM (const $ return False))
    prop (desc <> " filterM True") $
        transform (filter (const True)) t (S.filterM (const $ return True))
    prop (desc <> " filterM even") $
        transform (filter even) t (S.filterM (return . even))

    prop (desc <> " take maxBound") $
        transform (take maxBound) t (S.take maxBound)
    prop (desc <> " take 0") $ transform (take 0) t (S.take 0)

    prop (desc <> " takeWhile True") $
        transform (takeWhile (const True)) t (S.takeWhile (const True))
    prop (desc <> " takeWhile False") $
        transform (takeWhile (const False)) t (S.takeWhile (const False))

    prop (desc <> " takeWhileM True") $
        transform (takeWhile (const True)) t (S.takeWhileM (const $ return True))
    prop (desc <> " takeWhileM False") $
        transform (takeWhile (const False)) t (S.takeWhileM (const $ return False))

    prop (desc <> " drop maxBound") $
        transform (drop maxBound) t (S.drop maxBound)
    prop (desc <> " drop 0") $ transform (drop 0) t (S.drop 0)

    prop (desc <> " dropWhile True") $
        transform (dropWhile (const True)) t (S.dropWhile (const True))
    prop (desc <> " dropWhile False") $
        transform (dropWhile (const False)) t (S.dropWhile (const False))

    prop (desc <> " dropWhileM True") $
        transform (dropWhile (const True)) t (S.dropWhileM (const $ return True))
    prop (desc <> " dropWhileM False") $
        transform (dropWhile (const False)) t (S.dropWhileM (const $ return False))

    prop (desc <> " deleteBy (<=) maxBound") $
        transform (deleteBy (<=) maxBound) t (S.deleteBy (<=) maxBound)
    prop (desc <> " deleteBy (==) 4") $
        transform (deleteBy (==) 4) t (S.deleteBy (==) 4)

    -- transformation
    prop (desc <> " mapM (+1)") $
        transform (fmap (+1)) t (S.mapM (\x -> return (x + 1)))

    prop (desc <> " scanl'") $ transform (scanl' (flip const) 0) t
                                       (S.scanl' (flip const) 0)
    prop (desc <> " scanlM'") $ transform (scanl' (flip const) 0) t
                                       (S.scanlM' (\_ a -> return a) 0)
    prop (desc <> " scanl") $ transform (scanl' (flip const) 0) t
                                       (S.scanl' (flip const) 0)
    prop (desc <> " scanl1'") $ transform (scanl1 (flip const)) t
                                         (S.scanl1' (flip const))
    prop (desc <> " scanl1M'") $ transform (scanl1 (flip const)) t
                                          (S.scanl1M' (\_ a -> return a))

    let f x = if odd x then Just (x + 100) else Nothing
    prop (desc <> " mapMaybe") $ transform (mapMaybe f) t (S.mapMaybe f)

    -- reordering
    prop (desc <> " reverse") $ transform reverse t S.reverse
    -- prop (desc <> " reverse'") $ transform reverse t S.reverse'

    -- inserting
    prop (desc <> " intersperseM") $
        forAll (choose (minBound, maxBound)) $ \n ->
            transform (intersperse n) t (S.intersperseM $ return n)
    prop (desc <> " insertBy 0") $
        forAll (choose (minBound, maxBound)) $ \n ->
            transform (insertBy compare n) t (S.insertBy compare n)

    -- multi-stream
    prop (desc <> " concatMap") $
        forAll (choose (0, 100)) $ \n ->
            transform (concatMap (const [1..n]))
                t (S.concatMap (const (S.fromList [1..n])))

toListFL :: Monad m => FL.Fold m a [a]
toListFL = FL.toList

groupSplitOps :: String -> Spec
groupSplitOps desc = do
    -- splitting
    -- XXX add tests with multichar separators too

{-
    prop (desc <> " intercalate . splitOnSeq == id (nil separator)") $
        forAll listWithZeroes $ \xs -> do
            withMaxSuccess maxTestCount $
                monadicIO $ do
                    ys <- S.toList $ FL.splitOnSeq [] toListFL (S.fromList xs)
                    listEquals (==) (intercalate [] ys) xs

    prop (desc <> " intercalate . splitOnSeq == id (single element separator)") $
        forAll listWithZeroes $ \xs -> do
            withMaxSuccess maxTestCount $
                monadicIO $ do
                    ys <- S.toList $ FL.splitOnSeq [0] toListFL (S.fromList xs)
                    listEquals (==) (intercalate [0] ys) xs

    prop (desc <> " concat . splitOnSeq . intercalate == concat (nil separator/possibly empty list)") $
        forAll listsWithoutZeroes $ \xss -> do
            withMaxSuccess maxTestCount $
                monadicIO $ do
                    let xs = intercalate [] xss
                    ys <- S.toList $ FL.splitOnSeq [0] toListFL (S.fromList xs)
                    listEquals (==) (concat ys) (concat xss)

    prop (desc <> " concat . splitOnSeq . intercalate == concat (non-nil separator/possibly empty list)") $
        forAll listsWithoutZeroes $ \xss -> do
            withMaxSuccess maxTestCount $
                monadicIO $ do
                    let xs = intercalate [0] xss
                    ys <- S.toList $ FL.splitOnSeq [0] toListFL (S.fromList xs)
                    listEquals (==) (concat ys) (concat xss)

    prop (desc <> " splitOnSeq . intercalate == id (exclusive separator/non-empty list)") $
        forAll listsWithoutZeroes1 $ \xss -> do
            withMaxSuccess maxTestCount $
                monadicIO $ do
                    let xs = intercalate [0] xss
                    ys <- S.toList $ FL.splitOnSeq [0] toListFL (S.fromList xs)
                    listEquals (==) ys xss
-}

    prop (desc <> " intercalate [x] . splitOn (== x) == id") $
        forAll listWithZeroes $ \xs -> do
            withMaxSuccess maxTestCount $
                monadicIO $ do
                    ys <- S.toList $ S.splitOn (== 0) toListFL (S.fromList xs)
                    listEquals (==) (intercalate [0] ys) xs

    where

    listWithZeroes :: Gen [Int]
    listWithZeroes = listOf $ frequency [(3, arbitrary), (1, elements [0])]

{-
    listWithoutZeroes = vectorOf 4 $ suchThat arbitrary (/= 0)

    listsWithoutZeroes :: Gen [[Int]]
    listsWithoutZeroes = listOf listWithoutZeroes

    listsWithoutZeroes1 :: Gen [[Int]]
    listsWithoutZeroes1 = listOf1 listWithoutZeroes
-}

-- transformation tests that can only work reliably for ordered streams i.e.
-- Serial, Ahead and Zip. For example if we use "take 1" on an async stream, it
-- might yield a different result every time.
transformCombineOpsOrdered
    :: (IsStream t, Semigroup (t IO Int))
    => ([Int] -> t IO Int)
    -> String
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> Spec
transformCombineOpsOrdered constr desc eq t = do
    let transform = transformCombineFromList constr eq

    -- Filtering
    prop (desc <> " take 1") $ transform (take 1) t (S.take 1)
#ifdef DEVBUILD
    prop (desc <> " take 2") $ transform (take 2) t (S.take 2)
    prop (desc <> " take 3") $ transform (take 3) t (S.take 3)
    prop (desc <> " take 4") $ transform (take 4) t (S.take 4)
    prop (desc <> " take 5") $ transform (take 5) t (S.take 5)
#endif
    prop (desc <> " take 10") $ transform (take 10) t (S.take 10)

    prop (desc <> " takeWhile > 0") $
        transform (takeWhile (> 0)) t (S.takeWhile (> 0))

    prop (desc <> " drop 1") $ transform (drop 1) t (S.drop 1)
    prop (desc <> " drop 10") $ transform (drop 10) t (S.drop 10)

    prop (desc <> " dropWhile > 0") $
        transform (dropWhile (> 0)) t (S.dropWhile (> 0))
    prop (desc <> " scan") $ transform (scanl' (+) 0) t (S.scanl' (+) 0)

    prop (desc <> " uniq") $ transform referenceUniq t S.uniq

    prop (desc <> " deleteBy (<=) 0") $
        transform (deleteBy (<=) 0) t (S.deleteBy (<=) 0)

    prop (desc <> " findIndices") $
        transform (findIndices odd) t (S.findIndices odd)
    prop (desc <> " elemIndices") $
        transform (elemIndices 0) t (S.elemIndices 0)

    -- XXX this does not fail when the SVar is shared, need to fix.
    prop (desc <> " concurrent application") $
        transform (& fmap (+1)) t (|& S.map (+1))

-------------------------------------------------------------------------------
-- Elimination operations
-------------------------------------------------------------------------------

eliminateOp
    :: (Show a, Eq a)
    => ([s] -> t IO s)
    -> ([s] -> a)
    -> (t IO s -> IO a)
    -> [s]
    -> Property
eliminateOp constr listOp op a =
    monadicIO $ do
        stream <- run $ op (constr a)
        let list = listOp a
        equals (==) stream list

wrapMaybe :: ([a1] -> a2) -> [a1] -> Maybe a2
wrapMaybe f x = if null x then Nothing else Just (f x)

wrapOutOfBounds :: ([a1] -> Int -> a2) -> Int -> [a1] -> Maybe a2
wrapOutOfBounds f i x | null x = Nothing
                      | i >= length x = Nothing
                      | otherwise = Just (f x i)

wrapThe :: Eq a => [a] -> Maybe a
wrapThe (x:xs)
    | all (x ==) xs = Just x
    | otherwise = Nothing
wrapThe [] = Nothing

-- This is the reference uniq implementation to compare uniq against,
-- we can use uniq from vector package, but for now this should
-- suffice.
referenceUniq :: Eq a => [a] -> [a]
referenceUniq = go
  where
    go [] = []
    go (x:[]) = [x]
    go (x:y:xs)
        | x == y = go (x : xs)
        | otherwise = x : go (y : xs)

eliminationOps
    :: ([Int] -> t IO Int)
    -> String
    -> (t IO Int -> SerialT IO Int)
    -> Spec
eliminationOps constr desc t = do
    -- Elimination
    prop (desc <> " null") $ eliminateOp constr null $ S.null . t
    prop (desc <> " foldl'") $
        eliminateOp constr (foldl' (+) 0) $ S.foldl' (+) 0 . t
    prop (desc <> " foldl1'") $
        eliminateOp constr (wrapMaybe $ foldl1' (+)) $ S.foldl1' (+) . t
#ifdef DEVBUILD
    prop (desc <> " foldr1") $
        eliminateOp constr (wrapMaybe $ foldr1 (+)) $ S.foldr1 (+) . t
#endif
    prop (desc <> " all") $ eliminateOp constr (all even) $ S.all even . t
    prop (desc <> " any") $ eliminateOp constr (any even) $ S.any even . t
    prop (desc <> " and") $ eliminateOp constr (and . fmap (> 0)) $
        (S.and . S.map (> 0)) . t
    prop (desc <> " or") $ eliminateOp constr (or . fmap (> 0)) $
        (S.or . S.map (> 0)) . t
    prop (desc <> " length") $ eliminateOp constr length $ S.length . t
    prop (desc <> " sum") $ eliminateOp constr sum $ S.sum . t
    prop (desc <> " product") $ eliminateOp constr product $ S.product . t

    prop (desc <> " maximum") $
        eliminateOp constr (wrapMaybe maximum) $ S.maximum . t
    prop (desc <> " minimum") $
        eliminateOp constr (wrapMaybe minimum) $ S.minimum . t

    prop (desc <> " maximumBy compare") $
        eliminateOp constr (wrapMaybe $ maximumBy compare) $
        S.maximumBy compare . t
    prop (desc <> " maximumBy flip compare") $
        eliminateOp constr (wrapMaybe $ maximumBy $ flip compare) $
        S.maximumBy (flip compare) . t
    prop (desc <> " minimumBy compare") $
        eliminateOp constr (wrapMaybe $ minimumBy compare) $
        S.minimumBy compare . t
    prop (desc <> " minimumBy flip compare") $
        eliminateOp constr (wrapMaybe $ minimumBy $ flip compare) $
        S.minimumBy (flip compare) . t

    prop (desc <> " findIndex") $
        eliminateOp constr (findIndex odd) $ S.findIndex odd . t
    prop (desc <> " elemIndex") $
        eliminateOp constr (elemIndex 3) $ S.elemIndex 3 . t

    prop (desc <> " !! 5") $
        eliminateOp constr (wrapOutOfBounds (!!) 5) $ (S.!! 5) . t
    prop (desc <> " !! 4") $
        eliminateOp constr (wrapOutOfBounds (!!) 0) $ (S.!! 0) . t

    prop (desc <> " find") $ eliminateOp constr (find even) $ S.find even . t
    prop (desc <> " lookup") $
        eliminateOp constr (lookup 3 . flip zip [1..]) $
            S.lookup 3 . S.zipWith (\a b -> (b, a)) (S.fromList [(1::Int)..]) . t
    prop (desc <> " the") $ eliminateOp constr wrapThe $ S.the . t

    -- Multi-stream eliminations
    -- Add eqBy, cmpBy
    -- XXX Write better tests for substreams.
    prop (desc <> " isPrefixOf 10") $ eliminateOp constr (isPrefixOf [1..10]) $
        S.isPrefixOf (S.fromList [(1::Int)..10]) . t
    prop (desc <> " isSubsequenceOf 10") $
        eliminateOp constr (isSubsequenceOf $ filter even [1..10]) $
        S.isSubsequenceOf (S.fromList $ filter even [(1::Int)..10]) . t
    prop (desc <> " stripPrefix 10") $ eliminateOp constr (stripPrefix [1..10]) $
        (\s -> s >>= maybe (return Nothing) (fmap Just . S.toList)) .
        S.stripPrefix (S.fromList [(1::Int)..10]) . t

-- head/tail/last may depend on the order in case of parallel streams
-- so we test these only for serial streams.
eliminationOpsOrdered
    :: ([Int] -> t IO Int)
    -> String
    -> (t IO Int -> SerialT IO Int)
    -> Spec
eliminationOpsOrdered constr desc t = do
    prop (desc <> " head") $ eliminateOp constr (wrapMaybe head) $ S.head . t
    prop (desc <> " tail") $ eliminateOp constr (wrapMaybe tail) $ \x -> do
        r <- S.tail (t x)
        case r of
            Nothing -> return Nothing
            Just s -> Just <$> S.toList s
    prop (desc <> " last") $ eliminateOp constr (wrapMaybe last) $ S.last . t
    prop (desc <> " init") $ eliminateOp constr (wrapMaybe init) $ \x -> do
        r <- S.init (t x)
        case r of
            Nothing -> return Nothing
            Just s -> Just <$> S.toList s

elemOp
    :: ([Word8] -> t IO Word8)
    -> (t IO Word8 -> SerialT IO Word8)
    -> (Word8 -> SerialT IO Word8 -> IO Bool)
    -> (Word8 -> [Word8] -> Bool)
    -> (Word8, [Word8])
    -> Property
elemOp constr op streamOp listOp (x, xs) =
    monadicIO $ do
        stream <- run $ (streamOp x . op) (constr xs)
        let list = listOp x xs
        equals (==) stream list

eliminationOpsWord8
    :: ([Word8] -> t IO Word8)
    -> String
    -> (t IO Word8 -> SerialT IO Word8)
    -> Spec
eliminationOpsWord8 constr desc t = do
    prop (desc <> " elem") $ elemOp constr t S.elem elem
    prop (desc <> " notElem") $ elemOp constr t S.notElem notElem

-------------------------------------------------------------------------------
-- Semigroup operations
-------------------------------------------------------------------------------

transformFromList
    :: (Eq b, Show b) =>
       ([a] -> t IO a)
    -> ([b] -> [b] -> Bool)
    -> ([a] -> [b])
    -> (t IO a -> SerialT IO b)
    -> [a]
    -> Property
transformFromList constr eq listOp op a =
    monadicIO $ do
        stream <- run ((S.toList . op) (constr a))
        let list = listOp a
        listEquals eq stream list

foldFromList
    :: ([Int] -> t IO Int)
    -> (t IO Int -> SerialT IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> [Int]
    -> Property
foldFromList constr op eq = transformFromList constr eq id op

-- XXX concatenate streams of multiple elements rather than single elements
semigroupOps
    :: (IsStream t

#if __GLASGOW_HASKELL__ < 804
       , Semigroup (t IO Int)
#endif
       , Monoid (t IO Int))
    => String
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> Spec
semigroupOps desc eq t = do
    prop (desc <> " <>") $ foldFromList (S.foldMapWith (<>) singleton) t eq
    prop (desc <> " mappend") $ foldFromList (S.foldMapWith mappend singleton) t eq

-------------------------------------------------------------------------------
-- Functor operations
-------------------------------------------------------------------------------

functorOps
    :: Functor (t IO)
    => ([Int] -> t IO Int)
    -> String
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> Spec
functorOps constr desc eq t = do
    prop (desc <> " id") $ transformFromList constr eq id t
    prop (desc <> " fmap (+1)") $ transformFromList constr eq (fmap (+1)) $ t . fmap (+1)

-------------------------------------------------------------------------------
-- Applicative operations
-------------------------------------------------------------------------------

applicativeOps
    :: Applicative (t IO)
    => ([Int] -> t IO Int)
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> (t IO (Int, Int) -> SerialT IO (Int, Int))
    -> ([Int], [Int])
    -> Property
applicativeOps constr eq t (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <- run ((S.toList . t) ((,) <$> constr a <*> constr b))
        let list = (,) <$> a <*> b
        listEquals eq stream list

-------------------------------------------------------------------------------
-- Zip operations
-------------------------------------------------------------------------------

zipApplicative
    :: (IsStream t, Applicative (t IO))
    => ([Int] -> t IO Int)
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> (t IO (Int, Int) -> SerialT IO (Int, Int))
    -> ([Int], [Int])
    -> Property
zipApplicative constr eq t (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream1 <- run ((S.toList . t) ((,) <$> constr a <*> constr b))
        stream2 <- run ((S.toList . t) (pure (,) <*> constr a <*> constr b))
        stream3 <- run ((S.toList . t) (S.zipWith (,) (constr a) (constr b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        listEquals eq stream1 list
        listEquals eq stream2 list
        listEquals eq stream3 list

zipMonadic
    :: IsStream t
    => ([Int] -> t IO Int)
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> (t IO (Int, Int) -> SerialT IO (Int, Int))
    -> ([Int], [Int])
    -> Property
zipMonadic constr eq t (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream1 <-
            run
                ((S.toList . t)
                     (S.zipWithM (curry return) (constr a) (constr b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        listEquals eq stream1 list

zipAsyncMonadic
    :: IsStream t
    => ([Int] -> t IO Int)
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> (t IO (Int, Int) -> SerialT IO (Int, Int))
    -> ([Int], [Int])
    -> Property
zipAsyncMonadic constr eq t (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream1 <-
            run
                ((S.toList . t)
                     (S.zipWithM (curry return) (constr a) (constr b)))
        stream2 <-
            run
                ((S.toList . t)
                     (S.zipAsyncWithM (curry return) (constr a) (constr b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        listEquals eq stream1 list
        listEquals eq stream2 list

-------------------------------------------------------------------------------
-- Monad operations
-------------------------------------------------------------------------------

monadThen
    :: Monad (t IO)
    => ([Int] -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> ([Int], [Int])
    -> Property
monadThen constr eq t (a, b) = withMaxSuccess maxTestCount $ monadicIO $ do
    stream <- run ((S.toList . t) (constr a >> constr b))
    let list = a >> b
    listEquals eq stream list

monadBind
    :: Monad (t IO)
    => ([Int] -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> ([Int], [Int])
    -> Property
monadBind constr eq t (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <-
            run
                ((S.toList . t)
                     (constr a >>= \x -> (+ x) <$> constr b))
        let list = a >>= \x -> (+ x) <$> b
        listEquals eq stream list

main :: IO ()
main = hspec
    $ H.parallel
#ifdef COVERAGE_BUILD
    $ modifyMaxSuccess (const 10)
#endif
    $ do
    let folded :: IsStream t => [a] -> t IO a
        folded = serially . (\xs ->
            case xs of
                [x] -> return x -- singleton stream case
                _ -> S.foldMapWith (<>) return xs
            )

    let makeCommonOps :: IsStream t => (t m a -> c) -> [(String, t m a -> c)]
        makeCommonOps t =
            [ ("default", t)
#ifndef COVERAGE_BUILD
            , ("rate AvgRate 10000", t . avgRate 10000)
            , ("rate Nothing", t . rate Nothing)
            , ("maxBuffer 0", t . maxBuffer 0)
            , ("maxThreads 0", t . maxThreads 0)
            , ("maxThreads 1", t . maxThreads 1)
            , ("maxThreads -1", t . maxThreads (-1))
#endif
            ]

    let makeOps :: IsStream t => (t m a -> c) -> [(String, t m a -> c)]
        makeOps t = makeCommonOps t ++
            [
#ifndef COVERAGE_BUILD
              ("maxBuffer 1", t . maxBuffer 1)
#endif
            ]

    -- For concurrent application test we need a buffer of at least size 2 to
    -- allow two threads to run.
    let makeConcurrentAppOps :: IsStream t
            => (t m a -> c) -> [(String, t m a -> c)]
        makeConcurrentAppOps t = makeCommonOps t ++
            [
#ifndef COVERAGE_BUILD
              ("maxBuffer 2", t . maxBuffer 2)
#endif
            ]

    let mapOps spec = mapM_ (\(desc, f) -> describe desc $ spec f)
    let serialOps :: IsStream t => ((SerialT IO a -> t IO a) -> Spec) -> Spec
        serialOps spec = mapOps spec $ makeOps serially
#ifndef COVERAGE_BUILD
            <> [("rate AvgRate 0.00000001", serially . avgRate 0.00000001)]
            <> [("maxBuffer -1", serially . maxBuffer (-1))]
#endif
    let wSerialOps :: IsStream t => ((WSerialT IO a -> t IO a) -> Spec) -> Spec
        wSerialOps spec = mapOps spec $ makeOps wSerially
#ifndef COVERAGE_BUILD
            <> [("rate AvgRate 0.00000001", wSerially . avgRate 0.00000001)]
            <> [("maxBuffer (-1)", wSerially . maxBuffer (-1))]
#endif
    let asyncOps :: IsStream t => ((AsyncT IO a -> t IO a) -> Spec) -> Spec
        asyncOps spec = mapOps spec $ makeOps asyncly
#ifndef COVERAGE_BUILD
            <> [("maxBuffer (-1)", asyncly . maxBuffer (-1))]
#endif
    let wAsyncOps :: IsStream t => ((WAsyncT IO a -> t IO a) -> Spec) -> Spec
        wAsyncOps spec = mapOps spec $ makeOps wAsyncly
#ifndef COVERAGE_BUILD
            <> [("maxBuffer (-1)", wAsyncly . maxBuffer (-1))]
#endif
    let aheadOps :: IsStream t => ((AheadT IO a -> t IO a) -> Spec) -> Spec
        aheadOps spec = mapOps spec $ makeOps aheadly
#ifndef COVERAGE_BUILD
              <> [("maxBuffer (-1)", aheadly . maxBuffer (-1))]
#endif
    let parallelCommonOps :: IsStream t => [(String, ParallelT m a -> t m a)]
        parallelCommonOps = []
#ifndef COVERAGE_BUILD
            <> [("rate AvgRate 0.00000001", parallely . avgRate 0.00000001)]
            <> [("maxBuffer (-1)", parallely . maxBuffer (-1))]
#endif
    let parallelOps :: IsStream t
            => ((ParallelT IO a -> t IO a) -> Spec) -> Spec
        parallelOps spec = mapOps spec $ makeOps parallely <> parallelCommonOps

    let parallelConcurrentAppOps :: IsStream t
            => ((ParallelT IO a -> t IO a) -> Spec) -> Spec
        parallelConcurrentAppOps spec =
            mapOps spec $ makeConcurrentAppOps parallely <> parallelCommonOps

    let zipSerialOps :: IsStream t
            => ((ZipSerialM IO a -> t IO a) -> Spec) -> Spec
        zipSerialOps spec = mapOps spec $ makeOps zipSerially
#ifndef COVERAGE_BUILD
            <> [("rate AvgRate 0.00000001", zipSerially . avgRate 0.00000001)]
            <> [("maxBuffer (-1)", zipSerially . maxBuffer (-1))]
#endif
    -- Note, the "pure" of applicative Zip streams generates and infinite
    -- stream and therefore maxBuffer (-1) must not be used for that case.
    let zipAsyncOps :: IsStream t => ((ZipAsyncM IO a -> t IO a) -> Spec) -> Spec
        zipAsyncOps spec = mapOps spec $ makeOps zipAsyncly

    describe "Construction" $ do
        serialOps   $ prop "serially replicate" . constructWithReplicate

        serialOps   $ prop "serially replicateM" . constructWithReplicateM
        wSerialOps  $ prop "wSerially replicateM" . constructWithReplicateM
        aheadOps    $ prop "aheadly replicateM" . constructWithReplicateM
        asyncOps    $ prop "asyncly replicateM" . constructWithReplicateM
        wAsyncOps   $ prop "wAsyncly replicateM" . constructWithReplicateM
        parallelOps $ prop "parallely replicateM" .  constructWithReplicateM

        serialOps   $ prop "serially intFromThenTo" .
                            constructWithIntFromThenTo
#if __GLASGOW_HASKELL__ >= 806
        serialOps   $ prop "serially DoubleFromThenTo" .
                            constructWithDoubleFromThenTo
#endif

        serialOps   $ prop "serially iterate" . constructWithIterate

        -- XXX test for all types of streams
        serialOps   $ prop "serially iterateM" . constructWithIterateM
        -- take doesn't work well on concurrent streams. Even though it
        -- seems like take only has a problem when used with parallely.
        -- wSerialOps $ prop "wSerially iterateM" wSerially . constructWithIterate
        -- aheadOps $ prop "aheadly iterateM" aheadly . onstructWithIterate
        -- asyncOps $ prop "asyncly iterateM" asyncly . constructWithIterate
        -- wAsyncOps $ prop "wAsyncly iterateM" wAsyncly . onstructWithIterate
        -- parallelOps $ prop "parallely iterateM" parallely . onstructWithIterate
        -- XXX add tests for fromIndices

        serialOps $ prop "serially fromIndices" . constructWithFromIndices

        serialOps $ prop "serially fromIndicesM" . constructWithFromIndicesM

    describe "Functor operations" $ do
        serialOps    $ functorOps S.fromFoldable "serially" (==)
        serialOps    $ functorOps folded "serially folded" (==)
        wSerialOps   $ functorOps S.fromFoldable "wSerially" (==)
        wSerialOps   $ functorOps folded "wSerially folded" (==)
        aheadOps     $ functorOps S.fromFoldable "aheadly" (==)
        aheadOps     $ functorOps folded "aheadly folded" (==)
        asyncOps     $ functorOps S.fromFoldable "asyncly" sortEq
        asyncOps     $ functorOps folded "asyncly folded" sortEq
        wAsyncOps    $ functorOps S.fromFoldable "wAsyncly" sortEq
        wAsyncOps    $ functorOps folded "wAsyncly folded" sortEq
        parallelOps  $ functorOps S.fromFoldable "parallely" sortEq
        parallelOps  $ functorOps folded "parallely folded" sortEq
        zipSerialOps $ functorOps S.fromFoldable "zipSerially" (==)
        zipSerialOps $ functorOps folded "zipSerially folded" (==)
        zipAsyncOps  $ functorOps S.fromFoldable "zipAsyncly" (==)
        zipAsyncOps  $ functorOps folded "zipAsyncly folded" (==)

    describe "Semigroup operations" $ do
        serialOps    $ semigroupOps "serially" (==)
        wSerialOps   $ semigroupOps "wSerially" (==)
        aheadOps     $ semigroupOps "aheadly" (==)
        asyncOps     $ semigroupOps "asyncly" sortEq
        wAsyncOps    $ semigroupOps "wAsyncly" sortEq
        parallelOps  $ semigroupOps "parallely" sortEq
        zipSerialOps $ semigroupOps "zipSerially" (==)
        zipAsyncOps  $ semigroupOps "zipAsyncly" (==)

    describe "Applicative operations" $ do
        -- The tests using sorted equality are weaker tests
        -- We need to have stronger unit tests for all those
        -- XXX applicative with three arguments
        serialOps   $ prop "serially applicative" . applicativeOps S.fromFoldable (==)
        serialOps   $ prop "serially applicative folded" . applicativeOps folded (==)
        wSerialOps  $ prop "wSerially applicative" . applicativeOps S.fromFoldable sortEq
        wSerialOps  $ prop "wSerially applicative folded" . applicativeOps folded sortEq
        aheadOps    $ prop "aheadly applicative" . applicativeOps S.fromFoldable (==)
        aheadOps    $ prop "aheadly applicative folded" . applicativeOps folded (==)
        asyncOps    $ prop "asyncly applicative" . applicativeOps S.fromFoldable sortEq
        asyncOps    $ prop "asyncly applicative folded" . applicativeOps folded sortEq
        wAsyncOps   $ prop "wAsyncly applicative" . applicativeOps S.fromFoldable sortEq
        wAsyncOps   $ prop "wAsyncly applicative folded" . applicativeOps folded sortEq
        parallelOps $ prop "parallely applicative folded" . applicativeOps folded sortEq

    -- XXX add tests for indexed/indexedR
    describe "Zip operations" $ do
        zipSerialOps $ prop "zipSerially applicative" . zipApplicative S.fromFoldable (==)
        zipSerialOps $ prop "zipSerially applicative folded" . zipApplicative folded (==)
        zipAsyncOps  $ prop "zipAsyncly applicative" . zipApplicative S.fromFoldable (==)
        zipAsyncOps  $ prop "zipAsyncly applicative folded" . zipApplicative folded (==)

        -- We test only the serial zip with serial streams and the parallel
        -- stream, because the rate setting in these streams can slow down
        -- zipAsync.
        serialOps   $ prop "zip monadic serially" . zipMonadic S.fromFoldable (==)
        serialOps   $ prop "zip monadic serially folded" . zipMonadic folded (==)
        wSerialOps  $ prop "zip monadic wSerially" . zipMonadic S.fromFoldable (==)
        wSerialOps  $ prop "zip monadic wSerially folded" . zipMonadic folded (==)
        aheadOps    $ prop "zip monadic aheadly" . zipAsyncMonadic S.fromFoldable (==)
        aheadOps    $ prop "zip monadic aheadly folded" . zipAsyncMonadic folded (==)
        asyncOps    $ prop "zip monadic asyncly" . zipAsyncMonadic S.fromFoldable (==)
        asyncOps    $ prop "zip monadic asyncly folded" . zipAsyncMonadic folded (==)
        wAsyncOps   $ prop "zip monadic wAsyncly" . zipAsyncMonadic S.fromFoldable (==)
        wAsyncOps   $ prop "zip monadic wAsyncly folded" . zipAsyncMonadic folded (==)
        parallelOps $ prop "zip monadic parallely" . zipMonadic S.fromFoldable (==)
        parallelOps $ prop "zip monadic parallely folded" . zipMonadic folded (==)

    -- XXX add merge tests like zip tests
    -- for mergeBy, we can split a list randomly into two lists and
    -- then merge them, it should result in original list
    -- describe "Merge operations" $ do

    describe "Monad operations" $ do
        serialOps   $ prop "serially monad then" . monadThen S.fromFoldable (==)
        wSerialOps  $ prop "wSerially monad then" . monadThen S.fromFoldable sortEq
        aheadOps    $ prop "aheadly monad then" . monadThen S.fromFoldable (==)
        asyncOps    $ prop "asyncly monad then" . monadThen S.fromFoldable sortEq
        wAsyncOps   $ prop "wAsyncly monad then" . monadThen S.fromFoldable sortEq
        parallelOps $ prop "parallely monad then" . monadThen S.fromFoldable sortEq

        serialOps   $ prop "serially monad then folded" . monadThen folded (==)
        wSerialOps  $ prop "wSerially monad then folded" . monadThen folded sortEq
        aheadOps    $ prop "aheadly monad then folded" . monadThen folded (==)
        asyncOps    $ prop "asyncly monad then folded" . monadThen folded sortEq
        wAsyncOps   $ prop "wAsyncly monad then folded" . monadThen folded sortEq
        parallelOps $ prop "parallely monad then folded" . monadThen folded sortEq

        serialOps   $ prop "serially monad bind" . monadBind S.fromFoldable (==)
        wSerialOps  $ prop "wSerially monad bind" . monadBind S.fromFoldable sortEq
        aheadOps    $ prop "aheadly monad bind" . monadBind S.fromFoldable (==)
        asyncOps    $ prop "asyncly monad bind" . monadBind S.fromFoldable sortEq
        wAsyncOps   $ prop "wAsyncly monad bind" . monadBind S.fromFoldable sortEq
        parallelOps $ prop "parallely monad bind" . monadBind S.fromFoldable sortEq

        serialOps   $ prop "serially monad bind folded"  . monadBind folded (==)
        wSerialOps  $ prop "wSerially monad bind folded" . monadBind folded sortEq
        aheadOps    $ prop "aheadly monad bind folded"   . monadBind folded (==)
        asyncOps    $ prop "asyncly monad bind folded"   . monadBind folded sortEq
        wAsyncOps   $ prop "wAsyncly monad bind folded"  . monadBind folded sortEq
        parallelOps $ prop "parallely monad bind folded" . monadBind folded sortEq

    -- These tests won't work with maxBuffer or maxThreads set to 1, so we
    -- exclude those cases from these.
    let mkOps :: IsStream t => (t m a -> c) -> [(String, t m a -> c)]
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
        asyncOps $ prop "async" . concurrentApplication sortEq
        aheadOps $ prop "ahead" . concurrentApplication (==)
        parallelConcurrentAppOps $
            prop "parallel" . concurrentApplication sortEq

        prop "concurrent foldr application" $ withMaxSuccess maxTestCount
            concurrentFoldrApplication
        prop "concurrent foldl application" $ withMaxSuccess maxTestCount
            concurrentFoldlApplication

    describe "Stream transform and combine operations" $ do
        serialOps    $ transformCombineOpsCommon S.fromFoldable "serially" (==)
        wSerialOps   $ transformCombineOpsCommon S.fromFoldable "wSerially" sortEq
        aheadOps     $ transformCombineOpsCommon S.fromFoldable "aheadly" (==)
        asyncOps     $ transformCombineOpsCommon S.fromFoldable "asyncly" sortEq
        wAsyncOps    $ transformCombineOpsCommon S.fromFoldable "wAsyncly" sortEq
        parallelOps  $ transformCombineOpsCommon S.fromFoldable "parallely" sortEq
        zipSerialOps $ transformCombineOpsCommon S.fromFoldable "zipSerially" (==)
        zipAsyncOps  $ transformCombineOpsCommon S.fromFoldable "zipAsyncly" (==)

        serialOps    $ transformCombineOpsCommon folded "serially" (==)
        wSerialOps   $ transformCombineOpsCommon folded "wSerially" sortEq
        aheadOps     $ transformCombineOpsCommon folded "aheadly" (==)
        asyncOps     $ transformCombineOpsCommon folded "asyncly" sortEq
        wAsyncOps    $ transformCombineOpsCommon folded "wAsyncly" sortEq
        parallelOps  $ transformCombineOpsCommon folded "parallely" sortEq
        zipSerialOps $ transformCombineOpsCommon folded "zipSerially" (==)
        zipAsyncOps  $ transformCombineOpsCommon folded "zipAsyncly" (==)

        serialOps    $ transformCombineOpsOrdered S.fromFoldable "serially" (==)
        aheadOps     $ transformCombineOpsOrdered S.fromFoldable "aheadly" (==)
        zipSerialOps $ transformCombineOpsOrdered S.fromFoldable "zipSerially" (==)
        zipAsyncOps  $ transformCombineOpsOrdered S.fromFoldable "zipAsyncly" (==)

        serialOps    $ transformCombineOpsOrdered folded "serially" (==)
        aheadOps     $ transformCombineOpsOrdered folded "aheadly" (==)
        zipSerialOps $ transformCombineOpsOrdered folded "zipSerially" (==)
        zipAsyncOps  $ transformCombineOpsOrdered folded "zipAsyncly" (==)

    describe "Stream group and split operations" $ do
        groupSplitOps "serially"

    describe "Stream elimination operations" $ do
        serialOps    $ eliminationOps S.fromFoldable "serially"
        wSerialOps   $ eliminationOps S.fromFoldable "wSerially"
        aheadOps     $ eliminationOps S.fromFoldable "aheadly"
        asyncOps     $ eliminationOps S.fromFoldable "asyncly"
        wAsyncOps    $ eliminationOps S.fromFoldable "wAsyncly"
        parallelOps  $ eliminationOps S.fromFoldable "parallely"
        zipSerialOps $ eliminationOps S.fromFoldable "zipSerially"
        zipAsyncOps  $ eliminationOps S.fromFoldable "zipAsyncly"

        serialOps    $ eliminationOps folded "serially folded"
        wSerialOps   $ eliminationOps folded "wSerially folded"
        aheadOps     $ eliminationOps folded "aheadly folded"
        asyncOps     $ eliminationOps folded "asyncly folded"
        wAsyncOps    $ eliminationOps folded "wAsyncly folded"
        parallelOps  $ eliminationOps folded "parallely folded"
        zipSerialOps $ eliminationOps folded "zipSerially folded"
        zipAsyncOps  $ eliminationOps folded "zipAsyncly folded"

        serialOps    $ eliminationOpsWord8 S.fromFoldable "serially"
        wSerialOps   $ eliminationOpsWord8 S.fromFoldable "wSerially"
        aheadOps     $ eliminationOpsWord8 S.fromFoldable "aheadly"
        asyncOps     $ eliminationOpsWord8 S.fromFoldable "asyncly"
        wAsyncOps    $ eliminationOpsWord8 S.fromFoldable "wAsyncly"
        parallelOps  $ eliminationOpsWord8 S.fromFoldable "parallely"
        zipSerialOps $ eliminationOpsWord8 S.fromFoldable "zipSerially"
        zipAsyncOps  $ eliminationOpsWord8 S.fromFoldable "zipAsyncly"

        serialOps    $ eliminationOpsWord8 folded "serially folded"
        wSerialOps   $ eliminationOpsWord8 folded "wSerially folded"
        aheadOps     $ eliminationOpsWord8 folded "aheadly folded"
        asyncOps     $ eliminationOpsWord8 folded "asyncly folded"
        wAsyncOps    $ eliminationOpsWord8 folded "wAsyncly folded"
        parallelOps  $ eliminationOpsWord8 folded "parallely folded"
        zipSerialOps $ eliminationOpsWord8 folded "zipSerially folded"
        zipAsyncOps  $ eliminationOpsWord8 folded "zipAsyncly folded"

    -- XXX Add a test where we chain all transformation APIs and make sure that
    -- the state is being passed through all of them.
    describe "Stream serial elimination operations" $ do
        serialOps    $ eliminationOpsOrdered S.fromFoldable "serially"
        wSerialOps   $ eliminationOpsOrdered S.fromFoldable "wSerially"
        aheadOps     $ eliminationOpsOrdered S.fromFoldable "aheadly"
        zipSerialOps $ eliminationOpsOrdered S.fromFoldable "zipSerially"
        zipAsyncOps  $ eliminationOpsOrdered S.fromFoldable "zipAsyncly"

        serialOps    $ eliminationOpsOrdered folded "serially folded"
        wSerialOps   $ eliminationOpsOrdered folded "wSerially folded"
        aheadOps     $ eliminationOpsOrdered folded "aheadly folded"
        zipSerialOps $ eliminationOpsOrdered folded "zipSerially folded"
        zipAsyncOps  $ eliminationOpsOrdered folded "zipAsyncly folded"
