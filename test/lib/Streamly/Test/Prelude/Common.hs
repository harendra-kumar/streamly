-- |
-- Module      : Streamly.Test.Prelude.Common
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude.Common
    (
    -- * Construction operations
      constructWithRepeat
    , constructWithRepeatM
    , constructWithReplicate
    , constructWithReplicateM
    , constructWithIntFromThenTo
#if __GLASGOW_HASKELL__ >= 806
    , constructWithDoubleFromThenTo
#endif
    , constructWithIterate
    , constructWithIterateM
    , constructWithEnumerate
    , constructWithEnumerateTo
    , constructWithFromIndices
    , constructWithFromIndicesM
    , constructWithFromList
    , constructWithFromListM
    , constructWithUnfoldr
    , constructWithCons
    , constructWithConsM
    , constructWithValue
    , constructWithValueM
    , simpleOps
    -- * Applicative operations
    , applicativeOps
    , applicativeOps1
    -- * Elimination operations
    , eliminationOpsOrdered
    , eliminationOpsWord8
    , eliminationOps
    -- * Functor operations
    , functorOps
    -- * Monoid operations
    , monoidOps
    , loops
    , bindAndComposeSimpleOps
    , bindAndComposeHierarchyOps
    , nestTwoStreams
    , nestTwoStreamsApp
    , composeAndComposeSimpleSerially
    , composeAndComposeSimpleAheadly
    , composeAndComposeSimpleWSerially
    -- * Semigroup operations
    , semigroupOps
    , parallelCheck
    -- * Transformation operations
    , transformCombineOpsOrdered
    , transformCombineOpsCommon
    , toListFL
    -- * Monad operations
    , monadBind
    , monadThen
    -- * Zip operations
    , zipApplicative
    , zipMonadic
    , zipAsyncApplicative
    , zipAsyncMonadic
    -- * Exception operations
    , exceptionOps
    -- * MonadThrow operations
    , composeWithMonadThrow
    -- * Cleanup tests
    , checkCleanup
    -- * Adhoc tests
    , takeCombined
    -- * Default values
    , maxTestCount
    , maxStreamLen
    -- * Helper operations
    , folded
    , makeCommonOps
    , makeOps
    , mapOps
    , sortEq
    ) where

import Control.Applicative (ZipList(..), liftA2)
import Control.Exception (Exception, try)
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
#ifdef DEVBUILD
import Control.Monad (when)
#endif
import Control.Monad.Catch (throwM, MonadThrow)
import Data.IORef ( IORef, atomicModifyIORef', modifyIORef', newIORef
                  , readIORef, writeIORef)
import Data.List
    ( delete
    , deleteBy
    , elemIndex
    , elemIndices
    , find
    , findIndex
    , findIndices
    , foldl'
    , foldl1'
    , insert
    , intersperse
    , isPrefixOf
    , isSubsequenceOf
    , maximumBy
    , minimumBy
    , scanl'
    , sort
    , stripPrefix
    , unfoldr
    )
import Data.Maybe (mapMaybe)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup, (<>))
#endif
import GHC.Word (Word8)
import System.Mem (performMajorGC)
import Test.Hspec.QuickCheck
import Test.Hspec
import Test.QuickCheck (Property, choose, forAll, withMaxSuccess)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Streamly.Prelude (SerialT, IsStream, (.:), nil, (|&), serially)
#ifndef COVERAGE_BUILD
import Streamly.Prelude (avgRate, rate, maxBuffer, maxThreads)
#endif
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF

import Streamly.Test.Common

maxStreamLen :: Int
maxStreamLen = 1000

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

constructWithRepeat, constructWithRepeatM
    :: IsStream t
    => (t IO Int -> SerialT IO Int)
    -> Word8
    -> Property
constructWithRepeat = constructWithLenM stream list
  where
    stream n = S.take n $ S.repeat 1
    list n = return $ replicate n 1

constructWithRepeatM = constructWithLenM stream list
  where
    stream n = S.take n $ S.repeatM (return 1)
    list n = return $ replicate n 1

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

constructWithCons ::
       IsStream t
    => (Int -> t IO Int -> t IO Int)
    -> (t IO Int -> SerialT IO Int)
    -> Word8
    -> Property
constructWithCons cons op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        strm <-
            run
               $ S.toList . op . S.take (fromIntegral len)
               $ foldr cons S.nil (repeat 0)
        let list = replicate (fromIntegral len) 0
        listEquals (==) strm list

constructWithConsM ::
       IsStream t
    => (IO Int -> t IO Int -> t IO Int)
    -> ([Int] -> [Int])
    -> (t IO Int -> SerialT IO Int)
    -> Word8
    -> Property
constructWithConsM consM listT op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        strm <-
            run $
            S.toList . op . S.take (fromIntegral len) $
            foldr consM S.nil (repeat (return 0))
        let list = replicate (fromIntegral len) 0
        listEquals (==) (listT strm) list

constructWithEnumerate ::
       IsStream t
    => ([Int] -> [Int])
    -> (t IO Int -> SerialT IO Int)
    -> Word8
    -> Property
constructWithEnumerate listT op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        strm <- run $ S.toList . op . S.take (fromIntegral len) $ S.enumerate
        let list = take (fromIntegral len) (enumFrom minBound)
        listEquals (==) (listT strm) list

constructWithEnumerateTo ::
       IsStream t
    => ([Int] -> [Int])
    -> (t IO Int -> SerialT IO Int)
    -> Word8
    -> Property
constructWithEnumerateTo listT op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        -- It takes forever to enumerate from minBound to len, so
        -- instead we just do till len elements
        strm <- run $ S.toList . op $ S.enumerateTo (minBound + fromIntegral len)
        let list = enumFromTo minBound (minBound + fromIntegral len)
        listEquals (==) (listT strm) list

constructWithFromList ::
       IsStream t
    => ([Int] -> [Int])
    -> (t IO Int -> SerialT IO Int)
    -> Word8
    -> Property
constructWithFromList listT op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        strm <- run $ S.toList . op . S.fromList $ [0 .. fromIntegral len]
        let list = [0 .. fromIntegral len]
        listEquals (==) (listT strm) list

constructWithFromListM ::
       IsStream t
    => ([Int] -> [Int])
    -> (t IO Int -> SerialT IO Int)
    -> Word8
    -> Property
constructWithFromListM listT op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        strm <-
            run $
            S.toList . op . S.fromListM . fmap pure $ [0 .. fromIntegral len]
        let list = [0 .. fromIntegral len]
        listEquals (==) (listT strm) list

constructWithUnfoldr ::
       IsStream t
    => ([Int] -> [Int])
    -> (t IO Int -> SerialT IO Int)
    -> Word8
    -> Property
constructWithUnfoldr listT op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        strm <- run $ S.toList . op $ S.unfoldr unfoldStep 0
        let list = unfoldr unfoldStep 0
        listEquals (==) (listT strm) list
  where
    unfoldStep seed =
        if seed > fromIntegral len
        then Nothing
        else Just (seed, seed + 1)

constructWithValue ::
       (IsStream t
#if __GLASGOW_HASKELL__ < 806
       , Monoid (t IO Int)
#endif
       )
    => ([Int] -> [Int])
    -> (t IO Int -> SerialT IO Int)
    -> Word8
    -> Property
constructWithValue listT op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        strm <-
            run
                $ S.toList . op . S.take (fromIntegral len)
                $ foldMap S.value (repeat 0)
        let list = replicate (fromIntegral len) 0
        listEquals (==) (listT strm) list

constructWithValueM ::
       (IsStream t
#if __GLASGOW_HASKELL__ < 806
       , Monoid (t IO Int)
#endif
       )
    => ([Int] -> [Int])
    -> (t IO Int -> SerialT IO Int)
    -> Word8
    -> Property
constructWithValueM listT op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        strm <-
            run
                $ S.toList . op . S.take (fromIntegral len)
                $ foldMap S.valueM (repeat (return 0))
        let list = replicate (fromIntegral len) 0
        listEquals (==) (listT strm) list

simpleProps ::
       (Int -> t IO Int)
    -> (t IO Int -> SerialT IO Int)
    -> Int
    -> Property
simpleProps constr op a = monadicIO $ do
  strm <- run $ S.toList . op . constr $ a
  listEquals (==) strm [a]

simpleOps :: IsStream t => (t IO Int -> SerialT IO Int) -> Spec
simpleOps op = do
  prop "value a = a" $ simpleProps S.value op
  prop "valueM a = a" $ simpleProps (S.valueM . return) op

-------------------------------------------------------------------------------
-- Applicative operations
-------------------------------------------------------------------------------

applicativeOps
    :: (Applicative (t IO), Semigroup (t IO Int))
    => ([Int] -> t IO Int)
    -> String
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> (t IO (Int, Int) -> SerialT IO (Int, Int))
    -> Spec
applicativeOps constr desc eq t = do
    prop (desc <> " <*>") $
        transformFromList2
            constr
            eq
            (\a b -> (,) <$> a <*> b)
            (\a b -> t ((,) <$> a <*> b))
    prop (desc <> " liftA2") $
        transformFromList2 constr eq (liftA2 (,)) (\a b -> t $ liftA2 (,) a b)
    prop (desc <> " Apply - composed first argument") $
        sort <$>
        (S.toList . t) ((,) <$> (pure 1 <> pure 2) <*> pure 3) `shouldReturn`
        [(1, 3), (2, 3)]
    prop (desc <> " Apply - composed second argument") $
        sort <$>
        (S.toList . t) (pure ((,) 1) <*> (pure 2 <> pure 3)) `shouldReturn`
        [(1, 2), (1, 3)]

-- XXX we can combine this with applicativeOps by making the type sufficiently
-- polymorphic.
applicativeOps1
    :: Applicative (t IO)
    => ([Int] -> t IO Int)
    -> String
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> Spec
applicativeOps1 constr desc eq t = do
    prop (desc <> " *>") $
        transformFromList2 constr eq (*>) (\a b -> t (a *> b))
    prop (desc <> " <*") $
        transformFromList2 constr eq (<*) (\a b -> t (a <* b))

transformFromList2
  :: (Eq c, Show c)
  => ([a] -> t IO a)
  -> ([c] -> [c] -> Bool)
  -> ([a] -> [a] -> [c])
  -> (t IO a -> t IO a -> SerialT IO c)
  -> ([a], [a])
  -> Property
transformFromList2 constr eq listOp op (a, b) =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <- run (S.toList $ op (constr a) (constr b))
        let list = listOp a b
        listEquals eq stream list

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
    prop (desc <> " mapM_ sumIORef") $
        eliminateOp constr sum $
        (\strm -> do
             ioRef <- newIORef 0
             let sumInRef a = modifyIORef' ioRef (a +)
             S.mapM_ sumInRef strm
             readIORef ioRef) .
        t
    prop (desc <> "trace sumIORef") $
        eliminateOp constr sum $
        (\strm -> do
             ioRef <- newIORef 0
             let sumInRef a = modifyIORef' ioRef (a +)
             S.drain $ S.trace sumInRef strm
             readIORef ioRef) .
        t

    prop (desc <> " maximum") $
        eliminateOp constr (wrapMaybe maximum) $ S.maximum . t
    prop (desc <> " minimum") $
        eliminateOp constr (wrapMaybe minimum) $ S.minimum . t

    prop (desc <> " maximumBy compare") $
        eliminateOp constr (wrapMaybe maximum) $
        S.maximumBy compare . t
    prop (desc <> " maximumBy flip compare") $
        eliminateOp constr (wrapMaybe $ maximumBy $ flip compare) $
        S.maximumBy (flip compare) . t
    prop (desc <> " minimumBy compare") $
        eliminateOp constr (wrapMaybe minimum) $
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
    prop (desc <> " findM") $ eliminateOp constr (find even) $ S.findM (return . even) . t
    prop (desc <> " lookup") $
        eliminateOp constr (lookup 3 . flip zip [1..]) $
            S.lookup 3 . S.zipWith (\a b -> (b, a)) (S.fromList [(1::Int)..]) . t
    prop (desc <> " the") $ eliminateOp constr wrapThe $ S.the . t

    -- Multi-stream eliminations
    -- XXX Write better tests for substreams.
    prop (desc <> " eqBy (==) t t") $
        eliminateOp constr (\s -> s == s) $ (\s -> S.eqBy (==) s s) . t
    prop (desc <> " cmpBy (==) t t") $
        eliminateOp constr (\s -> compare s s) $ (\s -> S.cmpBy compare s s) . t
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
-- Functor operations
-------------------------------------------------------------------------------

functorOps
    :: (Functor (t IO), Semigroup (t IO Int))
    => ([Int] -> t IO Int)
    -> String
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> Spec
functorOps constr desc eq t = do
    prop (desc <> " id") $ transformFromList constr eq id t
    prop (desc <> " fmap (+1)") $
        transformFromList constr eq (fmap (+ 1)) $ t . fmap (+ 1)
    prop (desc <> " fmap on composed (<>)") $
        sort <$>
        (S.toList . t) (fmap (+ 1) (constr [1] <> constr [2])) `shouldReturn`
        ([2, 3] :: [Int])

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


------------------------------------------------------------------------------
-- Monoid operations
------------------------------------------------------------------------------

monoidOps
    :: (IsStream t, Semigroup (t IO Int))
    => String
    -> t IO Int
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> Spec
monoidOps desc z eq t = do
    -- XXX these should get covered by the property tests
    prop (desc <> " Compose mempty, mempty") $ spec (z <> z) []
    prop (desc <> " Compose empty at the beginning") $ spec (z <> singleton 1) [1]
    prop (desc <> " Compose empty at the end") $ spec (singleton 1 <> z) [1]
    prop (desc <> " Compose two") $ spec (singleton 0 <> singleton 1) [0, 1]
    prop (desc <> " Compose many") $
        spec (S.concatForFoldableWith (<>) [1 .. 100] singleton) [1 .. 100]

    -- These are not covered by the property tests
    prop (desc <> " Compose three - empty in the middle") $
        spec (singleton 0 <> z <> singleton 1) [0, 1]
    prop (desc <> " Compose left associated") $
        spec
            (((singleton 0 <> singleton 1) <> singleton 2) <> singleton 3)
            [0, 1, 2, 3]
    prop (desc <> " Compose right associated") $
        spec
            (singleton 0 <> (singleton 1 <> (singleton 2 <> singleton 3)))
            [0, 1, 2, 3]
    prop (desc <> " Compose hierarchical (multiple levels)") $
        spec
            (((singleton 0 <> singleton 1) <> (singleton 2 <> singleton 3)) <>
             ((singleton 4 <> singleton 5) <> (singleton 6 <> singleton 7)))
            [0 .. 7]

    where

    tl = S.toList . t
    spec s list =
        monadicIO $ do
            stream <- run $ tl s
            listEquals eq stream list

---------------------------------------------------------------------------
-- Monoidal composition recursion loops
---------------------------------------------------------------------------

loops
    :: (IsStream t, Semigroup (t IO Int), Monad (t IO))
    => (t IO Int -> t IO Int)
    -> ([Int] -> [Int])
    -> ([Int] -> [Int])
    -> Spec
loops t tsrt hsrt = do
    it "Tail recursive loop" $ (tsrt <$> (S.toList . S.adapt) (loopTail 0))
            `shouldReturn` [0..3]

    it "Head recursive loop" $ (hsrt <$> (S.toList . S.adapt) (loopHead 0))
            `shouldReturn` [0..3]

    where
        loopHead x = do
            -- this print line is important for the test (causes a bind)
            S.valueM $ putStrLn "LoopHead..."
            t $ (if x < 3 then loopHead (x + 1) else nil) <> return x

        loopTail x = do
            -- this print line is important for the test (causes a bind)
            S.valueM $ putStrLn "LoopTail..."
            t $ return x <> (if x < 3 then loopTail (x + 1) else nil)

---------------------------------------------------------------------------
-- Bind and monoidal composition combinations
---------------------------------------------------------------------------

bindAndComposeSimpleOps
    :: IsStream t
    => String
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> Spec
bindAndComposeSimpleOps desc eq t = do
    bindAndComposeSimple
        ("Bind and compose " <> desc <> " Stream serially/")
        S.serially
    bindAndComposeSimple
        ("Bind and compose " <> desc <> " Stream wSerially/")
        S.wSerially
    bindAndComposeSimple
        ("Bind and compose " <> desc <> " Stream aheadly/")
        S.aheadly
    bindAndComposeSimple
        ("Bind and compose " <> desc <> " Stream asyncly/")
        S.asyncly
    bindAndComposeSimple
        ("Bind and compose " <> desc <> " Stream wAsyncly/")
        S.wAsyncly
    bindAndComposeSimple
        ("Bind and compose " <> desc <> " Stream parallely/")
        S.parallely

    where

    bindAndComposeSimple
        :: (IsStream t2, Semigroup (t2 IO Int), Monad (t2 IO))
        => String
        -> (t2 IO Int -> t2 IO Int)
        -> Spec
    bindAndComposeSimple idesc t2 = do
      -- XXX need a bind in the body of forEachWith instead of a simple return
      prop (idesc <> " Compose many (right fold) with bind") $ \list ->
          monadicIO $ do
              stream <-
                  run $
                  (S.toList . t)
                      (S.adapt . t2 $ S.concatForFoldableWith (<>) list return)
              listEquals eq stream list

      prop (idesc <> " Compose many (left fold) with bind") $ \list ->
          monadicIO $ do
              let forL xs k = foldl (<>) nil $ fmap k xs
              stream <-
                  run $ (S.toList . t) (S.adapt . t2 $ forL list return)
              listEquals eq stream list

---------------------------------------------------------------------------
-- Bind and monoidal composition combinations
---------------------------------------------------------------------------

bindAndComposeHierarchyOps ::
       (IsStream t, Monad (t IO))
    => String
    -> (t IO Int -> SerialT IO Int)
    -> Spec
bindAndComposeHierarchyOps desc t1 = do
    let fldldesc = "Bind and compose foldl, " <> desc <> " Stream "
        fldrdesc = "Bind and compose foldr, " <> desc <> " Stream "

    bindAndComposeHierarchy
        (fldldesc <> "serially") S.serially fldl
    bindAndComposeHierarchy
        (fldrdesc <> "serially") S.serially fldr
    bindAndComposeHierarchy
        (fldldesc <> "wSerially") S.wSerially fldl
    bindAndComposeHierarchy
        (fldrdesc <> "wSerially") S.wSerially fldr
    bindAndComposeHierarchy
        (fldldesc <> "aheadly") S.aheadly fldl
    bindAndComposeHierarchy
        (fldrdesc <> "aheadly") S.aheadly fldr
    bindAndComposeHierarchy
        (fldldesc <> "asyncly") S.asyncly fldl
    bindAndComposeHierarchy
        (fldrdesc <> "asyncly") S.asyncly fldr
    bindAndComposeHierarchy
        (fldldesc <> "wAsyncly") S.wAsyncly fldl
    bindAndComposeHierarchy
        (fldrdesc <> "wAsyncly") S.wAsyncly fldr
    bindAndComposeHierarchy
        (fldldesc <> "parallely") S.parallely fldl
    bindAndComposeHierarchy
        (fldrdesc <> "parallely")  S.parallely fldr

  where

    bindAndComposeHierarchy
        :: (IsStream t2, Monad (t2 IO))
        => String
        -> (t2 IO Int -> t2 IO Int)
        -> ([t2 IO Int] -> t2 IO Int)
        -> Spec
    bindAndComposeHierarchy specdesc t2 g =
        describe specdesc $
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

        tripleCompose a b c = S.adapt . t2 $ g [a, b, c]
        tripleBind mx my mz =
            mx >>= \x -> my
            >>= \y -> mz
            >>= \z -> return (x + y + z)

    fldr, fldl :: (IsStream t, Semigroup (t IO Int))
                => [t IO Int] -> t IO Int
    fldr = foldr (<>) nil
    fldl = foldl (<>) nil

-- Nest two lists using different styles of product compositions
nestTwoStreams
    :: (IsStream t, Semigroup (t IO Int), Monad (t IO))
    => String
    -> ([Int] -> [Int])
    -> ([Int] -> [Int])
    -> (t IO Int -> SerialT IO Int)
    -> Spec
nestTwoStreams desc streamListT listT t =
    it ("Nests two streams using monadic " <> desc <> " composition") $ do
    let s1 = S.concatMapFoldableWith (<>) return [1..4]
        s2 = S.concatMapFoldableWith (<>) return [5..8]
    r <- (S.toList . t) $ do
                x <- s1
                y <- s2
                return $ x + y
    streamListT r `shouldBe` listT [6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12]

nestTwoStreamsApp
    :: (IsStream t, Semigroup (t IO Int), Monad (t IO))
    => String
    -> ([Int] -> [Int])
    -> ([Int] -> [Int])
    -> (t IO Int -> SerialT IO Int)
    -> Spec
nestTwoStreamsApp desc streamListT listT t =
    it ("Nests two streams using applicative " <> desc <> " composition") $ do
    let s1 = S.concatMapFoldableWith (<>) return [1..4]
        s2 = S.concatMapFoldableWith (<>) return [5..8]
        r  = (S.toList . t) ((+) <$> s1 <*> s2)
    streamListT <$> r
        `shouldReturn` listT [6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12]


-- TBD need more such combinations to be tested.
composeAndComposeSimple
    :: ( IsStream t1, Semigroup (t1 IO Int)
       , IsStream t2, Monoid (t2 IO Int), Monad (t2 IO)
#if !(MIN_VERSION_base(4,11,0))
       , Semigroup (t2 IO Int)
#endif
       )
    => (t1 IO Int -> SerialT IO Int)
    -> (t2 IO Int -> t2 IO Int)
    -> [[Int]] -> Spec
composeAndComposeSimple t1 t2 answer = do
    let rfold = S.adapt . t2 . S.concatMapFoldableWith (<>) return
    it "Compose right associated outer expr, right folded inner" $
         (S.toList . t1) (rfold [1,2,3] <> (rfold [4,5,6] <> rfold [7,8,9]))
            `shouldReturn` head answer

    it "Compose left associated outer expr, right folded inner" $
         (S.toList . t1) ((rfold [1,2,3] <> rfold [4,5,6]) <> rfold [7,8,9])
            `shouldReturn` (answer !! 1)

    let lfold xs = S.adapt $ t2 $ foldl (<>) mempty $ fmap return xs
    it "Compose right associated outer expr, left folded inner" $
         (S.toList . t1) (lfold [1,2,3] <> (lfold [4,5,6] <> lfold [7,8,9]))
            `shouldReturn` (answer !! 2)

    it "Compose left associated outer expr, left folded inner" $
         (S.toList . t1) ((lfold [1,2,3] <> lfold [4,5,6]) <> lfold [7,8,9])
            `shouldReturn` (answer !! 3)

composeAndComposeSimpleSerially
    :: (IsStream t, Semigroup (t IO Int))
    => String
    -> [[Int]]
    -> (t IO Int -> SerialT IO Int)
    -> Spec
composeAndComposeSimpleSerially desc answer t = do
    describe (desc <> " and Serial <>") $ composeAndComposeSimple t S.serially answer

composeAndComposeSimpleAheadly
    :: (IsStream t, Semigroup (t IO Int))
    => String
    -> [[Int]]
    -> (t IO Int -> SerialT IO Int)
    -> Spec
composeAndComposeSimpleAheadly desc answer t = do
    describe (desc <> " and Ahead <>") $ composeAndComposeSimple t S.aheadly answer

composeAndComposeSimpleWSerially
    :: (IsStream t, Semigroup (t IO Int))
    => String
    -> [[Int]]
    -> (t IO Int -> SerialT IO Int)
    -> Spec
composeAndComposeSimpleWSerially desc answer t = do
    describe (desc <> " and WSerial <>") $ composeAndComposeSimple t S.wSerially answer

-------------------------------------------------------------------------------
-- Semigroup operations
-------------------------------------------------------------------------------

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
    prop (desc <> " <>") $ foldFromList (S.concatMapFoldableWith (<>) singleton) t eq
    prop (desc <> " mappend") $ foldFromList (S.concatMapFoldableWith mappend singleton) t eq

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
    :: (IsStream t, Semigroup (t IO Int) , Functor (t IO))
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
        transform (delete 4) t (S.deleteBy (==) 4)

    -- transformation
    prop (desc <> " mapM (+1)") $
        transform (fmap (+1)) t (S.mapM (\x -> return (x + 1)))

    prop (desc <> " scanl'") $ transform (scanl' (const id) 0) t
                                       (S.scanl' (const id) 0)
    prop (desc <> " postscanl'") $ transform (tail . scanl' (const id) 0) t
                                       (S.postscanl' (const id) 0)
    prop (desc <> " scanlM'") $ transform (scanl' (const id) 0) t
                                       (S.scanlM' (\_ a -> return a) (return 0))
    prop (desc <> " postscanlM'") $ transform (tail . scanl' (const id) 0) t
                                       (S.postscanlM' (\_ a -> return a) (return 0))
    prop (desc <> " scanl1'") $ transform (scanl1 (const id)) t
                                         (S.scanl1' (const id))
    prop (desc <> " scanl1M'") $ transform (scanl1 (const id)) t
                                          (S.scanl1M' (\_ a -> return a))

    let f x = if odd x then Just (x + 100) else Nothing
    prop (desc <> " mapMaybe") $ transform (mapMaybe f) t (S.mapMaybe f)
    prop (desc <> " mapMaybeM") $
        transform (mapMaybe f) t (S.mapMaybeM (return . f))

    -- tap
    prop (desc <> " tap FL.sum . map (+1)") $ \a b ->
        withMaxSuccess maxTestCount $
        monadicIO $ do
            cref <- run $ newIORef 0
            let fldstp _ e = modifyIORef' cref (e +)
                sumfoldinref = FL.mkAccumM_ fldstp (return ())
                op = S.tap sumfoldinref . S.mapM (\x -> return (x+1))
                listOp = fmap (+1)
            stream <- run ((S.toList . t) $ op (constr a <> constr b))
            let list = listOp (a <> b)
            ssum <- run $ readIORef cref
            assert (sum list == ssum)
            listEquals eq stream list

    -- reordering
    prop (desc <> " reverse") $ transform reverse t S.reverse
    prop (desc <> " reverse'") $ transform reverse t S.reverse'

    -- inserting
    prop (desc <> " intersperseM") $
        forAll (choose (minBound, maxBound)) $ \n ->
            transform (intersperse n) t (S.intersperseM $ return n)
    prop (desc <> " intersperse") $
        forAll (choose (minBound, maxBound)) $ \n ->
            transform (intersperse n) t (S.intersperse n)
    prop (desc <> " insertBy 0") $
        forAll (choose (minBound, maxBound)) $ \n ->
            transform (insert n) t (S.insertBy compare n)

    -- multi-stream
    prop (desc <> " concatMap") $
        forAll (choose (0, 100)) $ \n ->
            transform (concatMap (const [1..n]))
                t (S.concatMap (const (S.fromList [1..n])))
    prop (desc <> " concatMapM") $
        forAll (choose (0, 100)) $ \n ->
            transform (concatMap (const [1..n]))
                t (S.concatMapM (const (return $ S.fromList [1..n])))
    prop (desc <> " concatUnfold") $
        forAll (choose (0, 100)) $ \n ->
            transform (concatMap (const [1..n]))
                t (S.concatUnfold (UF.lmap (const undefined)
                                   $ UF.supply UF.fromList [1..n]))

toListFL :: Monad m => FL.Fold m a [a]
toListFL = FL.toList

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
    prop (desc <> " takeWhileM > 0") $
        transform (takeWhile (> 0)) t (S.takeWhileM (return . (> 0)))

    prop (desc <> " drop 1") $ transform (drop 1) t (S.drop 1)
    prop (desc <> " drop 10") $ transform (drop 10) t (S.drop 10)

    prop (desc <> " dropWhile > 0") $
        transform (dropWhile (> 0)) t (S.dropWhile (> 0))
    prop (desc <> " dropWhileM > 0") $
        transform (dropWhile (> 0)) t (S.dropWhileM (return . (> 0)))
    prop (desc <> " scan") $ transform (scanl' (+) 0) t (S.scanl' (+) 0)

    prop (desc <> " uniq") $ transform referenceUniq t S.uniq

    prop (desc <> " deleteBy (<=) 0") $
        transform (deleteBy (<=) 0) t (S.deleteBy (<=) 0)

    prop (desc <> " findIndices") $
        transform (findIndices odd) t (S.findIndices odd)
    prop (desc <> " findIndices . filter") $
        transform (findIndices odd . filter odd)
                  t
                  (S.findIndices odd . S.filter odd)
    prop (desc <> " elemIndices") $
        transform (elemIndices 0) t (S.elemIndices 0)

    -- XXX this does not fail when the SVar is shared, need to fix.
    prop (desc <> " concurrent application") $
        transform (fmap (+1)) t (|& S.map (+1))

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

zipAsyncApplicative
    :: IsStream t
    => ([Int] -> t IO Int)
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> (t IO (Int, Int) -> SerialT IO (Int, Int))
    -> ([Int], [Int])
    -> Property
zipAsyncApplicative constr eq t (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <-
            run
                ((S.toList . t)
                     (S.zipAsyncWith (,) (constr a) (constr b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        listEquals eq stream list

---------------------------------------------------------------------------
-- Semigroup/Monoidal Composition strict ordering checks
---------------------------------------------------------------------------

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

    where event n = S.valueM (threadDelay (n * 200000)) >> return n

-------------------------------------------------------------------------------
-- Exception ops
-------------------------------------------------------------------------------

beforeProp :: IsStream t => (t IO Int -> SerialT IO Int) -> [Int] -> Property
beforeProp t vec =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        ioRef <- run $ newIORef []
        run
            $ S.drain . t
            $ S.before (writeIORef ioRef [0])
            $ S.mapM (\a -> do atomicModifyIORef' ioRef (\xs -> (xs ++ [a], ()))
                               return a)
            $ S.fromList vec
        refValue <- run $ readIORef ioRef
        listEquals (==) (head refValue : sort (tail refValue)) (0:sort vec)

afterProp :: IsStream t => (t IO Int -> SerialT IO Int) -> [Int] -> Property
afterProp t vec =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        ioRef <- run $ newIORef []
        run
            $ S.drain . t
            $ S.after (modifyIORef' ioRef (0:))
            $ S.mapM (\a -> do atomicModifyIORef' ioRef (\xs -> (a:xs, ()))
                               return a)
            $ S.fromList vec
        refValue <- run $ readIORef ioRef
        listEquals (==) (head refValue : sort (tail refValue)) (0:sort vec)

bracketProp :: IsStream t => (t IO Int -> SerialT IO Int) -> [Int] -> Property
bracketProp t vec =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        ioRef <- run $ newIORef (0 :: Int)
        run $
            S.drain . t $
            S.bracket
                (return ioRef)
                (`writeIORef` 1)
                (\ioref ->
                     S.mapM
                         (\a -> writeIORef ioref 2 >> return a)
                         (S.fromList vec))
        refValue <- run $ readIORef ioRef
        assert $ refValue == 1

#ifdef DEVBUILD
bracketPartialStreamProp ::
       (IsStream t) => (t IO Int -> SerialT IO Int) -> [Int] -> Property
bracketPartialStreamProp t vec =
    forAll (choose (0, length vec)) $ \len -> do
        withMaxSuccess maxTestCount $
            monadicIO $ do
                ioRef <- run $ newIORef (0 :: Int)
                run $
                    S.drain . t $
                    S.take len $
                    S.bracket
                        (writeIORef ioRef 1 >> return ioRef)
                        (`writeIORef` 3)
                        (\ioref ->
                             S.mapM
                                 (\a -> writeIORef ioref 2 >> return a)
                                 (S.fromList vec))
                run $ do
                    performMajorGC
                    threadDelay 10000
                refValue <- run $ readIORef ioRef
                when (refValue /= 0 && refValue /= 3) $
                    error $ "refValue == " ++ show refValue
#endif

bracketExceptionProp ::
       (IsStream t, MonadThrow (t IO)
#if __GLASGOW_HASKELL__ < 806
       , Semigroup (t IO Int)
#endif
       )
    => (t IO Int -> SerialT IO Int)
    -> Property
bracketExceptionProp t =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        ioRef <- run $ newIORef (0 :: Int)
        res <-
            run $
            try . S.drain . t $
            S.bracket
                (return ioRef)
                (`writeIORef` 1)
                (const $ throwM (ExampleException "E") <> S.nil)
        assert $ res == Left (ExampleException "E")
        refValue <- run $ readIORef ioRef
        assert $ refValue == 1

finallyProp :: (IsStream t) => (t IO Int -> SerialT IO Int) -> [Int] -> Property
finallyProp t vec =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        ioRef <- run $ newIORef (0 :: Int)
        run $
            S.drain . t $
            S.finally
                (writeIORef ioRef 1)
                (S.mapM (\a -> writeIORef ioRef 2 >> return a) (S.fromList vec))
        refValue <- run $ readIORef ioRef
        assert $ refValue == 1

#ifdef DEVBUILD
finallyPartialStreamProp ::
       (IsStream t) => (t IO Int -> SerialT IO Int) -> [Int] -> Property
finallyPartialStreamProp t vec =
    forAll (choose (0, length vec)) $ \len -> do
        withMaxSuccess maxTestCount $
            monadicIO $ do
                ioRef <- run $ newIORef (0 :: Int)
                run $
                    S.drain . t $
                    S.take len $
                    S.finally
                        (writeIORef ioRef 2)
                        (S.mapM
                             (\a -> writeIORef ioRef 1 >> return a)
                             (S.fromList vec))
                run $ do
                    performMajorGC
                    threadDelay 10000
                refValue <- run $ readIORef ioRef
                when (refValue /= 0 && refValue /= 2) $
                    error $ "refValue == " ++ show refValue
#endif

finallyExceptionProp ::
       (IsStream t, MonadThrow (t IO)
#if __GLASGOW_HASKELL__ < 806
       , Semigroup (t IO Int)
#endif
       )
    => (t IO Int -> SerialT IO Int)
    -> Property
finallyExceptionProp t =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        ioRef <- run $ newIORef (0 :: Int)
        res <-
            run $
            try . S.drain . t $
            S.finally
                (writeIORef ioRef 1)
                (throwM (ExampleException "E") <> S.nil)
        assert $ res == Left (ExampleException "E")
        refValue <- run $ readIORef ioRef
        assert $ refValue == 1

onExceptionProp ::
       (IsStream t, MonadThrow (t IO)
#if __GLASGOW_HASKELL__ < 806
       , Semigroup (t IO Int)
#endif
       )
    => (t IO Int -> SerialT IO Int)
    -> Property
onExceptionProp t =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        ioRef <- run $ newIORef (0 :: Int)
        res <-
            run $
            try . S.drain . t $
            S.onException
                (writeIORef ioRef 1)
                (throwM (ExampleException "E") <> S.nil)
        assert $ res == Left (ExampleException "E")
        refValue <- run $ readIORef ioRef
        assert $ refValue == 1

handleProp ::
       IsStream t
    => (t IO Int -> SerialT IO Int)
    -> [Int]
    -> Property
handleProp t vec =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        res <-
            run $
            S.toList . t $
            S.handle
                (\(ExampleException i) -> read i `S.cons` S.fromList vec)
                (S.serially $ S.fromList vec <> throwM (ExampleException "0"))
        assert $ res == vec ++ [0] ++ vec

exceptionOps ::
       (IsStream t, MonadThrow (t IO)
#if __GLASGOW_HASKELL__ < 806
       , Semigroup (t IO Int)
#endif
       )
    => String
    -> (t IO Int -> SerialT IO Int)
    -> Spec
exceptionOps desc t = do
    prop (desc <> " before") $ beforeProp t
    prop (desc <> " after") $ afterProp t
    prop (desc <> " bracket end of stream") $ bracketProp t
#ifdef DEVBUILD
    prop (desc <> " bracket partial stream") $ bracketPartialStreamProp t
#endif
    prop (desc <> " bracket exception in stream") $ bracketExceptionProp t
    prop (desc <> " onException") $ onExceptionProp t
    prop (desc <> " finally end of stream") $ finallyProp t
#ifdef DEVBUILD
    prop (desc <> " finally partial stream") $ finallyPartialStreamProp t
#endif
    prop (desc <> " finally exception in stream") $ finallyExceptionProp t
    prop (desc <> " handle") $ handleProp t

-------------------------------------------------------------------------------
-- Compose with MonadThrow
-------------------------------------------------------------------------------

newtype ExampleException = ExampleException String deriving (Eq, Show)

instance Exception ExampleException

composeWithMonadThrow
    :: ( IsStream t
       , Semigroup (t IO Int)
       , MonadThrow (t IO)
       )
    => (t IO Int -> SerialT IO Int)
    -> Spec
composeWithMonadThrow t = do
    it "Compose throwM, nil" $
        try (tl (throwM (ExampleException "E") <> S.nil))
        `shouldReturn` (Left (ExampleException "E") :: Either ExampleException [Int])
    it "Compose nil, throwM" $
        try (tl (S.nil <> throwM (ExampleException "E")))
        `shouldReturn` (Left (ExampleException "E") :: Either ExampleException [Int])
    oneLevelNestedSum "serially" S.serially
    oneLevelNestedSum "wSerially" S.wSerially
    oneLevelNestedSum "asyncly" S.asyncly
    oneLevelNestedSum "wAsyncly" S.wAsyncly
    -- XXX add two level nesting

    oneLevelNestedProduct "serially" S.serially
    oneLevelNestedProduct "wSerially" S.wSerially
    oneLevelNestedProduct "asyncly" S.asyncly
    oneLevelNestedProduct "wAsyncly"  S.wAsyncly

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
            let s1 = t $ S.concatMapFoldableWith (<>) return [1..4]
                s2 = t1 $ S.concatMapFoldableWith (<>) return [5..8]
            try $ tl (do
                x <- S.adapt s1
                y <- s2
                if x + y > 10
                then throwM (ExampleException "E")
                else return (x + y)
                )
            `shouldReturn` (Left (ExampleException "E") :: Either ExampleException [Int])

-------------------------------------------------------------------------------
-- Cleanup tests
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Some ad-hoc tests that failed at times
-------------------------------------------------------------------------------

takeCombined :: (Monad m, Semigroup (t m Int), Show a, Eq a, IsStream t)
    => Int -> (t m Int -> SerialT IO a) -> IO ()
takeCombined n t = do
    let constr = S.fromFoldable
    r <- (S.toList . t) $
            S.take n (constr ([] :: [Int]) <> constr ([] :: [Int]))
    r `shouldBe` []

-------------------------------------------------------------------------------
-- Helper operations
-------------------------------------------------------------------------------

folded :: IsStream t => [a] -> t IO a
folded =
    serially .
    (\xs ->
         case xs of
             [x] -> return x -- singleton stream case
             _ -> S.concatMapFoldableWith (<>) return xs)

#ifndef COVERAGE_BUILD
makeCommonOps :: IsStream t => (t m a -> c) -> [(String, t m a -> c)]
#else
makeCommonOps :: b -> [(String, b)]
#endif
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

#ifndef COVERAGE_BUILD
makeOps :: IsStream t => (t m a -> c) -> [(String, t m a -> c)]
#else
makeOps :: b -> [(String, b)]
#endif
makeOps t = makeCommonOps t ++
            [
#ifndef COVERAGE_BUILD
              ("maxBuffer 1", t . maxBuffer 1)
#endif
            ]

mapOps :: (a -> Spec) -> [(String, a)] -> Spec
mapOps spec = mapM_ (\(desc, f) -> describe desc $ spec f)
