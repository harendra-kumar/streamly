-- |
-- Module      : Streamly.Test.Prelude.Serial
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude.Serial where

import Control.Concurrent ( threadDelay )
import Control.Monad ( when, forM_ )
import Data.Function ( (&) )
import Data.IORef ( newIORef, readIORef, writeIORef, modifyIORef' )
import Data.Int (Int64)
import Data.List (group, intercalate)
import Data.Maybe ( isJust, fromJust )
import Foreign.Storable (Storable)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup ((<>))
#endif
import Data.Semigroup (Sum(..), getSum)
import Test.Hspec.QuickCheck
import Test.QuickCheck
    ( Gen
    , Property
    , Arbitrary(..)
    , choose
    , elements
    , forAll
    , frequency
    , listOf
    , listOf1
    , suchThat
    , vectorOf
    , withMaxSuccess
    )
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import Test.Hspec as H

import Streamly.Prelude (SerialT, IsStream, serial, serially)
#ifndef COVERAGE_BUILD
import Streamly.Prelude (avgRate, maxBuffer)
#endif
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream.IsStream as IS
import qualified Streamly.Data.Array.Foreign as A

import Streamly.Internal.Data.Time.Units
       (AbsTime, NanoSecond64(..), toRelTime64, diffAbsTime64)

#ifdef DEVBUILD
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
#endif

import Streamly.Test.Common
import Streamly.Test.Prelude.Common

splitOnSeq :: Spec
splitOnSeq = do
    describe "Tests for splitOnSeq" $ do
        it "splitOnSeq' \"hello\" \"\" = [\"\"]"
          $ splitOnSeq' "hello" "" `shouldReturn` [""]
        it "splitOnSeq' \"hello\" \"hello\" = [\"\", \"\"]"
          $ splitOnSeq' "hello" "hello" `shouldReturn` ["", ""]
        it "splitOnSeq' \"x\" \"hello\" = [\"hello\"]"
          $ splitOnSeq' "x" "hello" `shouldReturn` ["hello"]
        it "splitOnSeq' \"h\" \"hello\" = [\"\", \"ello\"]"
          $ splitOnSeq' "h" "hello" `shouldReturn` ["", "ello"]
        it "splitOnSeq' \"o\" \"hello\" = [\"hell\", \"\"]"
          $ splitOnSeq' "o" "hello" `shouldReturn` ["hell", ""]
        it "splitOnSeq' \"e\" \"hello\" = [\"h\", \"llo\"]"
          $ splitOnSeq' "e" "hello" `shouldReturn` ["h", "llo"]
        it "splitOnSeq' \"l\" \"hello\" = [\"he\", \"\", \"o\"]"
          $ splitOnSeq' "l" "hello" `shouldReturn` ["he", "", "o"]
        it "splitOnSeq' \"ll\" \"hello\" = [\"he\", \"o\"]"
          $ splitOnSeq' "ll" "hello" `shouldReturn` ["he", "o"]

    where

    splitOnSeq' pat xs =
        S.toList $ IS.splitOnSeq (A.fromList pat) FL.toList (S.fromList xs)

splitOnSuffixSeq :: Spec
splitOnSuffixSeq = do
    describe "Tests for splitOnSuffixSeq" $ do
        it "splitSuffixOn_ \".\" \"\" []"
          $ splitSuffixOn_ "." "" `shouldReturn` []
        it "splitSuffixOn_ \".\" \".\" [\"\"]"
          $ splitSuffixOn_ "." "." `shouldReturn` [""]
        it "splitSuffixOn_ \".\" \"a\" [\"a\"]"
          $ splitSuffixOn_ "." "a" `shouldReturn` ["a"]
        it "splitSuffixOn_ \".\" \".a\" [\"\",\"a\"]"
          $ splitSuffixOn_ "." ".a" `shouldReturn` ["", "a"]
        it "splitSuffixOn_ \".\" \"a.\" [\"a\"]"
          $ splitSuffixOn_ "." "a." `shouldReturn` ["a"]
        it "splitSuffixOn_ \".\" \"a.b\" [\"a\",\"b\"]"
          $ splitSuffixOn_ "." "a.b" `shouldReturn` ["a", "b"]
        it "splitSuffixOn_ \".\" \"a.b.\" [\"a\",\"b\"]"
          $ splitSuffixOn_ "." "a.b." `shouldReturn` ["a", "b"]
        it "splitSuffixOn_ \".\" \"a..b..\" [\"a\",\"\",\"b\",\"\"]"
          $ splitSuffixOn_ "." "a..b.." `shouldReturn` ["a", "", "b", ""]

    where

    splitSuffixOn_ pat xs =
        S.toList
             $ IS.splitOnSuffixSeq (A.fromList pat) FL.toList (S.fromList xs)

splitterProperties ::
       forall a. (Arbitrary a, Eq a, Show a, Storable a, Enum a)
    => a
    -> String
    -> Spec
splitterProperties sep desc = do
    describe (desc <> " splitOnSeq")
        $ do

            forM_ [0, 1, 2, 4]
                $ intercalateSplitEqId splitOnSeq_ intercalate IS.intercalate

            forM_ [0, 1, 2, 4]
                $ concatSplitIntercalateEqConcat
                      splitOnSeq_
                      intercalate
                      IS.intercalate

            -- Exclusive case
            splitIntercalateEqId splitOnSeq_ intercalate IS.intercalate

    describe (desc <> " splitOn")
        $ do

            intercalateSplitEqId splitOn_ intercalate IS.intercalate 1

            concatSplitIntercalateEqConcat
                splitOn_ intercalate IS.intercalate 1

            -- Exclusive case
            splitIntercalateEqId splitOn_ intercalate IS.intercalate

    describe (desc <> " splitOnSuffixSeq")
        $ do

            forM_ [0, 1, 2, 4]
                $ intercalateSplitEqIdNoSepEnd
                      splitOnSuffixSeq_
                      intercalate
                      IS.intercalate

            forM_ [0, 1, 2, 4]
                $ concatSplitIntercalateEqConcat
                      splitOnSuffixSeq_
                      intercalateSuffix
                      IS.intercalateSuffix

            -- Exclusive case
            splitIntercalateEqId
                splitOnSuffixSeq_
                intercalateSuffix
                IS.intercalateSuffix

    describe (desc <> " splitOnSuffix")
        $ do

            intercalateSplitEqIdNoSepEnd
                splitOnSuffix_ intercalate IS.intercalate 1

            concatSplitIntercalateEqConcat
                splitOnSuffix_ intercalateSuffix IS.intercalateSuffix 1

            -- Exclusive case
            splitIntercalateEqId
                splitOnSuffix_ intercalateSuffix IS.intercalateSuffix

    where

    splitOnSeq_ xs ys =
        S.toList $ IS.splitOnSeq (A.fromList ys) FL.toList (S.fromList xs)

    splitOnSuffixSeq_ xs ys =
        S.toList $ IS.splitOnSuffixSeq (A.fromList ys) FL.toList (S.fromList xs)

    splitOn_ xs ys =
        S.toList $ IS.splitOn (== (head ys)) FL.toList (S.fromList xs)

    splitOnSuffix_ xs ys =
        S.toList $ IS.splitOnSuffix (== (head ys)) FL.toList (S.fromList xs)

    intercalateSuffix xs yss = intercalate xs yss ++ xs

    nonSepElem :: Gen a
    nonSepElem = suchThat arbitrary (/= sep)

    listWithSep :: Gen [a]
    listWithSep = listOf $ frequency [(3, arbitrary), (1, elements [sep])]

    listWithoutSep :: Gen [a]
    listWithoutSep = vectorOf 4 nonSepElem

    listsWithoutSep :: Gen [[a]]
    listsWithoutSep = listOf listWithoutSep

    listsWithoutSep1 :: Gen [[a]]
    listsWithoutSep1 = listOf1 listWithoutSep

    intercalateSplitEqId splitter lIntercalater sIntercalater i =
        let name =
                "intercalater . splitter == id ("
                    <> show i <> " element separator)"
         in prop name
                $ forAll listWithSep
                $ \xs -> withMaxSuccess maxTestCount $ monadicIO $ testCase xs

        where

        testCase xs = do
            ys <- splitter xs (replicate i sep)
            szs <-
                IS.toList
                    $ sIntercalater (replicate i sep) UF.fromList
                    $ IS.fromList ys
            let lzs = lIntercalater (replicate i sep) ys
            listEquals (==) szs xs
            listEquals (==) lzs xs

    intercalateSplitEqIdNoSepEnd splitter lIntercalater sIntercalater i =
        let name =
                "intercalater . splitter . (++ [x \\= sep]) == id ("
                    <> show i <> " element separator)"
         in prop name
                $ forAll ((,) <$> listWithSep <*> nonSepElem)
                $ \(xs_, nonSep) -> do
                      let xs = xs_ ++ [nonSep]
                      withMaxSuccess maxTestCount $ monadicIO $ testCase xs

        where

        testCase xs = do
            ys <- splitter xs (replicate i sep)
            szs <-
                IS.toList
                    $ sIntercalater (replicate i sep) UF.fromList
                    $ IS.fromList ys
            let lzs = lIntercalater (replicate i sep) ys
            listEquals (==) szs xs
            listEquals (==) lzs xs

    concatSplitIntercalateEqConcat splitter lIntercalater sIntercalater i =
        let name =
                "concat . splitter . S.intercalater == "
                    <> "concat ("
                    <> show i <> " element separator/possibly empty list)"
         in prop name
                $ forAll listsWithoutSep
                $ \xss -> withMaxSuccess maxTestCount $ monadicIO $ testCase xss

        where

        testCase xss = do
            let lxs = lIntercalater (replicate i sep) xss
            lys <- splitter lxs (replicate i sep)
            sxs <-
                S.toList
                    $ sIntercalater (replicate i sep) UF.fromList
                    $ S.fromList xss
            sys <- splitter sxs (replicate i sep)
            listEquals (==) (concat lys) (concat xss)
            listEquals (==) (concat sys) (concat xss)

    splitIntercalateEqId splitter lIntercalater sIntercalater =
        let name =
                "splitter . intercalater == id"
                    <> " (exclusive separator/non-empty list)"
         in prop name
                $ forAll listsWithoutSep1
                $ \xss -> do
                      withMaxSuccess maxTestCount $ monadicIO $ testCase xss

        where

        testCase xss = do
            let lxs = lIntercalater [sep] xss
            lys <- splitter lxs [sep]
            sxs <- S.toList $ sIntercalater [sep] UF.fromList $ S.fromList xss
            sys <- splitter sxs [sep]
            listEquals (==) lys xss
            listEquals (==) sys xss


groupSplitOps :: String -> Spec
groupSplitOps desc = do
    -- splitting
    splitOnSeq
    splitOnSuffixSeq

    -- splitting properties
    splitterProperties (0 :: Int) desc
    -- XXX This will fail
    -- splitterProperties (0 :: Word8) desc

    prop (desc <> " intercalate [x] . splitOn (== x) == id") $
        forAll listWithZeroes $ \xs -> do
            withMaxSuccess maxTestCount $
                monadicIO $ do
                    ys <- S.toList $ S.splitOn (== 0) toListFL (S.fromList xs)
                    listEquals (==) (intercalate [0] ys) xs

    where

    listWithZeroes :: Gen [Int]
    listWithZeroes = listOf $ frequency [(3, arbitrary), (1, elements [0])]

-- |
-- After grouping (and folding) Int stream using @>@ operation,
-- the first @Int@ of every @[Int]@ in the @[Int]@ stream should be the minimum.
testGroupsBy :: Property
testGroupsBy =
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec -> monadicIO $ do
            r <- run $ S.all (\ls ->
                case ls of
                    [] -> True
                    (x:_) -> x == minimum ls)
                $ S.groupsBy (>) FL.toList
                $ S.fromList vec
            assert r

testGroups :: Property
testGroups =
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec -> monadicIO $ do
            r <- run $ S.toList $ S.groups FL.toList $ S.fromList vec
            assert $ r == group vec

testGroupsByRolling :: Property
testGroupsByRolling =
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec -> monadicIO $ do
            r <- run $ S.toList $ S.groupsByRolling (==) FL.toList $ S.fromList vec
            assert $ r == group vec

-- |
-- If the list is empty, returns Nothing,
-- else wraps the minimum value of the list in Just.
maybeMinimum :: [Int] -> Maybe Int
maybeMinimum [] = Nothing
maybeMinimum ls = Just $ minimum ls

-- |
-- Checks if the @[Int]@ is non-increasing.
decreasing :: [Maybe Int] -> Bool
decreasing [] = True
decreasing xs = all (== True) $ zipWith (<=) (tail xs) xs

-- |
-- To check if the minimum elements (after grouping on @>@)
-- are non-increasing (either decrease or remain the same).
-- Had an element been strictly greater, it would have been grouped
-- with that element only.
testGroupsBySep :: Property
testGroupsBySep =
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec -> monadicIO $ do
            a <- run $ S.toList
                $ S.map maybeMinimum
                $ S.groupsBy (>) FL.toList
                $ S.fromList vec
            assert $ decreasing a

groupingOps :: Spec
groupingOps = do
    prop "groupsBy" testGroupsBy
    prop "S.groups = groups" testGroups
    prop "S.groupsByRolling = groups" testGroupsByRolling
    prop "testGroupsBySep" testGroupsBySep

associativityCheck
    :: String
    -> (SerialT IO Int -> SerialT IO Int)
    -> Spec
associativityCheck desc t = prop desc assocCheckProp
  where
    assocCheckProp :: [Int] -> [Int] -> [Int] -> Property
    assocCheckProp xs ys zs =
        monadicIO $ do
            let xStream = S.fromList xs
                yStream = S.fromList ys
                zStream = S.fromList zs
            infixAssocstream <-
                run $ S.toList $ t $ xStream `serial` yStream `serial` zStream
            assocStream <- run $ S.toList $ t $ xStream <> yStream <> zStream
            listEquals (==) infixAssocstream assocStream

max_length :: Int
max_length = 1000

tenPow8 :: Int64
tenPow8 = 10^(8 :: Int)

tenPow7 :: Int64
tenPow7 = 10^(7 :: Int)

takeDropTime :: NanoSecond64
takeDropTime = NanoSecond64 $ 5 * tenPow8

checkTakeDropTime :: (Maybe AbsTime, Maybe AbsTime) -> IO Bool
checkTakeDropTime (mt0, mt1) = do
    let graceTime = NanoSecond64 $ 8 * tenPow7
    case mt0 of
        Nothing -> return True
        Just t0 ->
            case mt1 of
                Nothing -> return True
                Just t1 -> do
                    let tMax = toRelTime64 (takeDropTime + graceTime)
                    let tMin = toRelTime64 (takeDropTime - graceTime)
                    let t = diffAbsTime64 t1 t0
                    let r = t >= tMin && t <= tMax
                    when (not r) $ putStrLn $
                        "t = " ++ show t ++
                        " tMin = " ++ show tMin ++
                        " tMax = " ++ show tMax
                    return r

#ifdef DEVBUILD
testTakeByTime :: IO Bool
testTakeByTime = do
    r <-
          S.fold ((,) <$> FL.head <*> FL.last)
        $ IS.takeByTime takeDropTime
        $ S.repeatM (threadDelay 1000 >> getTime Monotonic)
    checkTakeDropTime r

testDropByTime :: IO Bool
testDropByTime = do
    t0 <- getTime Monotonic
    mt1 <-
          S.head
        $ IS.dropByTime takeDropTime
        $ S.repeatM (threadDelay 1000 >> getTime Monotonic)
    checkTakeDropTime (Just t0, mt1)
#endif

unfold :: Property
unfold = monadicIO $ do
    a <- pick $ choose (0, max_length `div` 2)
    b <- pick $ choose (0, max_length)
    let unf = UF.enumerateFromToIntegral b
    ls <- S.toList $ S.unfold unf a
    return $ ls == [a..b]

unfold0 :: Property
unfold0 = monadicIO $ do
    a <- pick $ choose (0, max_length `div` 2)
    b <- pick $ choose (0, max_length)
    let unf = UF.supply (UF.enumerateFromToIntegral b) a
    ls <- S.toList $ IS.unfold0 unf
    return $ ls == [a..b]

testFromCallback :: IO Int
testFromCallback = do
    ref <- newIORef Nothing
    let stream = S.map Just (IS.fromCallback (setCallback ref))
                    `S.parallel` runCallback ref
    S.sum $ S.map fromJust $ S.takeWhile isJust stream

    where

    setCallback ref cb = do
        writeIORef ref (Just cb)

    runCallback ref = S.yieldM $ do
        cb <-
              S.repeatM (readIORef ref)
                & IS.delayPost 0.1
                & S.mapMaybe id
                & S.head

        S.fromList [1..100]
            & IS.delayPost 0.001
            & S.mapM_ (fromJust cb)
        threadDelay 100000
        return Nothing

foldIterateM :: Property
foldIterateM =
    forAll (listOf (chooseInt (0, max_length))) $ \lst -> monadicIO $ do
        let s1 = Prelude.sum lst
            strm = S.fromList lst
        ms2 <-
            S.last
                $ S.map getSum
                $ IS.foldIterateM
                      (return . FL.take 1 . FL.sconcat)
                      (Sum 0)
                $ S.map Sum strm
        case ms2 of
            Nothing -> assert $ s1 == 0
            Just s2 -> assert $ s1 == s2

main :: IO ()
main = hspec
    $ H.parallel
#ifdef COVERAGE_BUILD
    $ modifyMaxSuccess (const 10)
#endif
    $ do
    let serialOps :: IsStream t => ((SerialT IO a -> t IO a) -> Spec) -> Spec
        serialOps spec = mapOps spec $ makeOps serially
#ifndef COVERAGE_BUILD
            <> [("rate AvgRate 0.00000001", serially . avgRate 0.00000001)]
            <> [("maxBuffer -1", serially . maxBuffer (-1))]
#endif
    let toListSerial :: SerialT IO a -> IO [a]
        toListSerial = S.toList . serially

    describe "Runners" $ do
        -- XXX use an IORef to store and check the side effects
        it "simple serially" $
            (S.drain . serially) (return (0 :: Int)) `shouldReturn` ()
        it "simple serially with IO" $
            (S.drain . serially) (S.yieldM $ putStrLn "hello") `shouldReturn` ()

    describe "Empty" $
        it "Monoid - mempty" $
            toListSerial mempty `shouldReturn` ([] :: [Int])
        -- it "Alternative - empty" $
        --     (toListSerial empty) `shouldReturn` ([] :: [Int])
        -- it "MonadPlus - mzero" $
        --     (toListSerial mzero) `shouldReturn` ([] :: [Int])

    describe "Construction" $ do
        -- Add all the construction tests for all stream types.
        serialOps   $ prop "serially repeat" . constructWithRepeat
        serialOps   $ prop "serially repeatM" . constructWithRepeatM
        serialOps   $ prop "serially replicate" . constructWithReplicate
        serialOps   $ prop "serially replicateM" . constructWithReplicateM
        serialOps   $ prop "serially intFromThenTo" .
                            constructWithIntFromThenTo
#if __GLASGOW_HASKELL__ >= 806
        serialOps   $ prop "serially DoubleFromThenTo" .
                            constructWithDoubleFromThenTo
#endif
        serialOps   $ prop "serially iterate" . constructWithIterate
        -- XXX test for all types of streams
        serialOps   $ prop "serially iterateM" . constructWithIterateM
        serialOps $ prop "serially enumerate" . constructWithEnumerate id
        serialOps $ prop "serially enumerateTo" . constructWithEnumerateTo id
        serialOps $ prop "serially fromIndices" . constructWithFromIndices
        serialOps $ prop "serially fromIndicesM" . constructWithFromIndicesM
        serialOps $ prop "serially fromList" . constructWithFromList id
        serialOps $ prop "serially fromListM" . constructWithFromListM id
        serialOps $ prop "serially unfoldr" . constructWithUnfoldr id
        serialOps $ prop "serially value" . constructWithValue id
        serialOps $ prop "serially yieldM" . constructWithYieldM id
        serialOps $ prop "serially cons" . constructWithCons S.cons
        serialOps $ prop "serially consM" . constructWithConsM S.consM id
        serialOps $ prop "serially (.:)" . constructWithCons (S..:)
        serialOps $ prop "serially (|:)" . constructWithConsM (S.|:) id

        describe "From Generators" $ do
            prop "unfold" unfold
            prop "unfold0" unfold0

    describe "Simple Operations" $ serialOps simpleOps

    describe "Functor operations" $ do
        serialOps    $ functorOps S.fromFoldable "serially" (==)
        serialOps    $ functorOps folded "serially folded" (==)

    describe "Monoid operations" $ do
        serialOps $ monoidOps "serially" mempty (==)

    describe "Serial loops" $ loops serially id reverse

    describe "Bind and Monoidal composition combinations" $ do
        -- XXX Taking a long time when serialOps is used.
        bindAndComposeSimpleOps "Serial" sortEq serially
        bindAndComposeHierarchyOps "Serial" serially
        serialOps $ nestTwoStreams "Serial" id id
        serialOps $ nestTwoStreamsApp "Serial" id id
        composeAndComposeSimpleSerially "Serial <> " (repeat [1..9]) serially
        composeAndComposeSimpleAheadly "Serial <> " (repeat [1 .. 9]) serially
        composeAndComposeSimpleWSerially
            "Serial <> "
            [[1..9], [1..9], [1,3,2,4,6,5,7,9,8], [1,3,2,4,6,5,7,9,8]]
            serially

    describe "Semigroup operations" $ do
        serialOps $ semigroupOps "serially" (==)
        serialOps $ associativityCheck "serial == <>"

    describe "Applicative operations" $ do
        -- The tests using sorted equality are weaker tests
        -- We need to have stronger unit tests for all those
        -- XXX applicative with three arguments
        serialOps $ applicativeOps S.fromFoldable "serially" (==)
        serialOps $ applicativeOps folded "serially folded" (==)
        serialOps $ applicativeOps1 S.fromFoldable "serially" (==)
        serialOps $ applicativeOps1 S.fromFoldable "serially folded" (==)

    -- XXX add tests for indexed/indexedR
    describe "Zip operations" $ do
        -- We test only the serial zip with serial streams and the parallel
        -- stream, because the rate setting in these streams can slow down
        -- zipAsync.
        serialOps   $ prop "zip monadic serially" . zipMonadic S.fromFoldable (==)
        serialOps   $ prop "zip monadic serially folded" . zipMonadic folded (==)

    -- XXX add merge tests like zip tests
    -- for mergeBy, we can split a list randomly into two lists and
    -- then merge them, it should result in original list
    -- describe "Merge operations" $ do

    describe "Monad operations" $ do
        serialOps   $ prop "serially monad then" . monadThen S.fromFoldable (==)
        serialOps   $ prop "serially monad then folded" . monadThen folded (==)
        serialOps   $ prop "serially monad bind" . monadBind S.fromFoldable (==)
        serialOps   $ prop "serially monad bind folded"  . monadBind folded (==)

    describe "Stream transform and combine operations" $ do
        serialOps    $ transformCombineOpsCommon S.fromFoldable "serially" (==)
        serialOps    $ transformCombineOpsCommon folded "serially" (==)
        serialOps    $ transformCombineOpsOrdered S.fromFoldable "serially" (==)
        serialOps    $ transformCombineOpsOrdered folded "serially" (==)

#ifdef DEVBUILD
        describe "Filtering" $ do
            it "takeByTime" (testTakeByTime `shouldReturn` True)
            it "dropByTime" (testDropByTime `shouldReturn` True)
#endif

    describe "Stream group and split operations" $ do
        groupSplitOps "serially"

    describe "Stream elimination operations" $ do
        serialOps    $ eliminationOps S.fromFoldable "serially"
        serialOps    $ eliminationOps folded "serially folded"
        serialOps    $ eliminationOpsWord8 S.fromFoldable "serially"
        serialOps    $ eliminationOpsWord8 folded "serially folded"
        serialOps $ \t ->
            prop "drainWhile (> 0)" $ \n ->
                withMaxSuccess maxTestCount $
                monadicIO $ do
                    let xs = [1..n]
                    ioRef <- run $ newIORef ([] :: [Int])
                    run $
                        S.drainWhile (> 0) . t $
                        S.mapM (\a -> modifyIORef' ioRef (a :) >> return a) $
                        S.fromList xs
                    strm <- run $ readIORef ioRef
                    listEquals (==) (reverse strm) (takeWhile (> 0) xs)

    -- XXX Add a test where we chain all transformation APIs and make sure that
    -- the state is being passed through all of them.
    describe "Stream serial elimination operations" $ do
        serialOps    $ eliminationOpsOrdered S.fromFoldable "serially"
        serialOps    $ eliminationOpsOrdered folded "serially folded"

    describe "Tests for S.groupsBy" groupingOps

    describe "Tests for exceptions" $ serialOps $ exceptionOps "serially"

    describe "Composed MonadThrow serially" $ composeWithMonadThrow serially

    it "fromCallback" $ testFromCallback `shouldReturn` (50*101)

    describe "Nesting" $ do
        prop "foldIterateM" foldIterateM
