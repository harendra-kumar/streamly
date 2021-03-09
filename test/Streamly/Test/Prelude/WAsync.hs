-- |
-- Module      : Streamly.Test.Prelude.WAsync
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude.WAsync where

#ifdef DEVBUILD
import Control.Concurrent ( threadDelay )
#endif
import Data.List (sort)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif
import Test.Hspec.QuickCheck
import Test.Hspec as H

import Streamly.Prelude
import qualified Streamly.Prelude as S

import Streamly.Test.Prelude.Common

main :: IO ()
main = hspec
    $ H.parallel
#ifdef COVERAGE_BUILD
    $ modifyMaxSuccess (const 10)
#endif
    $ do
    let wAsyncOps :: IsStream t => ((WAsyncT IO a -> t IO a) -> Spec) -> Spec
        wAsyncOps spec = mapOps spec $ makeOps wAsyncly
#ifndef COVERAGE_BUILD
            <> [("maxBuffer (-1)", wAsyncly . maxBuffer (-1))]
#endif

    describe "Construction" $ do
        wAsyncOps $ prop "wAsyncly replicateM" . constructWithReplicateM
        -- XXX add tests for fromIndices
        wAsyncOps $ prop "wAsyncly cons" . constructWithCons S.cons
        wAsyncOps $ prop "wAsyncly consM" . constructWithConsM S.consM sort
        wAsyncOps $ prop "wAsyncly (.:)" . constructWithCons (S..:)
        wAsyncOps $ prop "wAsyncly (|:)" . constructWithConsM (S.|:) sort

    describe "Functor operations" $ do
        wAsyncOps $ functorOps S.fromFoldable "wAsyncly" sortEq
        wAsyncOps $ functorOps folded "wAsyncly folded" sortEq

    describe "Monoid operations" $ do
        wAsyncOps $ monoidOps "wAsyncly" mempty sortEq

    describe "WAsync loops" $ loops wAsyncly sort sort

    describe "Bind and Monoidal composition combinations" $ do
        wAsyncOps $ bindAndComposeSimpleOps "WAsync" sortEq
        wAsyncOps $ bindAndComposeHierarchyOps "WAsync"
        wAsyncOps $ nestTwoStreams "WAsync" sort sort
        wAsyncOps $ nestTwoStreamsApp "WAsync" sort sort

    describe "Semigroup operations" $ do
        wAsyncOps $ semigroupOps "wAsyncly" sortEq

    describe "Applicative operations" $ do
        wAsyncOps $ applicativeOps S.fromFoldable "wAsyncly applicative" sortEq
        wAsyncOps $ applicativeOps folded "wAsyncly applicative folded" sortEq

    -- XXX add tests for indexed/indexedR
    describe "Zip operations" $
        -- We test only the serial zip with serial streams and the parallel
        -- stream, because the rate setting in these streams can slow down
        -- zipAsync.
     do
        wAsyncOps $ prop "zip applicative wAsyncly" . zipAsyncApplicative S.fromFoldable (==)
        wAsyncOps $ prop "zip applicative wAsyncly folded" . zipAsyncApplicative folded (==)
        wAsyncOps $
            prop "zip monadic wAsyncly" . zipAsyncMonadic S.fromFoldable (==)
        wAsyncOps $ prop "zip monadic wAsyncly folded" . zipAsyncMonadic folded (==)

    -- XXX add merge tests like zip tests
    -- for mergeBy, we can split a list randomly into two lists and
    -- then merge them, it should result in original list
    -- describe "Merge operations" $ do

    describe "Monad operations" $ do
        wAsyncOps $ prop "wAsyncly monad then" . monadThen S.fromFoldable sortEq
        wAsyncOps $ prop "wAsyncly monad then folded" . monadThen folded sortEq
        wAsyncOps $ prop "wAsyncly monad bind" . monadBind S.fromFoldable sortEq
        wAsyncOps $ prop "wAsyncly monad bind folded" . monadBind folded sortEq

    describe "Stream transform and combine operations" $ do
        wAsyncOps $ transformCombineOpsCommon S.fromFoldable "wAsyncly" sortEq
        wAsyncOps $ transformCombineOpsCommon folded "wAsyncly" sortEq

    describe "Stream elimination operations" $ do
        wAsyncOps $ eliminationOps S.fromFoldable "wAsyncly"
        wAsyncOps $ eliminationOps folded "wAsyncly folded"
        wAsyncOps $ eliminationOpsWord8 S.fromFoldable "wAsyncly"
        wAsyncOps $ eliminationOpsWord8 folded "wAsyncly folded"

    -- describe "WAsync interleaved (<>) ordering check" $
    --     interleaveCheck wAsyncly (<>)
    -- describe "WAsync interleaved mappend ordering check" $
    --     interleaveCheck wAsyncly mappend

    -- XXX this keeps failing intermittently, need to investigate
    -- describe "WAsync (<>) time order check" $
    --     parallelCheck wAsyncly (<>)
    -- describe "WAsync mappend time order check" $
    --     parallelCheck wAsyncly mappend

    describe "Tests for exceptions" $ wAsyncOps $ exceptionOps "wAsyncly"
    describe "Composed MonadThrow wAsyncly" $ composeWithMonadThrow wAsyncly

    -- Ad-hoc tests
    it "takes n from stream of streams" (takeCombined 3 wAsyncly)

#ifdef DEVBUILD
    let timed :: (IsStream t, Monad (t IO)) => Int -> t IO Int
        timed x = S.valueM (threadDelay (x * 100000)) >> return x

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
#endif
