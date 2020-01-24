module Main (main) where

import qualified Streamly.Prelude as S
import Streamly.Internal.Data.Fold

import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, arbitrary, choose)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Test.Hspec as H

maxStreamLen :: Int
maxStreamLen = 1000

testRollingHashFirstN :: Property
testRollingHashFirstN = 
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (choose (0, len)) $ \n ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec -> monadicIO $ do
                a <- run $ S.fold rollingHash $ S.take n $ S.fromList vec
                b <- run $ S.fold (rollingHashFirstN n) $ S.fromList vec
                assert $ a == b

main :: IO ()
main = hspec $
    describe "Rolling Hash Folds" $
        prop "testRollingHashFirstN" testRollingHashFirstN
