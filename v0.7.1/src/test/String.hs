{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Data.Char (ord, chr)
import Data.Word (Word8)
import Test.Hspec.QuickCheck
import Test.QuickCheck
       (Property, forAll, Gen, listOf, arbitraryUnicodeChar, arbitrary)
import Test.QuickCheck.Monadic (run, monadicIO, assert)

import           Test.Hspec as H

import qualified Streamly.Memory.Array as A
import qualified Streamly.Internal.Memory.ArrayStream as AS
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Unicode.Stream as SS
import qualified Streamly.Internal.Data.Unicode.Stream as IUS
import qualified Streamly.Internal.Memory.Unicode.Array as IUA

-- Coverage build takes too long with default number of tests
{-
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif
-}

-- Use quickcheck-unicode instead?
genUnicode :: Gen String
genUnicode = listOf arbitraryUnicodeChar

genWord8 :: Gen [Word8]
genWord8 = listOf arbitrary

propDecodeEncodeId :: Property
propDecodeEncodeId =
    forAll genUnicode $ \list ->
        monadicIO $ do
            let wrds = SS.encodeUtf8 $ S.fromList list
            chrs <- S.toList $ SS.decodeUtf8 wrds
            assert (chrs == list)

-- XXX need to use invalid characters
propDecodeEncodeIdLenient :: Property
propDecodeEncodeIdLenient =
    forAll genUnicode $ \list ->
        monadicIO $ do
            let wrds = SS.encodeUtf8 $ S.fromList list
            chrs <- S.toList $ SS.decodeUtf8Lax wrds
            assert (chrs == list)

propDecodeEncodeIdArrays :: Property
propDecodeEncodeIdArrays =
    forAll genUnicode $ \list ->
        monadicIO $ do
            let wrds = SS.encodeUtf8 $ S.fromList list
            chrs <- S.toList $ IUS.decodeUtf8ArraysLenient
                                    (S.fold A.write wrds)
            assert (chrs == list)

testLines :: Property
testLines =
    forAll genUnicode $ \list ->
        monadicIO $ do
            xs <- S.toList
                $ S.map A.toList
                $ IUA.lines
                $ S.fromList list
            assert (xs == lines list)

testLinesArray :: Property
testLinesArray =
    forAll genWord8 $ \list ->
        monadicIO $ do
            xs <- S.toList
                    $ S.map A.toList
                    $ AS.splitOnSuffix 10
                    $ S.yield (A.fromList list)
            assert (xs == map (map (fromIntegral . ord))
                              (lines (map (chr .  fromIntegral) list)))

testWords :: Property
testWords =
    forAll genUnicode $ \list ->
        monadicIO $ do
            xs <- S.toList
                $ S.map A.toList
                $ IUA.words
                $ S.fromList list
            assert (xs == words list)

testUnlines :: Property
testUnlines =
  forAll genUnicode $ \list ->
      monadicIO $ do
          xs <- S.toList
              $ IUA.unlines
              $ IUA.lines
              $ S.fromList list
          assert (xs == unlines (lines list))

testUnwords :: Property
testUnwords =
  forAll genUnicode $ \list ->
      monadicIO $ do
          xs <- run
              $ S.toList
              $ IUA.unwords
              $ IUA.words
              $ S.fromList list
          assert (xs == unwords (words list))

main :: IO ()
main = hspec
    $ H.parallel
    $ modifyMaxSuccess (const 1000)
    $ do
    describe "UTF8 - Encoding / Decoding" $ do
        prop "decodeUtf8 . encodeUtf8 == id" $ propDecodeEncodeId
        prop "decodeUtf8Lax . encodeUtf8 == id" $ propDecodeEncodeIdLenient
        prop "decodeUtf8ArraysLenient . encodeUtf8 == id"
                $ propDecodeEncodeIdArrays
        prop "Streamly.Data.String.lines == Prelude.lines" $ testLines
        prop "Arrays Streamly.Data.String.lines == Prelude.lines" $ testLinesArray
        prop "Streamly.Data.String.words == Prelude.words" $ testWords
        prop
            "Streamly.Data.String.unlines . Streamly.Data.String.lines == unlines . lines"
             $ testUnlines
        prop
            "Streamly.Data.String.unwords . Streamly.Data.String.words == unwords . words"
             $ testUnwords
