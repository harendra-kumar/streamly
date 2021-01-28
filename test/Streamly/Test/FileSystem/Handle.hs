-- |
-- Module      : Streamly.Test.FileSystem.Handle
-- Copyright   : (c) 2020 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.FileSystem.Handle (main) where

import Data.Functor.Identity (runIdentity)
import Data.Word (Word8)
import Foreign.Storable (Storable(..))
import Streamly.Internal.Data.Stream.IsStream (IsStream, SerialT)
import System.FilePath ((</>))
import System.IO
    ( Handle
    , IOMode(..)
    , SeekMode(..)
    , hClose
    , hFlush
    , hSeek
    , openFile
    )
import System.IO.Temp (withSystemTempDirectory)
import Test.QuickCheck (Property, forAll, Gen, vectorOf, choose)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Test.Hspec as H
import Test.Hspec.QuickCheck

allocOverhead :: Int
allocOverhead = 2 * sizeOf (undefined :: Int)

defaultChunkSize :: Int
defaultChunkSize = 32 * k - allocOverhead
   where k = 1024

maxArrLen :: Int
maxArrLen = defaultChunkSize * 8

maxTestCount :: Int
maxTestCount = 10

chooseWord8 :: (Word8, Word8) -> Gen Word8
chooseWord8 = choose

utf8ToString :: Array.Array Word8 -> String
utf8ToString =
    runIdentity . Stream.toList . Unicode.decodeUtf8' . Array.toStream

testData :: String
testData = "This is the test data for FileSystem.Handle ??`!@#$%^&*~~))`]"

testDataLarge :: String
testDataLarge = concat $ replicate 6000 testData

executor :: (Handle -> SerialT IO Char) -> IO (SerialT IO Char)
executor f =
    withSystemTempDirectory "fs_handle" $ \fp -> do
        let fpath = fp </> "tmp_read.txt"
        writeFile fpath testDataLarge
        h <- openFile fpath ReadMode
        return $ f h

readFromHandle :: IO (SerialT IO Char)
readFromHandle =
    let f = Unicode.decodeUtf8 . Stream.unfold Handle.read
    in executor f

readWithBufferFromHandle :: IO (SerialT IO Char)
readWithBufferFromHandle =
    let f1 = (\h -> (1024, h))
        f2 = Unicode.decodeUtf8 . Stream.unfold Handle.readWithBufferOf . f1
    in executor f2

readChunksFromHandle :: IO (SerialT IO Char)
readChunksFromHandle =
    let f =   Unicode.decodeUtf8
            . Stream.concatMap Array.toStream
            . Stream.unfold Handle.readChunks
    in executor f

readChunksWithBuffer :: IO (SerialT IO Char)
readChunksWithBuffer =
    let f1 = (\h -> (1024, h))
        f2 =
              Unicode.decodeUtf8
            . Stream.concatMap Array.toStream
            . Stream.unfold Handle.readChunksWithBufferOf
            . f1
    in executor f2

testRead :: (IsStream t) => IO (t IO Char) -> Property
testRead fn = monadicIO $ do
    let v2 = Stream.fromList testDataLarge
    v1 <- run fn
    res <- run $ Stream.eqBy (==) v1 v2
    assert res

testWrite :: (Handle -> Fold.Fold IO Word8 ()) -> Property
testWrite hfold =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len $ chooseWord8 (0, 255)) $ \list0 ->
            monadicIO $ do
                res <- run $ go list0
                assert res

                where

                go list =
                    withSystemTempDirectory "fs_handle" $ \fp -> do
                        let fpathWrite = fp </> "tmp_write.txt"
                        writeFile fpathWrite ""
                        h <- openFile fpathWrite ReadWriteMode
                        hSeek h AbsoluteSeek 0
                        _ <- Stream.fold (hfold h) $ Stream.fromList list
                        hFlush h
                        hSeek h AbsoluteSeek 0
                        ls <- Stream.toList $ Stream.unfold Handle.read h
                        hClose h
                        return (ls == list)

testWriteWithChunk :: Property
testWriteWithChunk =
    monadicIO $ do
        res <- run go
        assert res

        where

        go =
            withSystemTempDirectory "fs_handle" $ \fp -> do
                let fpathRead = fp </> "tmp_read.txt"
                    fpathWrite = fp </> "tmp_write.txt"
                writeFile fpathRead testDataLarge
                writeFile fpathWrite ""
                hr <- openFile fpathRead ReadMode
                hw <- openFile fpathWrite ReadWriteMode
                hSeek hw AbsoluteSeek 0
                _ <- Stream.fold (Handle.writeChunks hw)
                    $ Stream.unfold Handle.readChunksWithBufferOf (1024, hr)
                hFlush hw
                hSeek hw AbsoluteSeek 0
                ls <- Stream.toList $ Stream.unfold Handle.read hw
                let arr = Array.fromList ls
                return (testDataLarge == utf8ToString arr)

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
        describe "Read From Handle" $ do
            prop "read" $ testRead readFromHandle
            prop "readWithBufferOf" $ testRead readWithBufferFromHandle
            prop "readChunks" $ testRead readChunksFromHandle
            prop "readChunksWithBufferOf" $ testRead readChunksWithBuffer
        describe "Write To Handle" $ do
            prop "write" $ testWrite Handle.write
            prop "writeWithBufferOf" $ testWrite $ Handle.writeWithBufferOf 1024
            prop "writeChunks" testWriteWithChunk
