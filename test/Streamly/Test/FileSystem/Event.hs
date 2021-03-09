-- |
-- Module      : Streamly.Test.FileSystem.Event
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Main (main) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad.IO.Class (MonadIO)
#if !defined(CABAL_OS_WINDOWS)
import Data.Char (ord)
#endif
import Data.Maybe (fromJust)
import Data.Word (Word8)
import System.Directory
    ( createDirectoryIfMissing
    , removeFile
    , removeDirectory
    , removePathForcibly
    , renameDirectory
    , renamePath
    )
import System.FilePath ((</>))
import System.IO (BufferMode(..), hSetBuffering, stdout)
import System.IO.Temp (withSystemTempDirectory)
#if !defined(CABAL_OS_WINDOWS)
import System.IO.Unsafe (unsafePerformIO)
#endif
import Streamly.Internal.Data.Array.Foreign (Array)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream.IsStream as S
#if defined(CABAL_OS_DARWIN)
import qualified Streamly.Internal.FileSystem.Event.Darwin as Event
#elif defined(CABAL_OS_LINUX)
import qualified Streamly.Internal.FileSystem.Event.Linux as Event
#elif defined(CABAL_OS_WINDOWS)
import qualified Streamly.Internal.FileSystem.Event.Windows as Event
#else
#error "FS Events not supported on this platform
#endif

#if !defined(CABAL_OS_WINDOWS)
import Data.Functor.Identity (runIdentity)
import qualified Streamly.Internal.Unicode.Stream as U
#endif

import Test.Hspec

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

toUtf8 :: MonadIO m => String -> m (Array Word8)
toUtf8 = Array.fromStream . Unicode.encodeUtf8' . S.fromList

#if !defined(CABAL_OS_WINDOWS)
utf8ToString :: Array Word8 -> String
utf8ToString = runIdentity . S.toList . U.decodeUtf8' . Array.toStream
#endif

timeout :: IO String
timeout = threadDelay 5000000 >> return "Timeout"

fseventDir :: String
fseventDir = "fsevent_dir"

-- XXX Make the getRelPath type same on windows and other platforms
eventPredicate :: Event.Event -> Bool
eventPredicate ev =
#if defined(CABAL_OS_WINDOWS)
    if (Event.getRelPath ev) == "EOTask"
#else
    if (utf8ToString $ Event.getRelPath ev) == "EOTask"
#endif
    then False
    else True

-------------------------------------------------------------------------------
-- Event matching utilities
-------------------------------------------------------------------------------
#if defined(CABAL_OS_LINUX)
removeTrailingSlash :: Array Word8 -> Array Word8
removeTrailingSlash path =
    if Array.length path == 0
    then path
    else
        let mx = Array.readIndex path (Array.length path - 1)
         in case mx of
            Nothing -> error "removeTrailingSlash: Bug: Invalid index"
            Just x ->
                if x == fromIntegral (ord '/')
                -- XXX need array slicing
                then unsafePerformIO
                        $ Array.fromStreamN (Array.length path - 1)
                        $ Array.toStream path
                else path
#endif

-- XXX Return a tuple (path, flags) instead of appending flags to path. And
-- then check the flags using an event mask.

showEventShort :: Event.Event -> String
#if defined(CABAL_OS_WINDOWS)
-- | Convert an 'Event' record to a short representation for unit test.
showEventShort ev@Event.Event{..} =
    Event.getRelPath ev ++ "_" ++ show eventFlags
#elif defined(CABAL_OS_LINUX)
showEventShort ev@Event.Event{..} =
    (utf8ToString $ removeTrailingSlash $ Event.getRelPath ev)
        ++ "_" ++ show eventFlags
        ++ showev Event.isDir "Dir"

    where showev f str = if f ev then "_" ++ str else ""
#else
#error "Unsupported OS
#endif

-------------------------------------------------------------------------------
-- Event Watcher
-------------------------------------------------------------------------------

checkEvents :: FilePath -> MVar () -> [String] -> IO String
checkEvents rootPath m matchList = do
    let args = [rootPath]
    paths <- mapM toUtf8 args
    putStrLn ("Watch started !!!! on Path " ++ rootPath)
    events <- S.parse (PR.takeWhile eventPredicate FL.toList)
        $ S.before (putMVar m ())
        $ Event.watchTrees (NonEmpty.fromList paths)
    let eventStr =  map showEventShort events
    let baseSet = Set.fromList matchList
        resultSet = Set.fromList eventStr
    if (baseSet `Set.isSubsetOf` resultSet)
    then
        return "PASS"
    else do
        putStrLn $ "baseSet " ++ show matchList
        putStrLn $ "resultSet " ++ show eventStr
        return "Mismatch"

-------------------------------------------------------------------------------
-- FS Event Generators
-------------------------------------------------------------------------------

checker :: S.IsStream t =>
                FilePath -> MVar () -> [String] -> t IO String
checker rootPath synch matchList =
    S.valueM (checkEvents rootPath synch matchList)
    `S.parallelFst`
    S.valueM timeout

-------------------------------------------------------------------------------
-- Test Drivers
-------------------------------------------------------------------------------

driver ::
       ( String
       , FilePath -> IO ()
       , FilePath -> IO ()
       , [String]
       )
    -> SpecWith ()
driver (desc, pre, ops, events) = it desc $ runTest `shouldReturn` "PASS"

    where

    runTest = do
        sync <- newEmptyMVar
        withSystemTempDirectory fseventDir $ \fp -> do
            pre fp
            let eventStream = checker fp sync events
                fsOps = S.valueM $ runFSOps fp sync
            fmap fromJust $ S.head $ eventStream `S.parallelFst` fsOps

    runFSOps fp sync = do
        _ <- takeMVar sync
        threadDelay 200000
        ops fp
        threadDelay 200000 -- Why this delay?
        createDirectoryIfMissing True (fp </> "EOTask")
        threadDelay 10000000
        error "fs ops timed out"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

testDesc ::
    [ ( String                       -- test description
      , FilePath -> IO ()            -- pre test operation
      , FilePath -> IO ()            -- file system actions
      , [String])                    -- expected events
    ]
testDesc =
    [
      ( "Create a single directory"
      , const (return ())
      , \fp -> createDirectoryIfMissing True (fp </> "dir1Single")
#if defined(CABAL_OS_WINDOWS)
      , [ "dir1Single_1" ]
#elif defined(CABAL_OS_LINUX)
      , [ "dir1Single_1073742080_Dir"
        , "dir1Single_1073741856_Dir"
        , "dir1Single_1073741825_Dir"
        , "dir1Single_1073741840_Dir"
        ]
#endif
      )
    , ( "Remove a single directory"
      , \fp -> createDirectoryIfMissing True (fp </> "dir1Single")
      , \fp -> removeDirectory (fp </> "dir1Single")
#if defined(CABAL_OS_WINDOWS)
      , [ "dir1Single_2" ]
#elif defined(CABAL_OS_LINUX)
      , [ "dir1Single_1024"
        , "dir1Single_32768"
        ]
#endif
      )
    , ( "Rename a single directory"
      , \fp -> createDirectoryIfMissing True (fp </> "dir1Single")
      , \fp ->
            let spath = fp </> "dir1Single"
                tpath = fp </> "dir1SingleRenamed"
            in renameDirectory spath tpath
#if defined(CABAL_OS_WINDOWS)
      , [ "dir1Single_4"
        , "dir1SingleRenamed_5"
        ]
#elif defined(CABAL_OS_LINUX)
      , [ "dir1Single_1073741888_Dir"
        , "dir1SingleRenamed_1073741952_Dir"
        ]
#endif
      )
    , ( "Create a nested directory"
      , const (return ())
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
#if defined(CABAL_OS_WINDOWS)
      , [ "dir1_1"
        , "dir1\\dir2_1"
        , "dir1\\dir2\\dir3_1"
        ]
#elif defined(CABAL_OS_LINUX)
      , [ "dir1_1073742080_Dir"
        , "dir1_1073741856_Dir"
        , "dir1_1073741825_Dir"
        , "dir1_1073741840_Dir"
        ]
#endif
      )
    , ( "Remove a nested directory"
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
      , \fp -> removePathForcibly (fp </> "dir1")
#if defined(CABAL_OS_WINDOWS)
      , [ "dir1_3"
        , "dir1\\dir2_3"
        , "dir1\\dir2\\dir3_2"
        , "dir1\\dir2_2","dir1_2"
        ]
#elif defined(CABAL_OS_LINUX)
      , [ "dir1/dir2/dir3_1073742336_Dir"
        , "dir1/dir2_1073742336_Dir"
        , "dir1_1073742336_Dir"
        ]
#endif
      )
    , ( "Rename a nested directory"
      , \fp -> createDirectoryIfMissing True
                (fp </> "dir1" </> "dir2" </> "dir3")
      , \fp ->
            let spath = fp </> "dir1" </> "dir2" </> "dir3"
                tpath = fp </> "dir1" </> "dir2" </> "dir3Renamed"
            in renameDirectory spath tpath
#if defined(CABAL_OS_WINDOWS)
      , [ "dir1\\dir2_3"
        , "dir1\\dir2\\dir3_4"
        , "dir1\\dir2\\dir3Renamed_5"
        , "dir1\\dir2_3"
        ]
#elif defined(CABAL_OS_LINUX)
      , [ "dir1/dir2/dir3_1073741888_Dir"
        , "dir1/dir2/dir3Renamed_1073741952_Dir"
        ]
#endif
      )
    , ( "Create a file in root Dir"
      , const (return ())
      , \fp -> writeFile (fp </> "FileCreated.txt") "Test Data"
#if defined(CABAL_OS_WINDOWS)
      , [ "FileCreated.txt_1"
        , "FileCreated.txt_3"
        , "FileCreated.txt_3"
        ]
#elif defined(CABAL_OS_LINUX)
      , [ "FileCreated.txt_256"
        , "FileCreated.txt_32"
        , "FileCreated.txt_2"
        ]
#endif
      )
    , ( "Remove a file in root Dir"
      , \fp -> writeFile (fp </> "FileCreated.txt") "Test Data"
      , \fp -> removeFile (fp </> "FileCreated.txt")
#if defined(CABAL_OS_WINDOWS)
      , [ "FileCreated.txt_2" ]
#elif defined(CABAL_OS_LINUX)
      , [ "FileCreated.txt_512" ]
#endif
      )
    , ( "Rename a file in root Dir"
      , \fp -> writeFile (fp </> "FileCreated.txt") "Test Data"
      , \fp ->
            let spath = (fp </> "FileCreated.txt")
                tpath = (fp </> "FileRenamed.txt")
            in renamePath spath tpath
#if defined(CABAL_OS_WINDOWS)
      , [ "FileCreated.txt_4"
        , "FileRenamed.txt_5"
        ]
#elif defined(CABAL_OS_LINUX)
      , [ "FileCreated.txt_64"
        , "FileRenamed.txt_128"
        ]
#endif
      )
    , ( "Create a file in a nested Dir"
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
      , \fp ->
            let p = fp </> "dir1" </> "dir2" </> "dir3" </> "FileCreated.txt"
            in writeFile p "Test Data"
#if defined(CABAL_OS_WINDOWS)
      , [ "dir1\\dir2\\dir3\\FileCreated.txt_1"
        , "dir1\\dir2\\dir3\\FileCreated.txt_3"
        ]
#elif defined(CABAL_OS_LINUX)
      , [ "dir1/dir2/dir3/FileCreated.txt_256"
        , "dir1/dir2/dir3/FileCreated.txt_32"
        , "dir1/dir2/dir3/FileCreated.txt_2"
        , "dir1/dir2/dir3/FileCreated.txt_8"
        ]
#endif
      )
    , ( "Remove a file in a nested Dir"
      , \fp ->
            let nestedDir = fp </> "dir1" </> "dir2" </> "dir3"
                fpath = nestedDir </> "FileCreated.txt"
            in do
                createDirectoryIfMissing True nestedDir
                writeFile fpath "Test Data"
      , \fp ->
            let p = fp </> "dir1" </> "dir2" </> "dir3" </> "FileCreated.txt"
            in removeFile p
#if defined(CABAL_OS_WINDOWS)
      , ["dir1\\dir2\\dir3\\FileCreated.txt_2"]
#elif defined(CABAL_OS_LINUX)
      , ["dir1/dir2/dir3/FileCreated.txt_512"]
#endif
      )
    , ( "Rename a file in a nested Dir"
      , \fp ->
            let nestedDir = fp </> "dir1" </> "dir2" </> "dir3"
                fpath = nestedDir </> "FileCreated.txt"
            in do
                createDirectoryIfMissing True nestedDir
                writeFile fpath "Test Data"
      , \fp ->
            let s = (fp </> "dir1" </> "dir2" </> "dir3" </> "FileCreated.txt")
                t = (fp </> "dir1" </> "dir2" </> "dir3" </> "FileRenamed.txt")
            in renamePath s t
#if defined(CABAL_OS_WINDOWS)
      , [ "dir1\\dir2\\dir3_3"
        , "dir1\\dir2\\dir3\\FileCreated.txt_4"
        , "dir1\\dir2\\dir3\\FileRenamed.txt_5"
        ]
#elif defined(CABAL_OS_LINUX)
      , [ "dir1/dir2/dir3/FileCreated.txt_64"
        , "dir1/dir2/dir3/FileRenamed.txt_128"
        ]
#endif
      )
    ]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hspec $ sequence_ $ map driver testDesc
