import Data.Char (ord)
import System.Environment (getArgs)
import System.IO (IOMode(..), hSeek, SeekMode(..))

import qualified Streamly.Data.Fold as FL
import qualified Streamly.FileSystem.Handle as FH
import qualified System.IO as FH
import qualified Streamly.Memory.Array as A
import qualified Streamly.Prelude as S
-- import qualified Streamly.FileSystem.FD as FH

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unicode.Stream as US
import qualified Streamly.Internal.Memory.ArrayStream as AS
import qualified Streamly.Internal.Prelude as S

-- Read the contents of a file to stdout.
--
-- FH.read reads the file in 32KB chunks and converts the chunks into a byte
-- stream. FH.write takes the byte stream as input, converts it into chunks of
-- 32KB and writes those chunks to stdout.
--
_cat :: FH.Handle -> IO ()
_cat src = S.fold (FH.write FH.stdout) $ S.unfold FH.read src

-- Chunked version, more efficient than the byte stream version above. Reads
-- the file in 256KB chunks and writes those chunks to stdout.
cat :: FH.Handle -> IO ()
cat src =
      S.fold (FH.writeChunks FH.stdout)
    $ S.unfold FH.readChunksWithBufferOf ((256*1024), src)

-- Copy a source file to a destination file.
--
-- FH.read reads the file in 32KB chunks and converts the chunks into a byte
-- stream. FH.write takes the byte stream as input, converts it into chunks of
-- 32KB and writes those chunks to the destination file.
_cp :: FH.Handle -> FH.Handle -> IO ()
_cp src dst = S.fold (FH.write dst) $ S.unfold FH.read src

-- Chunked version, more efficient than the byte stream version above. Reads
-- the file in 256KB chunks and writes those chunks to stdout.
cp :: FH.Handle -> FH.Handle -> IO ()
cp src dst =
      S.fold (FH.writeChunks dst)
    $ S.unfold FH.readChunksWithBufferOf ((256*1024), src)

ord' :: Num a => Char -> a
ord' = (fromIntegral . ord)

-- Count lines like wc -l.
--
-- Char stream version. Reads the input as a byte stream, splits it into lines
-- and counts the lines..
_wcl :: FH.Handle -> IO ()
_wcl src = print =<< (S.length
    $ US.lines FL.drain
    $ US.decodeLatin1
    $ S.unfold FH.read src)

-- More efficient chunked version. Reads chunks from the input handles and
-- splits the chunks directly instead of converting them into byte stream
-- first.
wcl :: FH.Handle -> IO ()
wcl src = print =<< (S.length
    $ AS.splitOn 10
    $ S.unfold FH.readChunks src)

-- grep -c
--
-- count the occurrences of a pattern in a file.
grepc :: String -> FH.Handle -> IO ()
grepc pat src = print . (subtract 1) =<< (S.length
    $ S.splitOnSeq (A.fromList (map ord' pat)) FL.drain
    $ S.unfold FH.read src)

-- Compute the average line length in a file.
avgll :: FH.Handle -> IO ()
avgll src = print =<< (S.fold avg
    $ S.splitWithSuffix (== ord' '\n') FL.length
    $ S.unfold FH.read src)
    where avg = (/) <$> toDouble FL.sum <*> toDouble FL.length
          toDouble = fmap (fromIntegral :: Int -> Double)

-- histogram of line lengths in a file
llhisto :: FH.Handle -> IO ()
llhisto src = print =<< (S.fold (FL.classify FL.length)
    $ S.map bucket
    $ S.splitWithSuffix (== ord' '\n') FL.length
    $ S.unfold FH.read src)
    where
    bucket n = let i = n `mod` 10 in if i > 9 then (9,n) else (i,n)

main :: IO ()
main = do
    name <- fmap head getArgs
    src <- FH.openFile name ReadMode
    let rewind = hSeek src AbsoluteSeek 0

    rewind >> putStrLn "cat"    >> cat src          -- Unix cat program
    rewind >> putStr "wcl "     >> wcl src          -- Unix wc -l program
    rewind >> putStr "grepc "   >> grepc "aaaa" src -- Unix grep -c program
    rewind >> putStr "avgll "   >> avgll src        -- get average line length
    rewind >> putStr "llhisto " >> llhisto src      -- get line length histogram

    dst <- FH.openFile "dst-xyz.txt" WriteMode
    rewind >> putStr "cp " >> cp src dst       -- Unix cp program
