import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
-- import qualified Streamly.Memory.Array as A
import qualified Streamly.FileSystem.Handle as FH
import qualified System.IO as FH
-- import qualified Streamly.FileSystem.FD as FH
-- import qualified Streamly.Data.Unicode.Stream as US

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Memory.ArrayStream as AS
import qualified Streamly.Internal.FileSystem.Handle as IFH

import Data.Char (ord)
import System.Environment (getArgs)
import System.IO (IOMode(..), hSeek, SeekMode(..))

cat :: FH.Handle -> IO ()
cat src =
      S.fold (FH.writeChunks FH.stdout)
    $ IFH.toChunksWithBufferOf (256*1024) src
-- byte stream version
-- cat src = S.fold (FH.write FH.stdout) $ FH.read src

cp :: FH.Handle -> FH.Handle -> IO ()
cp src dst =
      S.fold (FH.writeChunks dst)
    $ IFH.toChunksWithBufferOf (256*1024) src
-- byte stream version
-- cp src dst = S.fold (FH.write dst) $ FH.read src

ord' :: Num a => Char -> a
ord' = (fromIntegral . ord)

wcl :: FH.Handle -> IO ()
wcl src = print =<< (S.length
    $ AS.splitOn 10
    $ IFH.toChunks src)
{-
-- Char stream version
wcl src = print =<< (S.length
    $ flip US.lines FL.drain
    $ US.decodeLatin1
    $ FH.read src)
-}

{-
grepc :: String -> FH.Handle -> IO ()
grepc pat src = print . (subtract 1) =<< (S.length
    $ FL.splitOnSeq (A.fromList (map ord' pat)) FL.drain
    $ FH.read src)
-}

avgll :: FH.Handle -> IO ()
avgll src = print =<< (S.fold avg
    $ S.splitWithSuffix (== ord' '\n') FL.length
    $ S.unfold FH.read src)
    where avg = (/) <$> toDouble FL.sum <*> toDouble FL.length
          toDouble = fmap (fromIntegral :: Int -> Double)

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
 -- rewind >> putStr "grepc "   >> grepc "aaaa" src -- Unix grep -c program
    rewind >> putStr "avgll "   >> avgll src        -- get average line length
    rewind >> putStr "llhisto " >> llhisto src      -- get line length histogram

    dst <- FH.openFile "dst-xyz.txt" WriteMode
    rewind >> putStr "cp " >> cp src dst       -- Unix cp program
