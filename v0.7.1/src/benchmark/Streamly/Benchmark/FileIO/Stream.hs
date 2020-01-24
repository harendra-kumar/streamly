-- |
-- Module      : Streamly.Benchmark.FileIO.Stream
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Streamly.Benchmark.FileIO.Stream
    (
    -- * FileIO
      last
    , countBytes
    , countLines
    , countLinesU
    , countWords
    , sumBytes
    , cat
    , catStreamWrite
    , catBracket
    , catBracketIO
    , catBracketStream
    , catBracketStreamIO
    , catOnException
    , catOnExceptionStream
    , catHandle
    , catHandleStream
    , catFinally
    , catFinallyIO
    , catFinallyStream
    , catFinallyStreamIO
    , copy
    , linesUnlinesCopy
    , linesUnlinesArrayWord8Copy
    , linesUnlinesArrayCharCopy
    -- , linesUnlinesArrayUtf8Copy
    , wordsUnwordsCopyWord8
    , wordsUnwordsCopy
    , wordsUnwordsCharArrayCopy
    , readWord8
    , decodeLatin1
    , copyCodecChar8
    , copyCodecUtf8
    , decodeUtf8Lax
    , copyCodecUtf8Lenient
    , chunksOfSum
    , chunksOf
    , chunksOfD
    , splitOn
    , splitOnSuffix
    , wordsBy
    , splitOnSeq
    , splitOnSeqUtf8
    , splitOnSuffixSeq
    )
where

import Control.Exception (SomeException)
import Data.Char (ord, chr)
import Data.Word (Word8)
import System.IO (Handle, hClose)
import Prelude hiding (last, length)

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Memory.Array as A
-- import qualified Streamly.Internal.Memory.Array as IA
import qualified Streamly.Internal.Memory.Array.Types as AT
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
-- import qualified Streamly.Internal.Data.Fold as IFL
import qualified Streamly.Data.Unicode.Stream as SS
import qualified Streamly.Internal.Data.Unicode.Stream as IUS
import qualified Streamly.Internal.Memory.Unicode.Array as IUA
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Internal.Data.Stream.StreamD as D

#ifdef INSPECTION
import Foreign.Storable (Storable)
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..), GroupState)
import Test.Inspection
#endif

-- | Get the last byte from a file bytestream.
{-# INLINE last #-}
last :: Handle -> IO (Maybe Word8)
last = S.last . S.unfold FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'last
inspect $ 'last `hasNoType` ''Step
inspect $ 'last `hasNoType` ''AT.FlattenState
inspect $ 'last `hasNoType` ''D.ConcatMapUState
#endif

-- assert that flattenArrays constructors are not present
-- | Count the number of bytes in a file.
{-# INLINE countBytes #-}
countBytes :: Handle -> IO Int
countBytes = S.length . S.unfold FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'countBytes
inspect $ 'countBytes `hasNoType` ''Step
inspect $ 'countBytes `hasNoType` ''AT.FlattenState
inspect $ 'countBytes `hasNoType` ''D.ConcatMapUState
#endif

-- | Count the number of lines in a file.
{-# INLINE countLines #-}
countLines :: Handle -> IO Int
countLines =
    S.length
        . IUS.lines FL.drain
        . SS.decodeLatin1
        . S.unfold FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'countLines
inspect $ 'countLines `hasNoType` ''Step
inspect $ 'countLines `hasNoType` ''AT.FlattenState
inspect $ 'countLines `hasNoType` ''D.ConcatMapUState
#endif

-- | Count the number of words in a file.
{-# INLINE countWords #-}
countWords :: Handle -> IO Int
countWords =
    S.length
        . IUS.words FL.drain
        . SS.decodeLatin1
        . S.unfold FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'countWords
-- inspect $ 'countWords `hasNoType` ''Step
-- inspect $ 'countWords `hasNoType` ''D.ConcatMapUState
#endif

-- | Count the number of lines in a file.
{-# INLINE countLinesU #-}
countLinesU :: Handle -> IO Int
countLinesU inh =
    S.length
        $ IUS.lines FL.drain
        $ SS.decodeLatin1
        $ S.concatUnfold A.read (IFH.toChunks inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'countLinesU
inspect $ 'countLinesU `hasNoType` ''Step
inspect $ 'countLinesU `hasNoType` ''D.ConcatMapUState
#endif

-- | Sum the bytes in a file.
{-# INLINE sumBytes #-}
sumBytes :: Handle -> IO Word8
sumBytes = S.sum . S.unfold FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sumBytes
inspect $ 'sumBytes `hasNoType` ''Step
inspect $ 'sumBytes `hasNoType` ''AT.FlattenState
inspect $ 'sumBytes `hasNoType` ''D.ConcatMapUState
#endif

-- | Send the file contents to /dev/null
{-# INLINE cat #-}
cat :: Handle -> Handle -> IO ()
cat devNull inh = S.fold (FH.write devNull) $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cat
inspect $ 'cat `hasNoType` ''Step
inspect $ 'cat `hasNoType` ''AT.FlattenState
inspect $ 'cat `hasNoType` ''D.ConcatMapUState
#endif

-- | Send the file contents to /dev/null
{-# INLINE catStreamWrite #-}
catStreamWrite :: Handle -> Handle -> IO ()
catStreamWrite devNull inh = IFH.fromBytes devNull $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'catStreamWrite
inspect $ 'catStreamWrite `hasNoType` ''Step
inspect $ 'catStreamWrite `hasNoType` ''AT.FlattenState
inspect $ 'catStreamWrite `hasNoType` ''D.ConcatMapUState
#endif

-- | Send the file contents to /dev/null with exception handling
{-# INLINE catBracket #-}
catBracket :: Handle -> Handle -> IO ()
catBracket devNull inh =
    let readEx = IUF.bracket return (\_ -> hClose inh) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'catBracket
-- inspect $ 'catBracket `hasNoType` ''Step
-- inspect $ 'catBracket `hasNoType` ''AT.FlattenState
-- inspect $ 'catBracket `hasNoType` ''D.ConcatMapUState
#endif

{-# INLINE catBracketIO #-}
catBracketIO :: Handle -> Handle -> IO ()
catBracketIO devNull inh =
    let readEx = IUF.bracketIO return (\_ -> hClose inh) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

-- | Send the file contents to /dev/null with exception handling
{-# INLINE catBracketStream #-}
catBracketStream :: Handle -> Handle -> IO ()
catBracketStream devNull inh =
    let readEx = S.bracket (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.toBytes inh)
    in IFH.fromBytes devNull $ readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'catBracketStream
-- inspect $ 'catBracketStream `hasNoType` ''Step
#endif

{-# INLINE catBracketStreamIO #-}
catBracketStreamIO :: Handle -> Handle -> IO ()
catBracketStreamIO devNull inh =
    let readEx = IP.bracketIO (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.toBytes inh)
    in IFH.fromBytes devNull $ readEx

-- | Send the file contents to /dev/null with exception handling
{-# INLINE catOnException #-}
catOnException :: Handle -> Handle -> IO ()
catOnException devNull inh =
    let readEx = IUF.onException (\_ -> hClose inh) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'catOnException
-- inspect $ 'catOnException `hasNoType` ''Step
-- inspect $ 'catOnException `hasNoType` ''AT.FlattenState
-- inspect $ 'catOnException `hasNoType` ''D.ConcatMapUState
#endif

-- | Send the file contents to /dev/null with exception handling
{-# INLINE catOnExceptionStream #-}
catOnExceptionStream :: Handle -> Handle -> IO ()
catOnExceptionStream devNull inh =
    let readEx = S.onException (hClose inh) (S.unfold FH.read inh)
    in S.fold (FH.write devNull) $ readEx

-- | Send the file contents to /dev/null with exception handling
{-# INLINE catFinally #-}
catFinally :: Handle -> Handle -> IO ()
catFinally devNull inh =
    let readEx = IUF.finally (\_ -> hClose inh) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'catFinally
-- inspect $ 'catFinally `hasNoType` ''Step
-- inspect $ 'catFinally `hasNoType` ''AT.FlattenState
-- inspect $ 'catFinally `hasNoType` ''D.ConcatMapUState
#endif

{-# INLINE catFinallyIO #-}
catFinallyIO :: Handle -> Handle -> IO ()
catFinallyIO devNull inh =
    let readEx = IUF.finallyIO (\_ -> hClose inh) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

-- | Send the file contents to /dev/null with exception handling
{-# INLINE catFinallyStream #-}
catFinallyStream :: Handle -> Handle -> IO ()
catFinallyStream devNull inh =
    let readEx = S.finally (hClose inh) (S.unfold FH.read inh)
    in S.fold (FH.write devNull) readEx

{-# INLINE catFinallyStreamIO #-}
catFinallyStreamIO :: Handle -> Handle -> IO ()
catFinallyStreamIO devNull inh =
    let readEx = IP.finallyIO (hClose inh) (S.unfold FH.read inh)
    in S.fold (FH.write devNull) readEx

-- | Send the file contents to /dev/null with exception handling
{-# INLINE catHandle #-}
catHandle :: Handle -> Handle -> IO ()
catHandle devNull inh =
    let handler (_e :: SomeException) = hClose inh >> return 10
        readEx = IUF.handle (IUF.singleton handler) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'catHandle
-- inspect $ 'catHandle `hasNoType` ''Step
-- inspect $ 'catHandle `hasNoType` ''AT.FlattenState
-- inspect $ 'catHandle `hasNoType` ''D.ConcatMapUState
#endif

-- | Send the file contents to /dev/null with exception handling
{-# INLINE catHandleStream #-}
catHandleStream :: Handle -> Handle -> IO ()
catHandleStream devNull inh =
    let handler (_e :: SomeException) = S.yieldM (hClose inh >> return 10)
        readEx = S.handle handler (S.unfold FH.read inh)
    in S.fold (FH.write devNull) $ readEx

-- | Copy file
{-# INLINE copy #-}
copy :: Handle -> Handle -> IO ()
copy inh outh = S.fold (FH.write outh) (S.unfold FH.read inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copy
inspect $ 'copy `hasNoType` ''Step
inspect $ 'copy `hasNoType` ''AT.FlattenState
inspect $ 'copy `hasNoType` ''D.ConcatMapUState
#endif

{-# INLINE readWord8 #-}
readWord8 :: Handle -> IO ()
readWord8 inh = S.drain $ S.unfold FH.read inh

{-# INLINE decodeLatin1 #-}
decodeLatin1 :: Handle -> IO ()
decodeLatin1 inh =
   S.drain
     $ SS.decodeLatin1
     $ S.unfold FH.read inh

-- | Copy file
{-# INLINE copyCodecChar8 #-}
copyCodecChar8 :: Handle -> Handle -> IO ()
copyCodecChar8 inh outh =
   S.fold (FH.write outh)
     $ SS.encodeLatin1
     $ SS.decodeLatin1
     $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyCodecChar8
inspect $ 'copyCodecChar8 `hasNoType` ''Step
inspect $ 'copyCodecChar8 `hasNoType` ''AT.FlattenState
inspect $ 'copyCodecChar8 `hasNoType` ''D.ConcatMapUState
#endif

{-# INLINE decodeUtf8Lax #-}
decodeUtf8Lax :: Handle -> IO ()
decodeUtf8Lax inh =
   S.drain
     $ SS.decodeUtf8Lax
     $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'decodeUtf8Lax
-- inspect $ 'decodeUtf8Lax `hasNoType` ''Step
-- inspect $ 'decodeUtf8Lax `hasNoType` ''AT.FlattenState
-- inspect $ 'decodeUtf8Lax `hasNoType` ''D.ConcatMapUState
#endif

-- | Copy file
{-# INLINE copyCodecUtf8 #-}
copyCodecUtf8 :: Handle -> Handle -> IO ()
copyCodecUtf8 inh outh =
   S.fold (FH.write outh)
     $ SS.encodeUtf8
     $ SS.decodeUtf8
     $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyCodecUtf8
-- inspect $ 'copyCodecUtf8 `hasNoType` ''Step
-- inspect $ 'copyCodecUtf8 `hasNoType` ''AT.FlattenState
-- inspect $ 'copyCodecUtf8 `hasNoType` ''D.ConcatMapUState
#endif

-- | Copy file
{-# INLINE copyCodecUtf8Lenient #-}
copyCodecUtf8Lenient :: Handle -> Handle -> IO ()
copyCodecUtf8Lenient inh outh =
   S.fold (FH.write outh)
     $ SS.encodeUtf8
     $ SS.decodeUtf8Lax
     $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyCodecUtf8Lenient
-- inspect $ 'copyCodecUtf8Lenient `hasNoType` ''Step
-- inspect $ 'copyCodecUtf8Lenient `hasNoType` ''AT.FlattenState
-- inspect $ 'copyCodecUtf8Lenient `hasNoType` ''D.ConcatMapUState
#endif

{-# INLINE chunksOfSum #-}
chunksOfSum :: Int -> Handle -> IO Int
chunksOfSum n inh = S.length $ S.chunksOf n FL.sum (S.unfold FH.read inh)

-- | Slice in chunks of size n and get the count of chunks.
{-# INLINE chunksOf #-}
chunksOf :: Int -> Handle -> IO Int
chunksOf n inh =
    -- writeNUnsafe gives 2.5x boost here over writeN.
    S.length $ S.chunksOf n (AT.writeNUnsafe n) (S.unfold FH.read inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'chunksOf
inspect $ 'chunksOf `hasNoType` ''Step
inspect $ 'chunksOf `hasNoType` ''AT.FlattenState
inspect $ 'chunksOf `hasNoType` ''D.ConcatMapUState
inspect $ 'chunksOf `hasNoType` ''GroupState
#endif

-- This is to make sure that the concatMap in FH.read, groupsOf and foldlM'
-- together can fuse.
--
-- | Slice in chunks of size n and get the count of chunks.
{-# INLINE chunksOfD #-}
chunksOfD :: Int -> Handle -> IO Int
chunksOfD n inh =
    D.foldlM' (\i _ -> return $ i + 1) 0
        $ D.groupsOf n (AT.writeNUnsafe n)
        $ D.fromStreamK (S.unfold FH.read inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'chunksOf
inspect $ 'chunksOf `hasNoType` ''Step
inspect $ 'chunksOfD `hasNoType` ''GroupState
inspect $ 'chunksOfD `hasNoType` ''AT.FlattenState
inspect $ 'chunksOfD `hasNoType` ''D.ConcatMapUState
#endif

{-# INLINE linesUnlinesCopy #-}
linesUnlinesCopy :: Handle -> Handle -> IO ()
linesUnlinesCopy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeLatin1
      $ IUS.unlines IUF.fromList
      $ S.splitOnSuffix (== '\n') FL.toList
      $ SS.decodeLatin1
      $ S.unfold FH.read inh

{-# INLINE linesUnlinesArrayWord8Copy #-}
linesUnlinesArrayWord8Copy :: Handle -> Handle -> IO ()
linesUnlinesArrayWord8Copy inh outh =
    S.fold (FH.write outh)
      $ IP.interposeSuffix 10 A.read
      $ S.splitOnSuffix (== 10) A.write
      $ S.unfold FH.read inh

-- XXX splitSuffixOn requires -funfolding-use-threshold=150 for better fusion
-- | Lines and unlines
{-# INLINE linesUnlinesArrayCharCopy #-}
linesUnlinesArrayCharCopy :: Handle -> Handle -> IO ()
linesUnlinesArrayCharCopy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeLatin1
      $ IUA.unlines
      $ IUA.lines
      $ SS.decodeLatin1
      $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'linesUnlinesArrayCharCopy [''Storable]
-- inspect $ 'linesUnlinesArrayCharCopy `hasNoType` ''Step
-- inspect $ 'linesUnlinesArrayCharCopy `hasNoType` ''AT.FlattenState
-- inspect $ 'linesUnlinesArrayCharCopy `hasNoType` ''D.ConcatMapUState
#endif

-- XXX to write this we need to be able to map decodeUtf8 on the A.read fold.
-- For that we have to write decodeUtf8 as a Pipe.
{-
{-# INLINE linesUnlinesArrayUtf8Copy #-}
linesUnlinesArrayUtf8Copy :: Handle -> Handle -> IO ()
linesUnlinesArrayUtf8Copy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeLatin1
      $ IP.intercalate (A.fromList [10]) (pipe SS.decodeUtf8P A.read)
      $ S.splitOnSuffix (== '\n') (IFL.lmap SS.encodeUtf8 A.write)
      $ SS.decodeLatin1
      $ S.unfold FH.read inh
-}

foreign import ccall unsafe "u_iswspace"
  iswspace :: Int -> Int

-- Code copied from base/Data.Char to INLINE it
{-# INLINE isSpace #-}
isSpace                 :: Char -> Bool
-- isSpace includes non-breaking space
-- The magic 0x377 isn't really that magical. As of 2014, all the codepoints
-- at or below 0x377 have been assigned, so we shouldn't have to worry about
-- any new spaces appearing below there. It would probably be best to
-- use branchless ||, but currently the eqLit transformation will undo that,
-- so we'll do it like this until there's a way around that.
isSpace c
  | uc <= 0x377 = uc == 32 || uc - 0x9 <= 4 || uc == 0xa0
  | otherwise = iswspace (ord c) /= 0
  where
    uc = fromIntegral (ord c) :: Word

{-# INLINE isSp #-}
isSp :: Word8 -> Bool
isSp = isSpace . chr . fromIntegral

-- | Word, unwords and copy
{-# INLINE wordsUnwordsCopyWord8 #-}
wordsUnwordsCopyWord8 :: Handle -> Handle -> IO ()
wordsUnwordsCopyWord8 inh outh =
    S.fold (FH.write outh)
        $ IP.interposeSuffix 32 IUF.fromList
        $ S.wordsBy isSp FL.toList
        $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'wordsUnwordsCopyWord8
-- inspect $ 'wordsUnwordsCopyWord8 `hasNoType` ''Step
-- inspect $ 'wordsUnwordsCopyWord8 `hasNoType` ''D.ConcatMapUState
#endif

-- | Word, unwords and copy
{-# INLINE wordsUnwordsCopy #-}
wordsUnwordsCopy :: Handle -> Handle -> IO ()
wordsUnwordsCopy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeLatin1
      $ IUS.unwords IUF.fromList
      -- XXX This pipeline does not fuse with wordsBy but fuses with splitOn
      -- with -funfolding-use-threshold=300.  With wordsBy it does not fuse
      -- even with high limits for inlining and spec-constr ghc options. With
      -- -funfolding-use-threshold=400 it performs pretty well and there
      -- is no evidence in the core that a join point involving Step
      -- constructors is not getting inlined. Not being able to fuse at all in
      -- this case could be an unknown issue, need more investigation.
      $ S.wordsBy isSpace FL.toList
      -- -- $ S.splitOn isSpace FL.toList
      $ SS.decodeLatin1
      $ S.unfold FH.read inh

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'wordsUnwordsCopy
-- inspect $ 'wordsUnwordsCopy `hasNoType` ''Step
-- inspect $ 'wordsUnwordsCopy `hasNoType` ''AT.FlattenState
-- inspect $ 'wordsUnwordsCopy `hasNoType` ''D.ConcatMapUState
#endif

{-# INLINE wordsUnwordsCharArrayCopy #-}
wordsUnwordsCharArrayCopy :: Handle -> Handle -> IO ()
wordsUnwordsCharArrayCopy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeLatin1
      $ IUA.unwords
      $ IUA.words
      $ SS.decodeLatin1
      $ S.unfold FH.read inh

lf :: Word8
lf = fromIntegral (ord '\n')

toarr :: String -> A.Array Word8
toarr = A.fromList . map (fromIntegral . ord)

-- | Split on line feed.
{-# INLINE splitOn #-}
splitOn :: Handle -> IO Int
splitOn inh =
    (S.length $ S.splitOn (== lf) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitOn
inspect $ 'splitOn `hasNoType` ''Step
inspect $ 'splitOn `hasNoType` ''AT.FlattenState
inspect $ 'splitOn `hasNoType` ''D.ConcatMapUState
#endif

-- | Split suffix on line feed.
{-# INLINE splitOnSuffix #-}
splitOnSuffix :: Handle -> IO Int
splitOnSuffix inh =
    (S.length $ S.splitOnSuffix (== lf) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitOnSuffix
inspect $ 'splitOnSuffix `hasNoType` ''Step
inspect $ 'splitOnSuffix `hasNoType` ''AT.FlattenState
inspect $ 'splitOnSuffix `hasNoType` ''D.ConcatMapUState
#endif

-- | Words by space
{-# INLINE wordsBy #-}
wordsBy :: Handle -> IO Int
wordsBy inh =
    (S.length $ S.wordsBy isSp FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'wordsBy
inspect $ 'wordsBy `hasNoType` ''Step
inspect $ 'wordsBy `hasNoType` ''AT.FlattenState
inspect $ 'wordsBy `hasNoType` ''D.ConcatMapUState
#endif

-- | Split on a word8 sequence.
{-# INLINE splitOnSeq #-}
splitOnSeq :: String -> Handle -> IO Int
splitOnSeq str inh =
    (S.length $ IP.splitOnSeq (toarr str) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'splitOnSeq
-- inspect $ 'splitOnSeq `hasNoType` ''Step
-- inspect $ 'splitOnSeq `hasNoType` ''AT.FlattenState
-- inspect $ 'splitOnSeq `hasNoType` ''D.ConcatMapUState
#endif

-- | Split on a character sequence.
{-# INLINE splitOnSeqUtf8 #-}
splitOnSeqUtf8 :: String -> Handle -> IO Int
splitOnSeqUtf8 str inh =
    (S.length $ IP.splitOnSeq (A.fromList str) FL.drain
        $ IUS.decodeUtf8ArraysLenient
        $ IFH.toChunks inh) -- >>= print

-- | Split on suffix sequence.
{-# INLINE splitOnSuffixSeq #-}
splitOnSuffixSeq :: String -> Handle -> IO Int
splitOnSuffixSeq str inh =
    (S.length $ IP.splitOnSuffixSeq (toarr str) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'splitOnSuffixSeq
-- inspect $ 'splitOnSuffixSeq `hasNoType` ''Step
-- inspect $ 'splitOnSuffixSeq `hasNoType` ''AT.FlattenState
-- inspect $ 'splitOnSuffixSeq `hasNoType` ''D.ConcatMapUState
#endif
