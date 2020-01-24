-- |
-- Module      : Streamly.Benchmark.FileIO.Array
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Streamly.Benchmark.FileIO.Array
    (
      last
    , countBytes
    , countLines
    , countWords
    , sumBytes
    , cat
    , catOnException
    , catBracket
    , catBracketIO
    , catBracketStream
    , catBracketStreamIO
    , copy
    , linesUnlinesCopy
    , wordsUnwordsCopy
    , decodeUtf8Lenient
    , copyCodecUtf8Lenient
    )
where

import Data.Functor.Identity (runIdentity)
import Data.Word (Word8)
import System.IO (Handle, hClose)
import Prelude hiding (last)

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Memory.Array as A
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Unicode.Stream as SS
import qualified Streamly.Internal.Data.Unicode.Stream as IUS

import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Internal.Memory.Array as IA
import qualified Streamly.Internal.Memory.ArrayStream as AS
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Internal.Prelude as IP

#ifdef INSPECTION
import Foreign.Storable (Storable)
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))
import Test.Inspection
#endif

-- | Get the last byte from a file bytestream.
{-# INLINE last #-}
last :: Handle -> IO (Maybe Word8)
last inh = do
    let s = IFH.toChunks inh
    larr <- S.last s
    return $ case larr of
        Nothing -> Nothing
        Just arr -> IA.readIndex arr (A.length arr - 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'last
inspect $ 'last `hasNoType` ''Step
#endif

-- | Count the number of bytes in a file.
{-# INLINE countBytes #-}
countBytes :: Handle -> IO Int
countBytes inh =
    let s = IFH.toChunks inh
    in S.sum (S.map A.length s)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'countBytes
inspect $ 'countBytes `hasNoType` ''Step
#endif

-- | Count the number of lines in a file.
{-# INLINE countLines #-}
countLines :: Handle -> IO Int
countLines = S.length . AS.splitOnSuffix 10 . IFH.toChunks

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'countLines
inspect $ 'countLines `hasNoType` ''Step
#endif

-- XXX use a word splitting combinator instead of splitOn and test it.
-- | Count the number of lines in a file.
{-# INLINE countWords #-}
countWords :: Handle -> IO Int
countWords = S.length . AS.splitOn 32 . IFH.toChunks

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'countWords
inspect $ 'countWords `hasNoType` ''Step
#endif

-- | Sum the bytes in a file.
{-# INLINE sumBytes #-}
sumBytes :: Handle -> IO Word8
sumBytes inh = do
    let foldlArr' f z = runIdentity . S.foldl' f z . IA.toStream
    let s = IFH.toChunks inh
    S.foldl' (\acc arr -> acc + foldlArr' (+) 0 arr) 0 s

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sumBytes
inspect $ 'sumBytes `hasNoType` ''Step
#endif

-- | Send the file contents to /dev/null
{-# INLINE cat #-}
cat :: Handle -> Handle -> IO ()
cat devNull inh =
    S.fold (IFH.writeChunks devNull) $ IFH.toChunksWithBufferOf (256*1024) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cat
inspect $ 'cat `hasNoType` ''Step
#endif

-- | Send the file contents to /dev/null with exception handling
{-# INLINE catBracket #-}
catBracket :: Handle -> Handle -> IO ()
catBracket devNull inh =
    let readEx = IUF.bracket return (\_ -> hClose inh)
                    (IUF.supplyFirst FH.readChunksWithBufferOf (256*1024))
    in IUF.fold readEx (IFH.writeChunks devNull) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'catBracket
-- inspect $ 'catBracket `hasNoType` ''Step
#endif

{-# INLINE catBracketIO #-}
catBracketIO :: Handle -> Handle -> IO ()
catBracketIO devNull inh =
    let readEx = IUF.bracketIO return (\_ -> hClose inh)
                    (IUF.supplyFirst FH.readChunksWithBufferOf (256*1024))
    in IUF.fold readEx (IFH.writeChunks devNull) inh

-- | Send the file contents to /dev/null with exception handling
{-# INLINE catBracketStream #-}
catBracketStream :: Handle -> Handle -> IO ()
catBracketStream devNull inh =
    let readEx = S.bracket (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.toChunksWithBufferOf (256*1024) inh)
    in S.fold (IFH.writeChunks devNull) $ readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'catBracketStream
-- inspect $ 'catBracketStream `hasNoType` ''Step
#endif

{-# INLINE catBracketStreamIO #-}
catBracketStreamIO :: Handle -> Handle -> IO ()
catBracketStreamIO devNull inh =
    let readEx = IP.bracketIO (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.toChunksWithBufferOf (256*1024) inh)
    in S.fold (IFH.writeChunks devNull) $ readEx

-- | Send the file contents to /dev/null with exception handling
{-# INLINE catOnException #-}
catOnException :: Handle -> Handle -> IO ()
catOnException devNull inh =
    let readEx = IUF.onException (\_ -> hClose inh)
                    (IUF.supplyFirst FH.readChunksWithBufferOf (256*1024))
    in IUF.fold readEx (IFH.writeChunks devNull) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'catOnException
-- inspect $ 'catOnException `hasNoType` ''Step
#endif

-- | Copy file
{-# INLINE copy #-}
copy :: Handle -> Handle -> IO ()
copy inh outh =
    let s = IFH.toChunks inh
    in S.fold (IFH.writeChunks outh) s

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copy
inspect $ 'copy `hasNoType` ''Step
#endif

-- | Lines and unlines
{-# INLINE linesUnlinesCopy #-}
linesUnlinesCopy :: Handle -> Handle -> IO ()
linesUnlinesCopy inh outh =
    S.fold (IFH.writeWithBufferOf (1024*1024) outh)
        $ AS.interposeSuffix 10
        $ AS.splitOnSuffix 10
        $ IFH.toChunksWithBufferOf (1024*1024) inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'linesUnlinesCopy [''Storable]
-- inspect $ 'linesUnlinesCopy `hasNoType` ''Step
#endif

-- | Words and unwords
{-# INLINE wordsUnwordsCopy #-}
wordsUnwordsCopy :: Handle -> Handle -> IO ()
wordsUnwordsCopy inh outh =
    S.fold (IFH.writeWithBufferOf (1024*1024) outh)
        $ AS.interpose 32
        -- XXX this is not correct word splitting combinator
        $ AS.splitOn 32
        $ IFH.toChunksWithBufferOf (1024*1024) inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'wordsUnwordsCopy [''Storable]
-- inspect $ 'wordsUnwordsCopy `hasNoType` ''Step
#endif

{-# INLINE decodeUtf8Lenient #-}
decodeUtf8Lenient :: Handle -> IO ()
decodeUtf8Lenient inh =
   S.drain
     $ IUS.decodeUtf8ArraysLenient
     $ IFH.toChunksWithBufferOf (1024*1024) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'decodeUtf8Lenient
-- inspect $ 'decodeUtf8Lenient `hasNoType` ''Step
-- inspect $ 'decodeUtf8Lenient `hasNoType` ''AT.FlattenState
-- inspect $ 'decodeUtf8Lenient `hasNoType` ''D.ConcatMapUState
#endif

-- | Copy file
{-# INLINE copyCodecUtf8Lenient #-}
copyCodecUtf8Lenient :: Handle -> Handle -> IO ()
copyCodecUtf8Lenient inh outh =
   S.fold (FH.write outh)
     $ SS.encodeUtf8
     $ IUS.decodeUtf8ArraysLenient
     $ IFH.toChunksWithBufferOf (1024*1024) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyCodecUtf8Lenient
-- inspect $ 'copyCodecUtf8Lenient `hasNoType` ''Step
-- inspect $ 'copyCodecUtf8Lenient `hasNoType` ''AT.FlattenState
-- inspect $ 'copyCodecUtf8Lenient `hasNoType` ''D.ConcatMapUState
#endif
