-------------------------------------------------------------------------------
-- Fast, streaming and parallel word counting (wc) program.
-------------------------------------------------------------------------------
-- 1) On utf8 inputs the serial version is around 3x faster than MacOS wc
-- 2) It can run parallely on multiple cores providing further speedup
-- 3) Parallel version works efficiently on stdin/streaming input as well
-- 4) Parallel version handles utf8 input correctly (including multi-byte space
--    chars) and gives the same output as the serial version on all inputs.
-- 5) There may be differences in word/char counts when there are invalid utf8
--    byte sequences present in the input because of different styles of error
--    handling.

-------------------------------------------------------------------------------
-- Build with the following options:
-------------------------------------------------------------------------------
-- streamly optimization plugin is required for best performance
-- ghc -O2 -fplugin Plugin -fspec-constr-recursive=10 -fmax-worker-args=16
-- For concurrent version add: -threaded -with-rtsopts "-N"

-------------------------------------------------------------------------------
-- Comparing with "wc -mwl" command:
-------------------------------------------------------------------------------
--
-- 1) To enable UTF8 with wc: export LANG=en_US.UTF-8; export LC_ALL=$LANG
-- 2) To test whether it is acutally using utf8, copy and paste this string
-- "U+1680 U+2000 U+2001 U+2002" and run "wc -mwl" on this. Without proper UTF8
-- handling word count would be 1, with proper UTF8 handling word count would
-- be 4. Note that the spaces in this string are not regular space chars they
-- are different unicode space chars.

{-# LANGUAGE CPP #-}

import Control.Monad (when)
import Data.Char (isSpace)
import Data.Word (Word8)
import GHC.Conc (numCapabilities)
import System.Environment (getArgs)
import System.IO (Handle, openFile, IOMode(..))
import Streamly.Internal.Data.Unicode.Stream
       (DecodeState, DecodeError(..), CodePoint, decodeUtf8Either,
       resumeDecodeUtf8Either)

import qualified Streamly as S
import qualified Streamly.Data.Unicode.Stream as S
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Memory.Array as A
import qualified Streamly.Prelude as S
import qualified Data.Vector.Storable.Mutable as V

-------------------------------------------------------------------------------
-- Parallel char, line and word counting
-------------------------------------------------------------------------------

-- We process individual chunks in the stream independently and parallely and
-- the combine the chunks to combine what they have counted.
--
-------------------------------------------------------------------------------
-- Char counting
-------------------------------------------------------------------------------

-- To count chars each block needs the following:
--
-- -- | header | char counts | trailer |
--
-- header and trailer are incomplete utf8 byte sequences that may be combined
-- with the previous or the next block to complete them later.
--
-- The trailer may have one or more bytes in a valid utf8 sequence and is
-- expecting more bytes to complete the sequence. The header stores any
-- possible continuation from the previous block. It contains a maximum of 3
-- bytes which all must be non-starter bytes.
--
-- When two blocks are combined, the trailer of the first block is combined
-- with the header of the next block and then utf8 decoded. The combined
-- header+trailer may yield:
--
-- * Nothing - when there is no trailer and header
-- * All errors - when there is no trailer in the previous block, and there is
-- a header in the next block. In this case there is no starting char which
-- means all header bytes are errors.
-- * It can yield at most one valid character followed by 0, 1 or 2 errors.
--
-- We count an incomplete utf8 sequence of 2 or more bytes starting with a
-- valid starter byte as a single codepoint. Bytes not following a valid
-- starter byte are treated as individual codepoints for counting.
--
-------------------------------------------------------------------------------
-- Word counting
-------------------------------------------------------------------------------

-- For word counting we need the following in each block:
--
-- -- | header | startsWithSpace | word counts | endsWithSpace | trailer |
--
-- The word counts in individual blocks are performed assuming that the
-- previous char before the block is a space.
-- When combining two blocks, after combining the trailer of previous blocks
-- with the header of the next we determine if the resulting char is a space or
-- not.
--
-- 1) If there is no new char joining the two blocks then we use endsWithSpace
-- of the previous block and startsWithSpace of the next block to determine if
-- the word counts are to be adjusted. If the previous block ends with
-- non-space and the next block starts with non-space we need to decrement the
-- word count by one if it is non-zero in the next block.
--
-- 2) If the new joining char is a space then we combine it with
-- startsWithSpace and endsWithSpace to determine the
-- startsWithSpace/endsWithSpace of the combined block and adjust the word
-- counts appropriately.
--
-------------------------------------------------------------------------------
-- Line counting
-------------------------------------------------------------------------------

-- Line counting is performed by counting "\n" in the stream. No new "\n" can
-- result from patching the trailer and header as it is always a single byte.

-------------------------------------------------------------------------------
-- Counting state
-------------------------------------------------------------------------------

-- We use a mutable vector for the counting state. A fold using an immutable
-- structure for such a large state does not perform well.  However, mutability
-- is confined to just the accumulator.
--
-- XXX we need convenient mutable records (like C structs) to handle things
-- like this. It may be possible to achieve the same performance with an
-- immutable accumulator, but that will require more research. Since we are
-- always discarding the previous state, we can perhaps make use of that memory
-- using safe in-place modifications, without having to allocate new memory.

-- XXX we can also count the number of decoding errors separately
data Field =
    -- The number of "\n" characters found in the block.
      LineCount
    -- Number of full words found in the block, words are counted on a
    -- transition from space char to a non-space char. We always assume the
    -- char before the first starter char in a block is a space. If this is
    -- found to be incorrect when joining two blocks then we fix the counts
    -- accordingly.
    | WordCount
    -- The number of successfully decoded characters plus the number of
    -- decoding failures in the block. Each byte or sequence of bytes on which
    -- decoding fails is also counted as one char. The header and trailer bytes
    -- are not accounted in this, they are accounted only when we join two
    -- blocks.
    | CharCount
    -- whether the last counted char in this block was a space char
    | WasSpace
    -- whether the first successfully decoded char in this block is a space. A
    -- decoding failure, after the trailing bytes from the previous block are
    -- accounted, is also considered as space.
    | FirstIsSpace
    -- If no starter byte is found in the first three bytes in the block then
    -- store those bytes to possibly combine them with the trailing incomplete
    -- byte sequence in the previous block. We mark it done when either we have
    -- stored three bytes or we have found a starter byte.
    --
    -- XXX This is ugly to manipulate, we can implement a statically max sized
    -- mutable ring structure within this record.
    | HeaderDone
    | HeaderWordCount
    | HeaderWord1
    | HeaderWord2
    | HeaderWord3
    -- If a byte sequence at the end of the block is not complete then store
    -- the current state of the utf8 decoder to continue it later using the
    -- incomplete leading byte sequence in the next block.
    | TrailerPresent
    | TrailerState
    | TrailerCodePoint
    deriving (Show, Enum, Bounded)

-------------------------------------------------------------------------------
-- Default/initial state of the block
-------------------------------------------------------------------------------

readField :: V.IOVector Int -> Field -> IO Int
readField v fld = V.read v (fromEnum fld)

writeField :: V.IOVector Int -> Field -> Int -> IO ()
writeField v fld val = V.write v (fromEnum fld) val

modifyField :: V.IOVector Int -> Field -> (Int -> Int) -> IO ()
modifyField v fld f = V.modify v f (fromEnum fld)

newCounts :: IO (V.IOVector Int)
newCounts = do
    counts <- V.new (fromEnum (maxBound :: Field) + 1)
    writeField counts LineCount 0
    writeField counts WordCount 0
    writeField counts CharCount 0
    writeField counts WasSpace 1
    writeField counts FirstIsSpace 0
    writeField counts HeaderDone 0
    writeField counts HeaderWordCount 0
    writeField counts TrailerPresent 0
    return counts

-------------------------------------------------------------------------------
-- Counting chars
-------------------------------------------------------------------------------

accountChar :: V.IOVector Int -> Bool -> IO ()
accountChar counts isSp = do
    c <- readField counts CharCount
    let space = if isSp then 1 else 0
    when (c == 0) $ writeField counts FirstIsSpace space
    writeField counts CharCount (c + 1)
    writeField counts WasSpace space

-------------------------------------------------------------------------------
-- Manipulating the header bytes
-------------------------------------------------------------------------------

addToHeader :: V.IOVector Int -> Int -> IO Bool
addToHeader counts cp = do
    cnt <- readField counts HeaderWordCount
    case cnt of
        0 -> do
            writeField counts HeaderWord1 cp
            writeField counts HeaderWordCount 1
            return True
        1 -> do
            writeField counts HeaderWord2 cp
            writeField counts HeaderWordCount 2
            return True
        2 -> do
            writeField counts HeaderWord3 cp
            writeField counts HeaderWordCount 3
            writeField counts HeaderDone 1
            return True
        _ -> return False

resetHeaderOnNewChar :: V.IOVector Int -> IO ()
resetHeaderOnNewChar counts = do
    hdone <- readField counts HeaderDone
    when (hdone == 0) $ writeField counts HeaderDone 1

-------------------------------------------------------------------------------
-- Manipulating the trailer
-------------------------------------------------------------------------------

setTrailer :: V.IOVector Int -> DecodeState -> CodePoint -> IO ()
setTrailer counts st cp = do
    writeField counts TrailerState (fromIntegral st)
    writeField counts TrailerCodePoint cp
    writeField counts TrailerPresent 1

resetTrailerOnNewChar :: V.IOVector Int -> IO ()
resetTrailerOnNewChar counts = do
    trailer <- readField counts TrailerPresent
    when (trailer /= 0) $ do
        writeField counts TrailerPresent 0
        accountChar counts True

-------------------------------------------------------------------------------
-- Counting the stream
-------------------------------------------------------------------------------

{-# INLINE countChar #-}
countChar :: V.IOVector Int -> Either DecodeError Char -> IO ()
countChar counts inp =
    case inp of
        Right ch -> do
            resetHeaderOnNewChar counts
            -- account the last stored error as whitespace and clear it
            resetTrailerOnNewChar counts

            when (ch == '\n') $ modifyField counts LineCount (+ 1)
            if isSpace ch
            then accountChar counts True
            else do
                wasSpace <- readField counts WasSpace
                when (wasSpace /= 0) $ modifyField counts WordCount (+ 1)
                accountChar counts False
        Left (DecodeError st cp) -> do
            hdone <- readField counts HeaderDone
            if hdone == 0
            then do
                if st == 0
                then do
                    -- We got a non-starter in initial decoder state, there may
                    -- be something that comes before this to complete it.
                    r <- addToHeader counts cp
                    when (not r) $ error "countChar: Bug addToHeader failed"
                else do
                    -- We got an error in a non-initial decoder state, it may
                    -- be an input underflow error, keep it as incomplete in
                    -- the trailer.
                    writeField counts HeaderDone 1
                    setTrailer counts st cp
            else do
                    resetTrailerOnNewChar counts
                    if st == 0
                    then accountChar counts True
                    else setTrailer counts st cp

printCounts :: V.IOVector Int -> IO ()
printCounts v = do
    l <- readField v LineCount
    w <- readField v WordCount
    c <- readField v CharCount
    putStrLn $ show l ++ " " ++ show w ++  " " ++ show c

-------------------------------------------------------------------------------
-- Serial counting using parallel version of countChar
-------------------------------------------------------------------------------

_wc_mwl_parserial :: Handle -> IO (V.IOVector Int)
_wc_mwl_parserial src = do
    counts <- newCounts
    S.mapM_ (countChar counts)
        $ decodeUtf8Either
        $ S.unfold FH.read src
    return counts

-------------------------------------------------------------------------------
-- Serial word counting with UTF-8 handling
-------------------------------------------------------------------------------

data Counts = Counts !Int !Int !Int !Bool deriving Show

{-# INLINE countCharSerial #-}
countCharSerial :: Counts -> Char -> Counts
countCharSerial (Counts l w c wasSpace) ch =
    let l1 = if (ch == '\n') then l + 1 else l
        (w1, wasSpace1) =
            if (isSpace ch)
            then (w, True)
            else (if wasSpace then w + 1 else w, False)
    in (Counts l1 w1 (c + 1) wasSpace1)

-- Note: This counts invalid byte sequences are non-space chars
_wc_mwl_serial :: Handle -> IO ()
_wc_mwl_serial src = print =<< (
      S.foldl' countCharSerial (Counts 0 0 0 True)
    $ S.decodeUtf8Lax
    $ S.unfold FH.read src)

-------------------------------------------------------------------------------
-- Parallel counting
-------------------------------------------------------------------------------

-- XXX we need a better data structure to store the header bytes to make these
-- routines simpler.
--
-- combine trailing bytes in preceding block with leading bytes in the next
-- block and decode them into a codepoint
reconstructChar :: Int
                -> V.IOVector Int
                -> V.IOVector Int
                -> IO (S.SerialT IO (Either DecodeError Char))
reconstructChar hdrCnt v1 v2 = do
    when (hdrCnt > 3 || hdrCnt < 0) $ error "reconstructChar: hdrCnt > 3"
    stream1 <-
        if (hdrCnt > 2)
        then do
            x <- readField v2 HeaderWord3
            return $ (fromIntegral x :: Word8) `S.cons` S.nil
        else return S.nil
    stream2 <-
        if (hdrCnt > 1)
        then do
            x <- readField v2 HeaderWord2
            return $ fromIntegral x `S.cons` stream1
        else return stream1
    stream3 <-
        if (hdrCnt > 0)
        then do
            x <- readField v2 HeaderWord1
            return $ fromIntegral x `S.cons` stream2
        else return stream2

    state <- readField v1 TrailerState
    cp <- readField v1 TrailerCodePoint
    return $ resumeDecodeUtf8Either (fromIntegral state) cp stream3

getHdrChar :: V.IOVector Int -> IO (Maybe Int)
getHdrChar v = do
    hdrCnt <- readField v HeaderWordCount
    case hdrCnt of
        0 -> return Nothing
        1 -> do
            writeField v HeaderWordCount 0
            fmap Just $ readField v HeaderWord1
        2 -> do
            x1 <- readField v HeaderWord1
            x2 <- readField v HeaderWord2
            writeField v HeaderWord1 x2
            writeField v HeaderWordCount 1
            return $ Just x1
        3 -> do
            x1 <- readField v HeaderWord1
            x2 <- readField v HeaderWord2
            x3 <- readField v HeaderWord3
            writeField v HeaderWord1 x2
            writeField v HeaderWord2 x3
            writeField v HeaderWordCount 2
            return $ Just x1
        _ -> error "getHdrChar: Bug, hdrCnt not in range 0-3"

-- If the header of the first block is not done then combine the header
-- with the header of the next block.
combineHeaders :: V.IOVector Int -> V.IOVector Int -> IO ()
combineHeaders v1 v2 = do
    hdone1 <- readField v1 HeaderDone
    if hdone1 == 0
    then do
        res <- getHdrChar v2
        case res of
            Nothing -> return ()
            Just x -> do
                r <- addToHeader v1 x
                when (not r) $ error "combineHeaders: Bug, addToHeader failed"
    else return ()

-- We combine the contents of the second vector into the first vector, mutating
-- the first vector and returning it.
-- XXX This is a quick hack and can be refactored to reduce the size
-- and understandability considerably.
addCounts :: V.IOVector Int -> V.IOVector Int -> IO (V.IOVector Int)
addCounts v1 v2 = do
    hdone1 <- readField v1 HeaderDone
    hdone2 <- readField v2 HeaderDone
    hdrCnt2_0 <- readField v2 HeaderWordCount
    if hdone1 == 0 && (hdrCnt2_0 /= 0 || hdone2 /= 0)
    then do
        combineHeaders v1 v2
        if hdone2 == 0
        then return v1
        else do
            writeField v1 HeaderDone 1
            addCounts v1 v2
    else do
        trailerPresent2 <- readField v2 TrailerPresent
        trailerState2 <- readField v2 TrailerState
        trailerCodePoint2 <- readField v2 TrailerCodePoint
        if hdone1 == 0
        then error "addCounts: Bug, trying to add completely empty second block"
        else do
            l1 <- readField v1 LineCount
            w1 <- readField v1 WordCount
            c1 <- readField v1 CharCount
            wasSpace1 <- readField v1 WasSpace

            l2 <- readField v2 LineCount
            w2 <- readField v2 WordCount
            c2 <- readField v2 CharCount
            wasSpace2 <- readField v2 WasSpace
            firstIsSpace2 <- readField v2 FirstIsSpace
            hdrCnt2 <- readField v2 HeaderWordCount

            trailer1 <- readField v1 TrailerPresent
            if trailer1 == 0 -- no trailer in the first block
            then do
                -- header2, if any, complete or incomplete, is just invalid
                -- bytes, count them as whitespace
                let firstIsSpace2' = firstIsSpace2 /= 0 || hdrCnt2 /= 0
                w <-
                        if w2 /= 0 && wasSpace1 == 0 && not firstIsSpace2'
                        then return $ w1 + w2 - 1
                        else return $ w1 + w2
                writeField v1 LineCount (l1 + l2)
                writeField v1 WordCount w
                writeField v1 CharCount (c1 + c2 + hdrCnt2)

                when (c1 == 0) $ do
                    if c2 == 0 && hdrCnt2 /= 0
                    then writeField v1 FirstIsSpace 1
                    else writeField v1 FirstIsSpace firstIsSpace2

                if c2 == 0 && hdrCnt2 /= 0
                then writeField v1 WasSpace 1
                else writeField v1 WasSpace wasSpace2

                writeField v1 TrailerPresent trailerPresent2
                writeField v1 TrailerState trailerState2
                writeField v1 TrailerCodePoint trailerCodePoint2
                return v1
            else do
                if hdrCnt2 == 0
                then do
                    when (hdone2 /= 0) $ do -- empty and Done header
                        -- count trailer as whitespace, its counted as one char
                        -- Note: hdrCnt2 == 0 means either header is not done
                        -- or c2 /= 0
                        writeField v1 LineCount (l1 + l2)
                        writeField v1 WordCount (w1 + w2)
                        writeField v1 CharCount (c1 + c2 + 1)

                        when (c1 == 0) $ writeField v1 FirstIsSpace 1
                        if (c2 == 0)
                        then writeField v1 WasSpace 1
                        else writeField v1 WasSpace wasSpace2

                        writeField v1 TrailerPresent trailerPresent2
                        writeField v1 TrailerState trailerState2
                        writeField v1 TrailerCodePoint trailerCodePoint2
                    -- If header of the next block is not done we continue the
                    -- trailer from the previous block instead of treating it
                    -- as whitespace
                    return v1
                else do
                    -- join the trailing part of the first block with the
                    -- header of the next
                    decoded <- reconstructChar hdrCnt2 v1 v2
                    res <- S.uncons decoded
                    case res of
                        Nothing -> error "addCounts: Bug. empty reconstructed char"
                        Just (h, t) -> do
                            tlength <- S.length t
                            case h of
                                Right ch -> do
                                    -- If we have an error case after this
                                    -- char then that would be treated
                                    -- as whitespace
                                    let lcount = l1 + l2
                                        lcount1 = if (ch == '\n') then lcount + 1 else lcount
                                        wcount = w1 + w2
                                        firstSpace = isSpace ch
                                        wasSpace = firstSpace || tlength /= 0
                                        wcount1 =
                                            if wasSpace
                                            then wcount
                                            else if w2 == 0 || firstIsSpace2 /= 0
                                                 then wcount
                                                 else wcount - 1
                                    writeField v1 LineCount lcount1
                                    writeField v1 WordCount wcount1
                                    writeField v1 CharCount (c1 + c2 + 1 + tlength)

                                    when (c1 == 0) $
                                        writeField v1 FirstIsSpace
                                                   (if firstSpace then 1 else 0)

                                    if c2 == 0
                                    then do
                                        if wasSpace
                                        then writeField v1 WasSpace 1
                                        else writeField v1 WasSpace 0
                                    else writeField v1 WasSpace wasSpace2

                                    writeField v1 TrailerPresent trailerPresent2
                                    writeField v1 TrailerState trailerState2
                                    writeField v1 TrailerCodePoint trailerCodePoint2
                                    return v1
                                Left (DecodeError st cp) -> do
                                    -- if header was incomplete it may result
                                    -- in partially decoded char to be written
                                    -- as trailer. Check if the last error is
                                    -- an incomplete decode.
                                    r <- S.last t
                                    let (st', cp') =
                                            case r of
                                                Nothing -> (st, cp)
                                                Just lst -> case lst of
                                                    Right _ -> error "addCounts: Bug"
                                                    Left (DecodeError st1 cp1) -> (st1, cp1)
                                    if hdone2 == 0 && st' /= 12
                                    then do
                                        -- all elements before the last one must be errors
                                        writeField v1 CharCount (c1 + tlength)

                                        when (c1 == 0) $
                                            writeField v1 FirstIsSpace 1

                                        writeField v1 WasSpace 1

                                        writeField v1 TrailerState (fromIntegral st')
                                        writeField v1 TrailerCodePoint cp'
                                    else do
                                        -- all elements must be errors
                                        -- treat them as whitespace
                                        writeField v1 LineCount (l1 + l2)
                                        writeField v1 WordCount (w1 + w2)
                                        writeField v1 CharCount (c1 + c2 + tlength + 1)

                                        when (c1 == 0) $
                                            writeField v1 FirstIsSpace 1

                                        if c2 == 0
                                        then writeField v1 WasSpace 1
                                        else writeField v1 WasSpace wasSpace2

                                        writeField v1 TrailerPresent trailerPresent2
                                        writeField v1 TrailerState trailerState2
                                        writeField v1 TrailerCodePoint trailerCodePoint2
                                    return v1

-- Individual array processing is an isolated loop, fusing it with the bigger
-- loop may be counter productive.
{-# NOINLINE countArray #-}
countArray :: A.Array Word8 -> IO (V.IOVector Int)
countArray src = do
    counts <- newCounts
    S.mapM_ (countChar counts)
        $ decodeUtf8Either
        $ S.unfold A.read src
    return counts

{-# INLINE wc_mwl_parallel #-}
wc_mwl_parallel :: Handle -> Int -> IO (V.IOVector Int)
wc_mwl_parallel src n = do
    counts <- newCounts
    S.foldlM' addCounts counts
        $ S.aheadly
        $ S.maxThreads numCapabilities
        $ S.mapM (countArray)
        $ S.unfold FH.readChunksWithBufferOf (n, src)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    name <- fmap head getArgs
    src <- openFile name ReadMode
    -- _wc_mwl_serial src -- Unix wc -l program
    -- printCounts =<< _wc_mwl_parserial src -- Unix wc -l program
    -- Using different sizes of chunks (1,2,3,4,5,10,128,256) is a good testing
    -- mechanism for parallel counting code.
    {-
    args <- getArgs
    let chunkSize = read $ args !! 1
    -}
    let chunkSize = 32 * 1024
    printCounts =<< wc_mwl_parallel src chunkSize  -- Unix wc -l program
