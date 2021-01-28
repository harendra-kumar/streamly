-- |
-- Module      : Streamly.Internal.Unicode.Stream
-- Copyright   : (c) 2018 Composewell Technologies
--               (c) Bjoern Hoehrmann 2008-2009
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

#include "inline.hs"

module Streamly.Internal.Unicode.Stream
    (
    -- * Construction (Decoding)
      decodeLatin1
    , decodeUtf8
    , decodeUtf8'
    , decodeUtf8_
    , DecodeError(..)
    , DecodeState
    , CodePoint
    , decodeUtf8Either
    , resumeDecodeUtf8Either
    , decodeUtf8Arrays
    , decodeUtf8Arrays'
    , decodeUtf8Arrays_

    -- * Elimination (Encoding)
    , encodeLatin1
    , encodeLatin1'
    , encodeLatin1_
    , encodeUtf8
    , encodeUtf8'
    , encodeUtf8_
    {-
    -- * Operations on character strings
    , strip -- (dropAround isSpace)
    , stripEnd
    -}

    -- * StreamD UTF8 Encoding / Decoding transformations.
    , decodeUtf8D
    , decodeUtf8D'
    , decodeUtf8D_
    , encodeUtf8D
    , encodeUtf8D'
    , encodeUtf8D_
    , decodeUtf8EitherD
    , resumeDecodeUtf8EitherD
    , decodeUtf8ArraysD
    , decodeUtf8ArraysD'
    , decodeUtf8ArraysD_

    -- * Transformation
    , stripStart
    , lines
    , words
    , unlines
    , unwords

    -- * Deprecations
    , decodeUtf8Lax
    , encodeLatin1Lax
    , encodeUtf8Lax
    )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.Char (chr, ord)
import Data.Word (Word8)
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Storable (Storable(..))
import Fusion.Plugin.Types (Fuse(..))
import GHC.Base (assert, unsafeChr)
import GHC.ForeignPtr (ForeignPtr (..))
import GHC.IO.Encoding.Failure (isSurrogate)
import GHC.Ptr (Ptr (..), plusPtr)
import System.IO.Unsafe (unsafePerformIO)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Array.Foreign (Array)
import Streamly.Internal.Data.Unfold (Unfold)
import Streamly.Internal.Data.SVar (adaptState)
import Streamly.Internal.Data.Stream.IsStream (IsStream)
import Streamly.Internal.Data.Stream.StreamD (Stream(..), Step (..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Streamly.Internal.Data.Array.Foreign.Type as A
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Stream.StreamD as D

import Prelude hiding (String, lines, words, unlines, unwords)

-- $setup
-- >>> :m
-- >>> import Prelude hiding (String, lines, words, unlines, unwords)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import Streamly.Internal.Unicode.Stream

-------------------------------------------------------------------------------
-- Encoding/Decoding Unicode (UTF-8) Characters
-------------------------------------------------------------------------------

-- UTF-8 primitives, Lifted from GHC.IO.Encoding.UTF8.

data WList = WCons !Word8 !WList | WNil

{-# INLINE ord2 #-}
ord2 :: Char -> WList
ord2 c = assert (n >= 0x80 && n <= 0x07ff) (WCons x1 (WCons x2 WNil))
  where
    n = ord c
    x1 = fromIntegral $ (n `shiftR` 6) + 0xC0
    x2 = fromIntegral $ (n .&. 0x3F) + 0x80

{-# INLINE ord3 #-}
ord3 :: Char -> WList
ord3 c = assert (n >= 0x0800 && n <= 0xffff) (WCons x1 (WCons x2 (WCons x3 WNil)))
  where
    n = ord c
    x1 = fromIntegral $ (n `shiftR` 12) + 0xE0
    x2 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
    x3 = fromIntegral $ (n .&. 0x3F) + 0x80

{-# INLINE ord4 #-}
ord4 :: Char -> WList
ord4 c = assert (n >= 0x10000)  (WCons x1 (WCons x2 (WCons x3 (WCons x4 WNil))))
  where
    n = ord c
    x1 = fromIntegral $ (n `shiftR` 18) + 0xF0
    x2 = fromIntegral $ ((n `shiftR` 12) .&. 0x3F) + 0x80
    x3 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
    x4 = fromIntegral $ (n .&. 0x3F) + 0x80

data CodingFailureMode
    = TransliterateCodingFailure
    | ErrorOnCodingFailure
    | DropOnCodingFailure
    deriving (Show)

{-# INLINE replacementChar #-}
replacementChar :: Char
replacementChar = '\xFFFD'

-- Int helps in cheaper conversion from Int to Char
type CodePoint = Int
type DecodeState = Word8

-- See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.

-- XXX Use names decodeSuccess = 0, decodeFailure = 12

decodeTable :: [Word8]
decodeTable = [
   -- The first part of the table maps bytes to character classes that
   -- to reduce the size of the transition table and create bitmasks.
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

   -- The second part is a transition table that maps a combination
   -- of a state of the automaton and a character class to a state.
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12
  ]

{-# NOINLINE utf8d #-}
utf8d :: A.Array Word8
utf8d =
      unsafePerformIO
    -- Aligning to cacheline makes a barely noticeable difference
    -- XXX currently alignment is not implemented for unmanaged allocation
    $ D.fold (A.writeNAlignedUnmanaged 64 (length decodeTable))
              (D.fromList decodeTable)

-- | Return element at the specified index without checking the bounds.
-- and without touching the foreign ptr.
{-# INLINE_NORMAL unsafePeekElemOff #-}
unsafePeekElemOff :: forall a. Storable a => Ptr a -> Int -> a
unsafePeekElemOff p i = let !x = A.unsafeInlineIO $ peekElemOff p i in x

-- decode is split into two separate cases to avoid branching instructions.
-- From the higher level flow we already know which case we are in so we can
-- call the appropriate decode function.
--
-- When the state is 0
{-# INLINE decode0 #-}
decode0 :: Ptr Word8 -> Word8 -> Tuple' DecodeState CodePoint
decode0 table byte =
    let !t = table `unsafePeekElemOff` fromIntegral byte
        !codep' = (0xff `shiftR` fromIntegral t) .&. fromIntegral byte
        !state' = table `unsafePeekElemOff` (256 + fromIntegral t)
     in assert ((byte > 0x7f || error showByte)
                && (state' /= 0 || error (showByte ++ showTable)))
               (Tuple' state' codep')

    where

    utf8table =
        let !(Ptr addr) = table
            end = table `plusPtr` 364
        in A.Array (ForeignPtr addr undefined) end :: A.Array Word8
    showByte = "Streamly: decode0: byte: " ++ show byte
    showTable = " table: " ++ show utf8table

-- When the state is not 0
{-# INLINE decode1 #-}
decode1
    :: Ptr Word8
    -> DecodeState
    -> CodePoint
    -> Word8
    -> Tuple' DecodeState CodePoint
decode1 table state codep byte =
    -- Remember codep is Int type!
    -- Can it be unsafe to convert the resulting Int to Char?
    let !t = table `unsafePeekElemOff` fromIntegral byte
        !codep' = (fromIntegral byte .&. 0x3f) .|. (codep `shiftL` 6)
        !state' = table `unsafePeekElemOff`
                    (256 + fromIntegral state + fromIntegral t)
     in assert (codep' <= 0x10FFFF
                    || error (showByte ++ showState state codep))
               (Tuple' state' codep')
    where

    utf8table =
        let !(Ptr addr) = table
            end = table `plusPtr` 364
        in A.Array (ForeignPtr addr undefined) end :: A.Array Word8
    showByte = "Streamly: decode1: byte: " ++ show byte
    showState st cp =
        " state: " ++ show st ++
        " codepoint: " ++ show cp ++
        " table: " ++ show utf8table

-- We can divide the errors in three general categories:
-- * A non-starter was encountered in a begin state
-- * A starter was encountered without completing a codepoint
-- * The last codepoint was not complete (input underflow)
--
-- Need to separate resumable and non-resumable error. In case of non-resumable
-- error we can also provide the failing byte. In case of resumable error the
-- state can be opaque.
--
data DecodeError = DecodeError !DecodeState !CodePoint deriving Show

{-# ANN type FreshPoint Fuse #-}
data FreshPoint s a
    = FreshPointDecodeInit s
    | FreshPointDecodeInit1 s Word8
    | FreshPointDecodeFirst s Word8
    | FreshPointDecoding s !DecodeState !CodePoint
    | YieldAndContinue a (FreshPoint s a)
    | Done

-- XXX write it as a parser and use parseMany to decode a stream, need to check
-- if that preserves the same performance. Or we can use a resumable parser
-- that parses a chunk at a time.
--
-- XXX Implement this in terms of decodeUtf8Either. Need to make sure that
-- decodeUtf8Either preserves the performance characterstics.
--
{-# INLINE_NORMAL decodeUtf8WithD #-}
decodeUtf8WithD :: Monad m
    => CodingFailureMode -> Stream m Word8 -> Stream m Char
decodeUtf8WithD cfm (Stream step state) =
    let A.Array p _ = utf8d
        !ptr = unsafeForeignPtrToPtr p
    in Stream (step' ptr) (FreshPointDecodeInit state)

    where

    prefix = "Streamly.Internal.Data.Stream.StreamD.decodeUtf8With: "

    {-# INLINE handleError #-}
    handleError e s =
        case cfm of
            ErrorOnCodingFailure -> error e
            TransliterateCodingFailure -> YieldAndContinue replacementChar s
            DropOnCodingFailure -> s

    {-# INLINE handleUnderflow #-}
    handleUnderflow =
        case cfm of
            ErrorOnCodingFailure -> error $ prefix ++ "Not enough input"
            TransliterateCodingFailure -> YieldAndContinue replacementChar Done
            DropOnCodingFailure -> Done

    {-# INLINE_LATE step' #-}
    step' _ gst (FreshPointDecodeInit st) = do
        r <- step (adaptState gst) st
        return $ case r of
            Yield x s -> Skip (FreshPointDecodeInit1 s x)
            Skip s -> Skip (FreshPointDecodeInit s)
            Stop   -> Skip Done

    step' _ _ (FreshPointDecodeInit1 st x) = do
        -- Note: It is important to use a ">" instead of a "<=" test
        -- here for GHC to generate code layout for default branch
        -- prediction for the common case. This is fragile and might
        -- change with the compiler versions, we need a more reliable
        -- "likely" primitive to control branch predication.
        case x > 0x7f of
            False ->
                return $ Skip $ YieldAndContinue
                    (unsafeChr (fromIntegral x))
                    (FreshPointDecodeInit st)
            -- Using a separate state here generates a jump to a
            -- separate code block in the core which seems to perform
            -- slightly better for the non-ascii case.
            True -> return $ Skip $ FreshPointDecodeFirst st x

    -- XXX should we merge it with FreshPointDecodeInit1?
    step' table _ (FreshPointDecodeFirst st x) = do
        let (Tuple' sv cp) = decode0 table x
        return $
            case sv of
                12 ->
                    let msg = prefix ++ "Invalid first UTF8 byte " ++ show x
                     in Skip $ handleError msg (FreshPointDecodeInit st)
                0 -> error "unreachable state"
                _ -> Skip (FreshPointDecoding st sv cp)

    -- We recover by trying the new byte x as a starter of a new codepoint.
    -- XXX need to use the same recovery in array decoding routine as well
    step' table gst (FreshPointDecoding st statePtr codepointPtr) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                let (Tuple' sv cp) = decode1 table statePtr codepointPtr x
                return $ case sv of
                    0 -> Skip $ YieldAndContinue
                            (unsafeChr cp) (FreshPointDecodeInit s)
                    12 ->
                        let msg = prefix
                                ++ "Invalid subsequent UTF8 byte "
                                ++ show x
                                ++ " in state "
                                ++ show statePtr
                                ++ " accumulated value "
                                ++ show codepointPtr
                         in Skip $ handleError msg (FreshPointDecodeInit1 s x)
                    _ -> Skip (FreshPointDecoding s sv cp)
            Skip s -> return $
                Skip (FreshPointDecoding s statePtr codepointPtr)
            Stop -> return $ Skip handleUnderflow

    step' _ _ (YieldAndContinue c s) = return $ Yield c s
    step' _ _ Done = return Stop

{-# INLINE decodeUtf8D #-}
decodeUtf8D :: Monad m => Stream m Word8 -> Stream m Char
decodeUtf8D = decodeUtf8WithD TransliterateCodingFailure

{-# INLINE decodeUtf8D' #-}
decodeUtf8D' :: Monad m => Stream m Word8 -> Stream m Char
decodeUtf8D' = decodeUtf8WithD ErrorOnCodingFailure

{-# INLINE decodeUtf8D_ #-}
decodeUtf8D_ :: Monad m => Stream m Word8 -> Stream m Char
decodeUtf8D_ = decodeUtf8WithD DropOnCodingFailure

{-# INLINE_NORMAL resumeDecodeUtf8EitherD #-}
resumeDecodeUtf8EitherD
    :: Monad m
    => DecodeState
    -> CodePoint
    -> Stream m Word8
    -> Stream m (Either DecodeError Char)
resumeDecodeUtf8EitherD dst codep (Stream step state) =
    let A.Array p _ = utf8d
        !ptr = unsafeForeignPtrToPtr p
        stt =
            if dst == 0
            then FreshPointDecodeInit state
            else FreshPointDecoding state dst codep
    in Stream (step' ptr) stt
  where
    {-# INLINE_LATE step' #-}
    step' _ gst (FreshPointDecodeInit st) = do
        r <- step (adaptState gst) st
        return $ case r of
            Yield x s -> Skip (FreshPointDecodeInit1 s x)
            Skip s -> Skip (FreshPointDecodeInit s)
            Stop   -> Skip Done

    step' _ _ (FreshPointDecodeInit1 st x) = do
        -- Note: It is important to use a ">" instead of a "<=" test
        -- here for GHC to generate code layout for default branch
        -- prediction for the common case. This is fragile and might
        -- change with the compiler versions, we need a more reliable
        -- "likely" primitive to control branch predication.
        case x > 0x7f of
            False ->
                return $ Skip $ YieldAndContinue
                    (Right $ unsafeChr (fromIntegral x))
                    (FreshPointDecodeInit st)
            -- Using a separate state here generates a jump to a
            -- separate code block in the core which seems to perform
            -- slightly better for the non-ascii case.
            True -> return $ Skip $ FreshPointDecodeFirst st x

    -- XXX should we merge it with FreshPointDecodeInit1?
    step' table _ (FreshPointDecodeFirst st x) = do
        let (Tuple' sv cp) = decode0 table x
        return $
            case sv of
                12 ->
                    Skip $ YieldAndContinue (Left $ DecodeError 0 (fromIntegral x))
                                            (FreshPointDecodeInit st)
                0 -> error "unreachable state"
                _ -> Skip (FreshPointDecoding st sv cp)

    -- We recover by trying the new byte x a starter of a new codepoint.
    -- XXX on error need to report the next byte "x" as well.
    -- XXX need to use the same recovery in array decoding routine as well
    step' table gst (FreshPointDecoding st statePtr codepointPtr) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                let (Tuple' sv cp) = decode1 table statePtr codepointPtr x
                return $
                    case sv of
                        0 -> Skip $ YieldAndContinue (Right $ unsafeChr cp)
                                        (FreshPointDecodeInit s)
                        12 ->
                            Skip $ YieldAndContinue (Left $ DecodeError statePtr codepointPtr)
                                        (FreshPointDecodeInit1 s x)
                        _ -> Skip (FreshPointDecoding s sv cp)
            Skip s -> return $ Skip (FreshPointDecoding s statePtr codepointPtr)
            Stop -> return $ Skip $ YieldAndContinue (Left $ DecodeError statePtr codepointPtr) Done

    step' _ _ (YieldAndContinue c s) = return $ Yield c s
    step' _ _ Done = return Stop

{-# INLINE_NORMAL decodeUtf8EitherD #-}
decodeUtf8EitherD :: Monad m
    => Stream m Word8 -> Stream m (Either DecodeError Char)
decodeUtf8EitherD = resumeDecodeUtf8EitherD 0 0

{-# ANN type FlattenState Fuse #-}
data FlattenState s a
    = OuterLoop s !(Maybe (DecodeState, CodePoint))
    | InnerLoopDecodeInit s (ForeignPtr a) !(Ptr a) !(Ptr a)
    | InnerLoopDecodeFirst s (ForeignPtr a) !(Ptr a) !(Ptr a) Word8
    | InnerLoopDecoding s (ForeignPtr a) !(Ptr a) !(Ptr a)
        !DecodeState !CodePoint
    | YAndC !Char (FlattenState s a) -- These constructors can be
                                     -- encoded in the FreshPoint
                                     -- type, I prefer to keep these
                                     -- flat even though that means
                                     -- coming up with new names
    | D

-- The normal decodeUtf8 above should fuse with flattenArrays
-- to create this exact code but it doesn't for some reason, as of now this
-- remains the fastest way I could figure out to decodeUtf8.
--
-- XXX Add Proper error messages
{-# INLINE_NORMAL decodeUtf8ArraysWithD #-}
decodeUtf8ArraysWithD ::
       MonadIO m
    => CodingFailureMode
    -> Stream m (A.Array Word8)
    -> Stream m Char
decodeUtf8ArraysWithD cfm (Stream step state) =
    let A.Array p _ = utf8d
        !ptr = unsafeForeignPtrToPtr p
    in Stream (step' ptr) (OuterLoop state Nothing)
  where
    {-# INLINE transliterateOrError #-}
    transliterateOrError e s =
        case cfm of
            ErrorOnCodingFailure -> error e
            TransliterateCodingFailure -> YAndC replacementChar s
            DropOnCodingFailure -> s
    {-# INLINE inputUnderflow #-}
    inputUnderflow =
        case cfm of
            ErrorOnCodingFailure ->
                error
                    "Streamly.Internal.Data.Stream.StreamD.decodeUtf8ArraysWith: Input Underflow"
            TransliterateCodingFailure -> YAndC replacementChar D
            DropOnCodingFailure -> D
    {-# INLINE_LATE step' #-}
    step' _ gst (OuterLoop st Nothing) = do
        r <- step (adaptState gst) st
        return $
            case r of
                Yield A.Array {..} s ->
                    let p = unsafeForeignPtrToPtr aStart
                     in Skip (InnerLoopDecodeInit s aStart p aEnd)
                Skip s -> Skip (OuterLoop s Nothing)
                Stop -> Skip D
    step' _ gst (OuterLoop st dst@(Just (ds, cp))) = do
        r <- step (adaptState gst) st
        return $
            case r of
                Yield A.Array {..} s ->
                    let p = unsafeForeignPtrToPtr aStart
                     in Skip (InnerLoopDecoding s aStart p aEnd ds cp)
                Skip s -> Skip (OuterLoop s dst)
                Stop -> Skip inputUnderflow
    step' _ _ (InnerLoopDecodeInit st startf p end)
        | p == end = do
            liftIO $ touchForeignPtr startf
            return $ Skip $ OuterLoop st Nothing
    step' _ _ (InnerLoopDecodeInit st startf p end) = do
        x <- liftIO $ peek p
        -- Note: It is important to use a ">" instead of a "<=" test here for
        -- GHC to generate code layout for default branch prediction for the
        -- common case. This is fragile and might change with the compiler
        -- versions, we need a more reliable "likely" primitive to control
        -- branch predication.
        case x > 0x7f of
            False ->
                return $ Skip $ YAndC
                    (unsafeChr (fromIntegral x))
                    (InnerLoopDecodeInit st startf (p `plusPtr` 1) end)
            -- Using a separate state here generates a jump to a separate code
            -- block in the core which seems to perform slightly better for the
            -- non-ascii case.
            True -> return $ Skip $ InnerLoopDecodeFirst st startf p end x

    step' table _ (InnerLoopDecodeFirst st startf p end x) = do
        let (Tuple' sv cp) = decode0 table x
        return $
            case sv of
                12 ->
                    Skip $
                    transliterateOrError
                        "Streamly.Internal.Data.Stream.StreamD.decodeUtf8ArraysWith: Invalid UTF8 codepoint encountered"
                        (InnerLoopDecodeInit st startf (p `plusPtr` 1) end)
                0 -> error "unreachable state"
                _ -> Skip (InnerLoopDecoding st startf (p `plusPtr` 1) end sv cp)
    step' _ _ (InnerLoopDecoding st startf p end sv cp)
        | p == end = do
            liftIO $ touchForeignPtr startf
            return $ Skip $ OuterLoop st (Just (sv, cp))
    step' table _ (InnerLoopDecoding st startf p end statePtr codepointPtr) = do
        x <- liftIO $ peek p
        let (Tuple' sv cp) = decode1 table statePtr codepointPtr x
        return $
            case sv of
                0 ->
                    Skip $
                    YAndC
                        (unsafeChr cp)
                        (InnerLoopDecodeInit st startf (p `plusPtr` 1) end)
                12 ->
                    Skip $
                    transliterateOrError
                        "Streamly.Internal.Data.Stream.StreamD.decodeUtf8ArraysWith: Invalid UTF8 codepoint encountered"
                        (InnerLoopDecodeInit st startf (p `plusPtr` 1) end)
                _ -> Skip (InnerLoopDecoding st startf (p `plusPtr` 1) end sv cp)
    step' _ _ (YAndC c s) = return $ Yield c s
    step' _ _ D = return Stop

{-# INLINE decodeUtf8ArraysD #-}
decodeUtf8ArraysD ::
       MonadIO m
    => Stream m (A.Array Word8)
    -> Stream m Char
decodeUtf8ArraysD = decodeUtf8ArraysWithD TransliterateCodingFailure

{-# INLINE decodeUtf8ArraysD' #-}
decodeUtf8ArraysD' ::
       MonadIO m
    => Stream m (A.Array Word8)
    -> Stream m Char
decodeUtf8ArraysD' = decodeUtf8ArraysWithD ErrorOnCodingFailure

{-# INLINE decodeUtf8ArraysD_ #-}
decodeUtf8ArraysD_ ::
       MonadIO m
    => Stream m (A.Array Word8)
    -> Stream m Char
decodeUtf8ArraysD_ = decodeUtf8ArraysWithD DropOnCodingFailure

{-# ANN type EncodeState Fuse #-}
data EncodeState s = EncodeState s !WList

-- More yield points improve performance, but I am not sure if they can cause
-- too much code bloat or some trouble with fusion. So keeping only two yield
-- points for now, one for the ascii chars (fast path) and one for all other
-- paths (slow path).
{-# INLINE_NORMAL encodeUtf8D' #-}
encodeUtf8D' :: Monad m => Stream m Char -> Stream m Word8
encodeUtf8D' (Stream step state) = Stream step' (EncodeState state WNil)
  where
    {-# INLINE_LATE step' #-}
    step' gst (EncodeState st WNil) = do
        r <- step (adaptState gst) st
        return $
            case r of
                Yield c s ->
                    case ord c of
                        x
                            | x <= 0x7F ->
                                Yield (fromIntegral x) (EncodeState s WNil)
                            | x <= 0x7FF -> Skip (EncodeState s (ord2 c))
                            | x <= 0xFFFF ->
                                if isSurrogate c
                                    then error
                                             "Streamly.Internal.Data.Stream.StreamD.encodeUtf8: Encountered a surrogate"
                                    else Skip (EncodeState s (ord3 c))
                            | otherwise -> Skip (EncodeState s (ord4 c))
                Skip s -> Skip (EncodeState s WNil)
                Stop -> Stop
    step' _ (EncodeState s (WCons x xs)) = return $ Yield x (EncodeState s xs)


-- | Decode a stream of bytes to Unicode characters by mapping each byte to a
-- corresponding Unicode 'Char' in 0-255 range.
--
-- /Since: 0.7.0 ("Streamly.Data.Unicode.Stream")/
--
-- @since 0.8.0
{-# INLINE decodeLatin1 #-}
decodeLatin1 :: (IsStream t, Monad m) => t m Word8 -> t m Char
decodeLatin1 = S.map (unsafeChr . fromIntegral)

-- | Encode a stream of Unicode characters to bytes by mapping each character
-- to a byte in 0-255 range. Throws an error if the input stream contains
-- characters beyond 255.
--
-- @since 0.8.0
{-# INLINE encodeLatin1' #-}
encodeLatin1' :: (IsStream t, Monad m) => t m Char -> t m Word8
encodeLatin1' = S.map convert
    where
    convert c =
        let codepoint = ord c
        in if codepoint > 255
           then error $ "Streamly.String.encodeLatin1 invalid " ++
                      "input char codepoint " ++ show codepoint
           else fromIntegral codepoint

-- | Like 'encodeLatin1'' but silently truncates and maps input characters beyond
-- 255 to (incorrect) chars in 0-255 range. No error or exception is thrown
-- when such truncation occurs.
--
-- /Since: 0.7.0 ("Streamly.Data.Unicode.Stream")/
--
-- /Since: 0.8.0 (Lenient Behaviour)/
{-# INLINE encodeLatin1 #-}
encodeLatin1 :: (IsStream t, Monad m) => t m Char -> t m Word8
encodeLatin1 = S.map (fromIntegral . ord)

-- | Like 'encodeLatin1' but drops the input characters beyond 255.
--
-- @since 0.8.0
{-# INLINE encodeLatin1_ #-}
encodeLatin1_ :: (IsStream t, Monad m) => t m Char -> t m Word8
encodeLatin1_ = S.map (fromIntegral . ord) . S.filter (<= chr (255))

-- | Same as 'encodeLatin1'
--
{-# DEPRECATED encodeLatin1Lax "Please use 'encodeLatin1' instead" #-}
{-# INLINE encodeLatin1Lax #-}
encodeLatin1Lax :: (IsStream t, Monad m) => t m Char -> t m Word8
encodeLatin1Lax = encodeLatin1

-- | Decode a UTF-8 encoded bytestream to a stream of Unicode characters.
-- The function throws an error if an invalid codepoint is encountered.
--
-- @since 0.8.0
{-# INLINE decodeUtf8' #-}
decodeUtf8' :: (Monad m, IsStream t) => t m Word8 -> t m Char
decodeUtf8' = D.fromStreamD . decodeUtf8D' . D.toStreamD

-- |
--
-- /Pre-release/
{-# INLINE decodeUtf8Arrays' #-}
decodeUtf8Arrays' :: (MonadIO m, IsStream t) => t m (Array Word8) -> t m Char
decodeUtf8Arrays' = D.fromStreamD . decodeUtf8ArraysD' . D.toStreamD

-- | Decode a UTF-8 encoded bytestream to a stream of Unicode characters.
-- Any invalid codepoint encountered is replaced with the unicode replacement
-- character.
--
-- /Since: 0.7.0 ("Streamly.Data.Unicode.Stream")/
--
-- /Since: 0.8.0 (Lenient Behaviour)/
{-# INLINE decodeUtf8 #-}
decodeUtf8 :: (Monad m, IsStream t) => t m Word8 -> t m Char
decodeUtf8 = D.fromStreamD . decodeUtf8D . D.toStreamD

-- | Decode a UTF-8 encoded bytestream to a stream of Unicode characters.
-- Any invalid codepoint encountered is dropped.
--
-- @since 0.8.0
{-# INLINE decodeUtf8_ #-}
decodeUtf8_ :: (Monad m, IsStream t) => t m Word8 -> t m Char
decodeUtf8_ = D.fromStreamD . decodeUtf8D_ . D.toStreamD

-- | Same as 'decodeUtf8'
--
{-# DEPRECATED decodeUtf8Lax "Please use 'decodeUtf8' instead" #-}
{-# INLINE decodeUtf8Lax #-}
decodeUtf8Lax :: (IsStream t, Monad m) => t m Word8 -> t m Char
decodeUtf8Lax = decodeUtf8

-- |
--
-- /Pre-release/
{-# INLINE decodeUtf8Either #-}
decodeUtf8Either :: (Monad m, IsStream t)
    => t m Word8 -> t m (Either DecodeError Char)
decodeUtf8Either = D.fromStreamD . decodeUtf8EitherD . D.toStreamD

-- |
--
-- /Pre-release/
{-# INLINE resumeDecodeUtf8Either #-}
resumeDecodeUtf8Either
    :: (Monad m, IsStream t)
    => DecodeState
    -> CodePoint
    -> t m Word8
    -> t m (Either DecodeError Char)
resumeDecodeUtf8Either st cp =
    D.fromStreamD . resumeDecodeUtf8EitherD st cp . D.toStreamD

-- |
--
-- /Pre-release/
{-# INLINE decodeUtf8Arrays #-}
decodeUtf8Arrays ::
       (MonadIO m, IsStream t) => t m (Array Word8) -> t m Char
decodeUtf8Arrays =
    D.fromStreamD . decodeUtf8ArraysD . D.toStreamD

-- |
--
-- /Pre-release/
{-# INLINE decodeUtf8Arrays_ #-}
decodeUtf8Arrays_ ::
       (MonadIO m, IsStream t) => t m (Array Word8) -> t m Char
decodeUtf8Arrays_ =
    D.fromStreamD . decodeUtf8ArraysD_ . D.toStreamD

-- | Encode a stream of Unicode characters to a UTF-8 encoded bytestream. When
-- any invalid character (U+D800-U+D8FF) is encountered in the input stream the
-- function errors out.
--
-- @since 0.8.0
{-# INLINE encodeUtf8' #-}
encodeUtf8' :: (Monad m, IsStream t) => t m Char -> t m Word8
encodeUtf8' = D.fromStreamD . encodeUtf8D' . D.toStreamD

-- | See section "3.9 Unicode Encoding Forms" in
-- https://www.unicode.org/versions/Unicode13.0.0/UnicodeStandard-13.0.pdf
--
{-# INLINE_NORMAL encodeUtf8D #-}
encodeUtf8D :: Monad m => Stream m Char -> Stream m Word8
encodeUtf8D (Stream step state) = Stream step' (EncodeState state WNil)
  where
    {-# INLINE_LATE step' #-}
    step' gst (EncodeState st WNil) = do
        r <- step (adaptState gst) st
        return $
            case r of
                Yield c s ->
                    case ord c of
                        x | x <= 0x7F ->
                              Yield (fromIntegral x) (EncodeState s WNil)
                          | x <= 0x7FF -> Skip (EncodeState s (ord2 c))
                          | x <= 0xFFFF ->
                              if isSurrogate c
                              then Skip $
                                   EncodeState
                                       s
                                       (WCons 239 (WCons 191 (WCons 189 WNil)))
                              else Skip (EncodeState s (ord3 c))
                          | otherwise -> Skip (EncodeState s (ord4 c))
                Skip s -> Skip (EncodeState s WNil)
                Stop -> Stop
    step' _ (EncodeState s (WCons x xs)) = return $ Yield x (EncodeState s xs)


-- | Encode a stream of Unicode characters to a UTF-8 encoded bytestream. Any
-- Invalid characters (U+D800-U+D8FF) in the input stream are replaced by the
-- Unicode replacement character U+FFFD.
--
-- /Since: 0.7.0 ("Streamly.Data.Unicode.Stream")/
--
-- /Since: 0.8.0 (Lenient Behaviour)/
{-# INLINE encodeUtf8 #-}
encodeUtf8 :: (Monad m, IsStream t) => t m Char -> t m Word8
encodeUtf8 = D.fromStreamD . encodeUtf8D . D.toStreamD

{-# INLINE_NORMAL encodeUtf8D_ #-}
encodeUtf8D_ :: Monad m => Stream m Char -> Stream m Word8
encodeUtf8D_ (Stream step state) = Stream step' (EncodeState state WNil)
  where
    {-# INLINE_LATE step' #-}
    step' gst (EncodeState st WNil) = do
        r <- step (adaptState gst) st
        return $
            case r of
                Yield c s ->
                    case ord c of
                        x | x <= 0x7F ->
                              Yield (fromIntegral x) (EncodeState s WNil)
                          | x <= 0x7FF -> Skip (EncodeState s (ord2 c))
                          | x <= 0xFFFF ->
                              if isSurrogate c
                              then Skip $
                                   EncodeState s WNil
                              else Skip (EncodeState s (ord3 c))
                          | otherwise -> Skip (EncodeState s (ord4 c))
                Skip s -> Skip (EncodeState s WNil)
                Stop -> Stop
    step' _ (EncodeState s (WCons x xs)) = return $ Yield x (EncodeState s xs)

-- | Encode a stream of Unicode characters to a UTF-8 encoded bytestream. Any
-- Invalid characters (U+D800-U+D8FF) in the input stream are dropped.
--
-- @since 0.8.0
{-# INLINE encodeUtf8_ #-}
encodeUtf8_ :: (Monad m, IsStream t) => t m Char -> t m Word8
encodeUtf8_ = D.fromStreamD . encodeUtf8D_ . D.toStreamD

-- | Same as 'encodeUtf8'
--
{-# DEPRECATED encodeUtf8Lax "Please use 'encodeUtf8' instead" #-}
{-# INLINE encodeUtf8Lax #-}
encodeUtf8Lax :: (IsStream t, Monad m) => t m Char -> t m Word8
encodeUtf8Lax = encodeUtf8

{-
-------------------------------------------------------------------------------
-- Utility operations on strings
-------------------------------------------------------------------------------

strip :: IsStream t => t m Char -> t m Char
strip = undefined

stripEnd :: IsStream t => t m Char -> t m Char
stripEnd = undefined
-}

-- | Remove leading whitespace from a string.
--
-- > stripStart = S.dropWhile isSpace
--
-- /Pre-release/
{-# INLINE stripStart #-}
stripStart :: (Monad m, IsStream t) => t m Char -> t m Char
stripStart = S.dropWhile isSpace

-- | Fold each line of the stream using the supplied 'Fold'
-- and stream the result.
--
-- >>> Stream.toList $ lines Fold.toList (Stream.fromList "lines\nthis\nstring\n\n\n")
-- ["lines","this","string","",""]
--
-- > lines = S.splitOnSuffix (== '\n')
--
-- /Pre-release/
{-# INLINE lines #-}
lines :: (Monad m, IsStream t) => Fold m Char b -> t m Char -> t m b
lines = S.splitOnSuffix (== '\n')

foreign import ccall unsafe "u_iswspace"
  iswspace :: Int -> Int

-- | Code copied from base/Data.Char to INLINE it
{-# INLINE isSpace #-}
isSpace :: Char -> Bool
isSpace c
  | uc <= 0x377 = uc == 32 || uc - 0x9 <= 4 || uc == 0xa0
  | otherwise = iswspace (ord c) /= 0
  where
    uc = fromIntegral (ord c) :: Word

-- | Fold each word of the stream using the supplied 'Fold'
-- and stream the result.
--
-- >>>  Stream.toList $ words Fold.toList (Stream.fromList "fold these     words")
-- ["fold","these","words"]
--
-- > words = S.wordsBy isSpace
--
-- /Pre-release/
{-# INLINE words #-}
words :: (Monad m, IsStream t) => Fold m Char b -> t m Char -> t m b
words = S.wordsBy isSpace

-- | Unfold a stream to character streams using the supplied 'Unfold'
-- and concat the results suffixing a newline character @\\n@ to each stream.
--
-- @
-- unlines = Stream.interposeSuffix '\n'
-- @
--
-- /Pre-release/
{-# INLINE unlines #-}
unlines :: (MonadIO m, IsStream t) => Unfold m a Char -> t m a -> t m Char
unlines = S.interposeSuffix '\n'

-- | Unfold the elements of a stream to character streams using the supplied
-- 'Unfold' and concat the results with a whitespace character infixed between
-- the streams.
--
-- @
-- unwords = Stream.interpose ' '
-- @
--
-- /Pre-release/
{-# INLINE unwords #-}
unwords :: (MonadIO m, IsStream t) => Unfold m a Char -> t m a -> t m Char
unwords = S.interpose ' '
