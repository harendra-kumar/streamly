#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Array.Storable.Foreign.Types
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Array.Storable.Foreign.Types
    (
      Array (..)

    -- * Freezing and Thawing
    , unsafeFreeze
    , unsafeFreezeWithShrink
    , unsafeThaw

    -- * Construction
    , spliceTwo

    , fromPtr
    , fromAddr#
    , fromCString#
    , fromList
    , fromListN
    , fromStreamDN

    -- * Streams of arrays
    , fromStreamDArraysOf
    , MA.FlattenState (..) -- for inspection testing
    , flattenArrays
    , flattenArraysRev
    , packArraysChunksOf
    , MA.SpliceState (..)
    , lpackArraysChunksOf
#if !defined(mingw32_HOST_OS)
    , groupIOVecsOf
#endif
    , splitOn
    , breakOn

    -- * Elimination
    , unsafeIndexIO
    , unsafeIndex
    , byteLength
    , length

    , foldl'
    , foldr
    , splitAt

    , toStreamD
    , toStreamDRev
    , toStreamK
    , toStreamKRev
    , toList

    , unlines

    -- * Folds
    , toArrayMinChunk
    , writeN
    , writeNUnsafe
    , MA.ArrayUnsafe (..)
    , writeNAligned
    , writeNAlignedUnmanaged
    , write
    , writeAligned

    -- * Utilities
    , MA.defaultChunkSize
    , MA.mkChunkSize
    , MA.mkChunkSizeKB
    , MA.unsafeInlineIO

    , MA.memcpy
    , MA.memcmp
    , MA.bytesToElemCount
    , writeNUnsafeMaybe 
    )
where

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.Base (Addr#, nullAddr#)
import GHC.Exts (IsList, IsString(..))
import GHC.ForeignPtr (ForeignPtr(..), newForeignPtr_)
#ifdef DEVBUILD
import GHC.ForeignPtr (touchForeignPtr, unsafeForeignPtrToPtr)
#endif
import GHC.IO (unsafePerformIO)
import GHC.Ptr (Ptr(..))
import Streamly.Internal.Data.Fold.Types (Fold(..), lmap)
import Text.Read (readPrec, readListPrec, readListPrecDefault)

import Prelude hiding (length, foldr, read, unlines, splitAt)

import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Array.Storable.Foreign.Mut.Types as MA
import qualified GHC.Exts as Exts

#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif

#if !defined(mingw32_HOST_OS)
import Streamly.FileSystem.FDIO (IOVec(..))
#endif

#ifdef DEVBUILD
import qualified Data.Foldable as F
#endif

-------------------------------------------------------------------------------
-- Design Notes
-------------------------------------------------------------------------------

-- There are two goals that we want to fulfill with arrays.  One, holding large
-- amounts of data in non-GC memory and the ability to pass this data to and
-- from the operating system without an extra copy overhead. Two, allow random
-- access to elements based on their indices. The first one falls in the
-- category of storage buffers while the second one falls in the category of
-- maps/multisets/hashmaps.
--
-- For the first requirement we use an array of Storables and store it in a
-- ForeignPtr. We can have both immutable and mutable variants of this array
-- using wrappers over the same underlying type.
--
-- For the second requirement, we need a separate type for arrays of
-- polymorphic values, for example vectors of handler functions, lookup tables.
-- We can call this type a "vector" in contrast to arrays.  It should not
-- require Storable instance for the type. In that case we need to use an
-- Array# instead of a ForeignPtr. This type of array would not reduce the GC
-- overhead much because each element of the array still needs to be scanned by
-- the GC.  However, it can store polymorphic elements and allow random access
-- to those.  But in most cases random access means storage, and it means we
-- need to avoid GC scanning except in cases of trivially small storage. One
-- way to achieve that would be to put the array in a Compact region. However,
-- if and when we mutate this, we will have to use a manual GC copying out to
-- another CR and freeing the old one.

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

-- We require that an array stores only Storable. Arrays are used for buffering
-- data while streams are used for processing. If you want something to be
-- buffered it better be Storable so that we can store it in non-GC memory.
--
-- We can use a "Storable" constraint in the Array type and the Constraint can
-- be automatically provided to a function that pattern matches on the Array
-- type. However, it has huge performance cost, so we do not use it.
-- XXX Can this cost be alleviated in GHC-8.10 specialization fix?
--
-- XXX Another way to not require the Storable constraint in array operations
-- is to store the elemSize in the array at construction and use that instead
-- of using sizeOf. Need to charaterize perf cost of this.
--
-- XXX rename the fields to "start, next, end".
--
data Array a =
#ifdef DEVBUILD
    Storable a =>
#endif
    Array
    { aStart :: {-# UNPACK #-} !(ForeignPtr a) -- first address
    , aEnd   :: {-# UNPACK #-} !(Ptr a)        -- first unused addres
    }

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

foreign import ccall unsafe "string.h strlen" c_strlen
    :: CString -> IO CSize

-------------------------------------------------------------------------------
-- Freezing and Thawing
-------------------------------------------------------------------------------

-- | Returns an immutable array using the same underlying pointers of the
-- mutable array. If the underlying array is mutated, the immutable promise is
-- lost. Please make sure that the mutable array is never used after freezing it
-- using /unsafeFreeze/.
{-# INLINE unsafeFreeze #-}
unsafeFreeze :: MA.Array a -> Array a
unsafeFreeze (MA.Array as ae _) = Array as ae

-- | Similar to 'unsafeFreeze' but uses 'MA.shrinkToFit' on the mutable array
-- first.
{-# INLINE unsafeFreezeWithShrink #-}
unsafeFreezeWithShrink :: Storable a => MA.Array a -> Array a
unsafeFreezeWithShrink arr = unsafePerformIO $ do
  MA.Array as ae _ <- MA.shrinkToFit arr
  return $ Array as ae

-- | Returns a mutable array using the same underlying pointers of the immutable
-- array. If the resulting array is mutated, the older immutable array is
-- mutated as well. Please make sure that the immutable array is never used
-- after thawing it using /unsafeThaw/.
{-# INLINE unsafeThaw #-}
unsafeThaw :: Array a -> MA.Array a
unsafeThaw (Array as ae) = MA.Array as ae ae

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- Splice two immutable arrays creating a new array.
{-# INLINE spliceTwo #-}
spliceTwo :: (MonadIO m, Storable a) => Array a -> Array a -> m (Array a)
spliceTwo arr1 arr2 =
    unsafeFreeze <$> MA.spliceTwo (unsafeThaw arr1) (unsafeThaw arr2)

-- | Create an 'Array' of the given number of elements of type @a@ from a read
-- only pointer @Ptr a@.  The pointer is not freed when the array is garbage
-- collected. This API is unsafe for the following reasons:
--
-- 1. The pointer must point to static pinned memory or foreign memory that
-- does not require freeing..
-- 2. The pointer must be legally accessible upto the given length.
-- 3. To guarantee that the array is immutable, the contents of the address
-- must be guaranteed to not change.
--
-- /Unsafe/
--
-- /Internal/
--
{-# INLINE fromPtr #-}
fromPtr ::
#ifdef DEVBUILD
    Storable a =>
#endif
    Int -> Ptr a -> Array a
fromPtr n ptr = MA.unsafeInlineIO $ do
    fptr <- newForeignPtr_ ptr
    let end = ptr `plusPtr` n
    return $ Array
        { aStart = fptr
        , aEnd   = end
        }

-- XXX when converting an array of Word8 from a literal string we can simply
-- refer to the literal string. Is it possible to write rules such that
-- fromList Word8 can be rewritten so that GHC does not first convert the
-- literal to [Char] and then we convert it back to an Array Word8?
--
-- TBD: We can also add template haskell quasiquotes to specify arrays of other
-- literal types. TH will encode them into a string literal and we read that as
-- an array of the required type. With template Haskell we can provide a safe
-- version of fromString#.
--
-- | Create an @Array Word8@ of the given length from a static, read only
-- machine address 'Addr#'. See 'fromPtr' for safety caveats.
--
-- A common use case for this API is to create an array from a static unboxed
-- string literal. GHC string literals are of type 'Addr#', and must contain
-- characters that can be encoded in a byte i.e. characters or literal bytes in
-- the range from 0-255.
--
-- >>> fromAddr# 5 "hello world!"#
-- > [104,101,108,108,111]
--
-- >>> fromAddr# 3 "\255\NUL\255"#
-- > [255,0,255]
--
-- /See also: 'fromString#'/
--
-- /Unsafe/
--
-- /Time complexity: O(1)/
--
-- /Internal/
--
{-# INLINE fromAddr# #-}
fromAddr# ::
#ifdef DEVBUILD
    Storable a =>
#endif
    Int -> Addr# -> Array a
fromAddr# n addr# = fromPtr n (castPtr $ Ptr addr#)

-- | Generate a byte array from an 'Addr#' that contains a sequence of NUL
-- (@0@) terminated bytes. The array would not include the NUL byte. The
-- address must be in static read-only memory and must be legally accessible up
-- to and including the first NUL byte.
--
-- An unboxed string literal (e.g. @"hello"#@) is a common example of an
-- 'Addr#' in static read only memory. It represents the UTF8 encoded sequence
-- of bytes terminated by a NUL byte (a 'CString') corresponding to the
-- given unicode string.
--
-- >>> fromCString# "hello world!"#
-- > [104,101,108,108,111,32,119,111,114,108,100,33]
--
-- >>> fromCString# "\255\NUL\255"#
-- > [255]
--
-- /See also: 'fromAddr#'/
--
-- /Unsafe/
--
-- /Time complexity: O(n) (computes the length of the string)/
--
-- /Internal/
--
{-# INLINE fromCString# #-}
fromCString# :: Addr# -> Array Word8
fromCString# addr# = do
    let cstr = Ptr addr#
        len = MA.unsafeInlineIO $ c_strlen cstr
    fromPtr (fromIntegral len) (castPtr cstr)

-- | Create an 'Array' from the first N elements of a list. The array is
-- allocated to size N, if the list terminates before N elements then the
-- array may hold less than N elements.
--
-- @since 0.7.0
{-# INLINABLE fromListN #-}
fromListN :: Storable a => Int -> [a] -> Array a
fromListN n xs = unsafeFreeze $ MA.fromListN n xs

-- | Create an 'Array' from a list. The list must be of finite size.
--
-- @since 0.7.0
{-# INLINABLE fromList #-}
fromList :: Storable a => [a] -> Array a
fromList xs = unsafeFreeze $ MA.fromList xs

{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: forall m a. (MonadIO m, Storable a)
    => Int -> D.Stream m a -> m (Array a)
fromStreamDN limit str = unsafeFreeze <$> MA.fromStreamDN limit str

-------------------------------------------------------------------------------
-- Streams of arrays
-------------------------------------------------------------------------------

-- | @fromStreamArraysOf n stream@ groups the input stream into a stream of
-- arrays of size n.
{-# INLINE_NORMAL fromStreamDArraysOf #-}
fromStreamDArraysOf :: forall m a. (MonadIO m, Storable a)
    => Int -> D.Stream m a -> D.Stream m (Array a)
-- fromStreamDArraysOf n str = D.groupsOf n (writeN n) str
fromStreamDArraysOf n str = D.map unsafeFreeze $ MA.fromStreamDArraysOf n str

-- XXX concatMap does not seem to have the best possible performance so we have
-- a custom way to concat arrays.
{-# INLINE_NORMAL flattenArrays #-}
flattenArrays :: forall m a. (MonadIO m, Storable a)
    => D.Stream m (Array a) -> D.Stream m a
flattenArrays = MA.flattenArrays . D.map unsafeThaw

{-# INLINE_NORMAL flattenArraysRev #-}
flattenArraysRev :: forall m a. (MonadIO m, Storable a)
    => D.Stream m (Array a) -> D.Stream m a
flattenArraysRev = MA.flattenArraysRev . D.map unsafeThaw


{-# INLINE_NORMAL packArraysChunksOf #-}
packArraysChunksOf :: (MonadIO m, Storable a)
    => Int -> D.Stream m (Array a) -> D.Stream m (Array a)
packArraysChunksOf n str =
    D.map unsafeFreeze $ MA.packArraysChunksOf n $ D.map unsafeThaw str

-- XXX instead of writing two different versions of this operation, we should
-- write it as a pipe.
{-# INLINE_NORMAL lpackArraysChunksOf #-}
lpackArraysChunksOf :: (MonadIO m, Storable a)
    => Int -> Fold m (Array a) () -> Fold m (Array a) ()
lpackArraysChunksOf n fld =
    lmap unsafeThaw $ MA.lpackArraysChunksOf n (lmap unsafeFreeze fld)

#if !defined(mingw32_HOST_OS)

-- | @groupIOVecsOf maxBytes maxEntries@ groups arrays in the incoming stream
-- to create a stream of 'IOVec' arrays with a maximum of @maxBytes@ bytes in
-- each array and a maximum of @maxEntries@ entries in each array.
--
-- @since 0.7.0
{-# INLINE_NORMAL groupIOVecsOf #-}
groupIOVecsOf :: MonadIO m
    => Int -> Int -> D.Stream m (Array a) -> D.Stream m (Array IOVec)
groupIOVecsOf n maxIOVLen str =
    D.map unsafeFreeze $ MA.groupIOVecsOf n maxIOVLen $ D.map unsafeThaw str
#endif

-- | Split a stream of arrays on a given separator byte, dropping the separator
-- and coalescing all the arrays between two separators into a single array.
--
-- @since 0.7.0
{-# INLINE_NORMAL splitOn #-}
splitOn
    :: MonadIO m
    => Word8
    -> D.Stream m (Array Word8)
    -> D.Stream m (Array Word8)
splitOn byte str = D.map unsafeFreeze $ MA.splitOn byte $ D.map unsafeThaw str

-- Drops the separator byte
{-# INLINE breakOn #-}
breakOn :: MonadIO m
    => Word8 -> Array Word8 -> m (Array Word8, Maybe (Array Word8))
breakOn sep arr = do
  (a, b) <- MA.breakOn sep (unsafeThaw arr)
  return (unsafeFreeze a, unsafeFreeze <$> b)

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- | Return element at the specified index without checking the bounds.
--
-- Unsafe because it does not check the bounds of the array.
{-# INLINE_NORMAL unsafeIndexIO #-}
unsafeIndexIO :: forall a. Storable a => Array a -> Int -> IO a
unsafeIndexIO arr = MA.unsafeIndexIO (unsafeThaw arr)

-- | Return element at the specified index without checking the bounds.
{-# INLINE_NORMAL unsafeIndex #-}
unsafeIndex :: forall a. Storable a => Array a -> Int -> a
unsafeIndex arr = MA.unsafeIndex (unsafeThaw arr)

-- | /O(1)/ Get the byte length of the array.
--
-- @since 0.7.0
{-# INLINE byteLength #-}
byteLength :: Array a -> Int
byteLength = MA.byteLength . unsafeThaw

-- | /O(1)/ Get the length of the array i.e. the number of elements in the
-- array.
--
-- @since 0.7.0
{-# INLINE length #-}
length :: forall a. Storable a => Array a -> Int
length arr =  MA.length (unsafeThaw arr)

{-# INLINE_NORMAL toStreamD #-}
toStreamD :: forall m a. (Monad m, Storable a) => Array a -> D.Stream m a
toStreamD arr = MA.toStreamD (unsafeThaw arr)

{-# INLINE toStreamK #-}
toStreamK :: forall t m a. (K.IsStream t, Storable a) => Array a -> t m a
toStreamK arr = MA.toStreamK (unsafeThaw arr)

{-# INLINE_NORMAL toStreamDRev #-}
toStreamDRev :: forall m a. (Monad m, Storable a) => Array a -> D.Stream m a
toStreamDRev arr = MA.toStreamDRev (unsafeThaw arr)

{-# INLINE toStreamKRev #-}
toStreamKRev :: forall t m a. (K.IsStream t, Storable a) => Array a -> t m a
toStreamKRev arr = MA.toStreamKRev (unsafeThaw arr)

{-# INLINE_NORMAL foldl' #-}
foldl' :: forall a b. Storable a => (b -> a -> b) -> b -> Array a -> b
foldl' f z arr = MA.foldl' f z (unsafeThaw arr)

{-# INLINE_NORMAL foldr #-}
foldr :: Storable a => (a -> b -> b) -> b -> Array a -> b
foldr f z arr = MA.foldr f z (unsafeThaw arr)

-- | Create two slices of an array without copying the original array. The
-- specified index @i@ is the first index of the second slice.
--
-- @since 0.7.0
splitAt :: forall a. Storable a => Int -> Array a -> (Array a, Array a)
splitAt i arr = (unsafeFreeze a, unsafeFreeze b)
  where
    (a, b) = MA.splitAt i (unsafeThaw arr)

-- | Convert an 'Array' into a list.
--
-- @since 0.7.0
{-# INLINE toList #-}
toList :: Storable a => Array a -> [a]
toList = MA.toList . unsafeThaw

{-# INLINE_NORMAL unlines #-}
unlines :: forall m a. (MonadIO m, Storable a)
    => a -> D.Stream m (Array a) -> D.Stream m a
unlines sep str = MA.unlines sep (D.map unsafeThaw str)

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- | @writeN n@ folds a maximum of @n@ elements from the input stream to an
-- 'Array'.
--
-- @since 0.7.0
{-# INLINE_NORMAL writeN #-}
writeN :: forall m a. (MonadIO m, Storable a) => Int -> Fold m a (Array a)
writeN = fmap unsafeFreeze . MA.writeN

-- | @writeNAligned alignment n@ folds a maximum of @n@ elements from the input
-- stream to an 'Array' aligned to the given size.
--
-- /Internal/
--
{-# INLINE_NORMAL writeNAligned #-}
writeNAligned :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Fold m a (Array a)
writeNAligned alignSize = fmap unsafeFreeze . MA.writeNAligned alignSize

-- | @writeNAlignedUnmanaged n@ folds a maximum of @n@ elements from the input
-- stream to an 'Array' aligned to the given size and using unmanaged memory.
-- This could be useful to allocate memory that we need to allocate only once
-- in the lifetime of the program.
--
-- /Internal/
--
{-# INLINE_NORMAL writeNAlignedUnmanaged #-}
writeNAlignedUnmanaged :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Fold m a (Array a)
writeNAlignedUnmanaged alignSize =
    fmap unsafeFreeze . MA.writeNAlignedUnmanaged alignSize

-- | Like 'writeN' but does not check the array bounds when writing. The fold
-- driver must not call the step function more than 'n' times otherwise it will
-- corrupt the memory and crash. This function exists mainly because any
-- conditional in the step function blocks fusion causing 10x performance
-- slowdown.
--
-- @since 0.7.0
{-# INLINE_NORMAL writeNUnsafe #-}
writeNUnsafe :: forall m a. (MonadIO m, Storable a)
    => Int -> Fold m a (Array a)
writeNUnsafe n = unsafeFreeze <$> MA.writeNUnsafe n

{-# INLINE_NORMAL writeNUnsafeMaybe #-}
writeNUnsafeMaybe :: forall m a. (MonadIO m, Storable a)
    => Int -> Fold m (Maybe a) (Array a)
writeNUnsafeMaybe n = unsafeFreeze <$> MA.writeNUnsafeMaybe n

-- XXX The realloc based implementation needs to make one extra copy if we use
-- shrinkToFit.  On the other hand, the stream of arrays implementation may
-- buffer the array chunk pointers in memory but it does not have to shrink as
-- we know the exact size in the end. However, memory copying does not seems to
-- be as expensive as the allocations. Therefore, we need to reduce the number
-- of allocations instead. Also, the size of allocations matters, right sizing
-- an allocation even at the cost of copying sems to help.  Should be measured
-- on a big stream with heavy calls to toArray to see the effect.
--
-- XXX check if GHC's memory allocator is efficient enough. We can try the C
-- malloc to compare against.

{-# INLINE_NORMAL toArrayMinChunk #-}
toArrayMinChunk :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Fold m a (Array a)
-- toArrayMinChunk n = FL.mapM spliceArrays $ toArraysOf n
toArrayMinChunk alignSize elemCount =
    unsafeFreeze <$> MA.toArrayMinChunk alignSize elemCount

-- | Fold the whole input to a single array.
--
-- /Caution! Do not use this on infinite streams./
--
-- @since 0.7.0
{-# INLINE write #-}
write :: forall m a. (MonadIO m, Storable a) => Fold m a (Array a)
write = fmap unsafeFreeze MA.write

-- | Like 'write' but the array memory is aligned according to the specified
-- alignment size. This could be useful when we have specific alignment, for
-- example, cache aligned arrays for lookup table etc.
--
-- /Caution! Do not use this on infinite streams./
--
-- @since 0.7.0
{-# INLINE writeAligned #-}
writeAligned :: forall m a. (MonadIO m, Storable a)
    => Int -> Fold m a (Array a)
writeAligned = fmap unsafeFreeze . MA.writeAligned

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance (Show a, Storable a) => Show (Array a) where
    {-# INLINE showsPrec #-}
    showsPrec _ = shows . toList

instance (Storable a, Read a, Show a) => Read (Array a) where
    {-# INLINE readPrec #-}
    readPrec = fromList <$> readPrec
    readListPrec = readListPrecDefault

instance (a ~ Char) => IsString (Array a) where
    {-# INLINE fromString #-}
    fromString = fromList

-- GHC versions 8.0 and below cannot derive IsList
instance Storable a => IsList (Array a) where
    type (Item (Array a)) = a
    {-# INLINE fromList #-}
    fromList = fromList
    {-# INLINE fromListN #-}
    fromListN = fromListN
    {-# INLINE toList #-}
    toList = toList

-- XXX we are assuming that Storable equality means element equality. This may
-- or may not be correct? arrcmp is 40% faster compared to stream equality.
instance (Storable a, Eq a) => Eq (Array a) where
    {-# INLINE (==) #-}
    a == b = unsafeThaw a == unsafeThaw b
    -- arr1 == arr2 = runIdentity $ D.eqBy (==) (toStreamD arr1) (toStreamD arr2)

instance (Storable a, NFData a) => NFData (Array a) where
    {-# INLINE rnf #-}
    rnf = foldl' (\_ x -> rnf x) ()

instance (Storable a, Ord a) => Ord (Array a) where
    {-# INLINE compare #-}
    compare a b = compare (unsafeThaw a) (unsafeThaw b)

    -- Default definitions defined in base do not have an INLINE on them, so we
    -- replicate them here with an INLINE.
    {-# INLINE (<) #-}
    x <  y = case compare x y of { LT -> True;  _ -> False }

    {-# INLINE (<=) #-}
    x <= y = case compare x y of { GT -> False; _ -> True }

    {-# INLINE (>) #-}
    x >  y = case compare x y of { GT -> True;  _ -> False }

    {-# INLINE (>=) #-}
    x >= y = case compare x y of { LT -> False; _ -> True }

    -- These two default methods use '<=' rather than 'compare'
    -- because the latter is often more expensive
    {-# INLINE max #-}
    max x y = if x <= y then y else x

    {-# INLINE min #-}
    min x y = if x <= y then x else y

#ifdef DEVBUILD
-- Definitions using the Storable constraint from the Array type. These are to
-- make the Foldable instance possible though it is much slower (7x slower).
--
{-# INLINE_NORMAL toStreamD_ #-}
toStreamD_ :: forall m a. MonadIO m => Int -> Array a -> D.Stream m a
toStreamD_ size Array{..} =
    let p = unsafeForeignPtrToPtr aStart
    in D.Stream step p

    where

    {-# INLINE_LATE step #-}
    step _ p | p == aEnd = return D.Stop
    step _ p = do
        x <- liftIO $ do
                    r <- peek p
                    touchForeignPtr aStart
                    return r
        return $ D.Yield x (p `plusPtr` size)

{-# INLINE_NORMAL _foldr #-}
_foldr :: forall a b. (a -> b -> b) -> b -> Array a -> b
_foldr f z arr@Array {..} =
    let !n = sizeOf (undefined :: a)
    in unsafePerformIO $ D.foldr f z $ toStreamD_ n arr

-- | Note that the 'Foldable' instance is 7x slower than the direct
-- operations.
instance Foldable Array where
  foldr = _foldr
#endif

-------------------------------------------------------------------------------
-- Semigroup and Monoid
-------------------------------------------------------------------------------

instance Storable a => Semigroup (Array a) where
    arr1 <> arr2 = unsafePerformIO $ spliceTwo arr1 arr2

nullForeignPtr :: ForeignPtr a
nullForeignPtr = ForeignPtr nullAddr# (error "nullForeignPtr")

nil ::
#ifdef DEVBUILD
    Storable a =>
#endif
    Array a
nil = Array nullForeignPtr (Ptr nullAddr#)

instance Storable a => Monoid (Array a) where
    mempty = nil
    mappend = (<>)
