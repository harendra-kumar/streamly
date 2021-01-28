-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (first)
import Data.Primitive.Types (Prim(..), sizeOf)
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.SVar (adaptState)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.StreamD as D

import GHC.Exts
import Control.Monad.Primitive
import Prelude hiding (length, unlines)

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

data Array a = Array (MutableByteArray# RealWorld)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Copy a range of the first array to the specified region in the second
-- array. Both arrays must fully contain the specified ranges, but this is not
-- checked. The regions are allowed to overlap, although this is only possible
-- when the same array is provided as both the source and the destination.
{-# INLINE unsafeCopy #-}
unsafeCopy ::
       forall m a. (MonadIO m, Prim a)
    => Array a -- ^ destination array
    -> Int -- ^ offset into destination array
    -> Array a -- ^ source array
    -> Int -- ^ offset into source array
    -> Int -- ^ number of elements to copy
    -> m ()
unsafeCopy (Array dst#) (I# doff#) (Array src#) (I# soff#) (I# n#) =
    liftIO $ do
        let toBytes cnt# = cnt# *# (sizeOf# (undefined :: a))
        primitive_ $
            copyMutableByteArray#
                src#
                (toBytes soff#)
                dst#
                (toBytes doff#)
                (toBytes n#)

-------------------------------------------------------------------------------
-- Length
-------------------------------------------------------------------------------

-- XXX rename to byteCount?
{-# INLINE byteLength #-}
byteLength :: MonadIO m => Array a -> m Int
byteLength (Array arr#) =
    liftIO $
    primitive
        (\s0# ->
             case getSizeofMutableByteArray# arr# s0# of
                 (# s1#, blen# #) -> (# s1#, I# blen# #))


-- XXX Rename length to elemCount so that there is no confusion bout what it
-- means.
--
-- XXX Since size of 'a' is statically known, we can replace `quot` with shift
-- when it is power of 2. Though it may not matter unless length is used too
-- often.
--
{-# INLINE length #-}
length ::
       forall m a. (MonadIO m, Prim a)
    => Array a
    -> m Int
length arr =
    liftIO $ do
        blen <- byteLength arr
        return $ blen `quot` (sizeOf (undefined :: a))

-------------------------------------------------------------------------------
-- Random Access
-------------------------------------------------------------------------------

{-# INLINE unsafeReadIndex #-}
unsafeReadIndex :: (MonadIO m, Prim a) => Array a -> Int -> m a
unsafeReadIndex (Array arr#) (I# i#) =
    liftIO $ primitive (readByteArray# arr# i#)

{-# INLINE unsafeWriteIndex #-}
unsafeWriteIndex ::
       (MonadIO m, Prim a)
    => Array a -- ^ array
    -> Int -- ^ index
    -> a -- ^ element
    -> m ()
unsafeWriteIndex (Array arr#) (I# i#) x =
    liftIO $ primitive_ (writeByteArray# arr# i# x)

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- Note: We do not store the actual length of the array in the Array
-- constructor. Therefore, for "length" API to work correctly we need to match
-- the ByteArray length with the used length by shrinking it.
--
-- However, it may be expensive to always shrink the array. We may want to
-- shrink only if significant space is being wasted. If we want to do that then
-- we will have to store the used length separately. Or does GHC take care of
-- that?
-- Although the docs are not explicit about it, given how the signature is,
-- the shrinking must me inplace. "resizeMutableByteArray#" shrinks the
-- array inplace.
{-# INLINE shrinkArray #-}
shrinkArray ::
       forall m a. (MonadIO m, Prim a)
    => Array a
    -> Int -- ^ new size
    -> m ()
shrinkArray (Array arr#) (I# n#) =
    liftIO $ do
        let bytes = n# *# (sizeOf# (undefined :: a))
        primitive_ (shrinkMutableByteArray# arr# bytes)

-- | Fold the whole input to a single array.
--
-- /Caution! Do not use this on infinite streams./
--
-- /Pre-release/
{-# INLINE_NORMAL write #-}
write :: (MonadIO m, Prim a) => Fold m a (Array a)
write = FL.rmapM extract $ FL.mkFoldlM step initial

    where

    initial = do
        marr <- newArray 0
        return $ Tuple3' marr 0 0

    step (Tuple3' marr i capacity) x
        | i == capacity = do
            let newCapacity = max (capacity * 2) 1
            newMarr <- resizeArray marr newCapacity
            unsafeWriteIndex newMarr i x
            return $ Tuple3' newMarr (i + 1) newCapacity
        | otherwise = do
            unsafeWriteIndex marr i x
            return $ Tuple3' marr (i + 1) capacity

    extract (Tuple3' marr len _) = shrinkArray marr len >> return marr

-- | @writeN n@ folds a maximum of @n@ elements from the input stream to an
-- 'Array'.
--
-- /Pre-release/
{-# INLINE_NORMAL writeN #-}
writeN :: (MonadIO m, Prim a) => Int -> Fold m a (Array a)
writeN limit = Fold step initial extract

    where

    initial = do
        marr <- newArray limit
        return $ FL.Partial $ Tuple' marr 0

    extract (Tuple' marr len) = shrinkArray marr len >> return marr

    step s@(Tuple' marr i) x
        | i == limit = FL.Done <$> extract s
        | otherwise = do
            unsafeWriteIndex marr i x
            return $ FL.Partial $ Tuple' marr (i + 1)

-- Use Tuple' instead?
data ArrayUnsafe a = ArrayUnsafe
    {-# UNPACK #-} !(Array a)
    {-# UNPACK #-} !Int

-- | Like 'writeN' but does not check the array bounds when writing. The fold
-- driver must not call the step function more than 'n' times otherwise it will
-- corrupt the memory and crash. This function exists mainly because any
-- conditional in the step function blocks fusion causing 10x performance
-- slowdown.
--
-- /Pre-release/
{-# INLINE_NORMAL writeNUnsafe #-}
writeNUnsafe :: (MonadIO m, Prim a) => Int -> Fold m a (Array a)
writeNUnsafe n = FL.rmapM extract $ FL.mkFoldlM step initial

    where

    initial = do
        arr <- newArray (max n 0)
        return $ ArrayUnsafe arr 0
    step (ArrayUnsafe marr i) x = do
        unsafeWriteIndex marr i x
        return $ ArrayUnsafe marr (i + 1)
    extract (ArrayUnsafe marr i) = shrinkArray marr i >> return marr

{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: (MonadIO m, Prim a) => Int -> D.Stream m a -> m (Array a)
fromStreamDN limit str = do
    marr <- newArray (max limit 0)
    let step i x = i `seq` (unsafeWriteIndex marr i x) >> return (i + 1)
    n <- D.foldlM' step (return 0) $ D.take limit str
    shrinkArray marr n
    return marr

{-# INLINE fromStreamD #-}
fromStreamD :: (MonadIO m, Prim a) => D.Stream m a -> m (Array a)
fromStreamD str = D.fold write str

{-# INLINABLE fromListNM #-}
fromListNM :: (MonadIO m, Prim a) => Int -> [a] -> m (Array a)
fromListNM n xs = fromStreamDN n $ D.fromList xs

{-# INLINABLE fromListM #-}
fromListM :: (MonadIO m, Prim a) => [a] -> m (Array a)
fromListM xs = fromStreamD $ D.fromList xs

-------------------------------------------------------------------------------
-- Combining
-------------------------------------------------------------------------------

-- Splice two mutable arrays creating a new array.
{-# INLINE spliceTwo #-}
spliceTwo :: (MonadIO m, Prim a) => Array a -> Array a -> m (Array a)
spliceTwo a1 a2 = do
    l1 <- length a1
    l2 <- length a2
    a3 <- resizeArray a1 (l1 + l2)
    unsafeCopy a2 0 a3 l1 l2
    return a3

-------------------------------------------------------------------------------
-- Stream of Arrays
-------------------------------------------------------------------------------

data GroupState s a
    = GroupStart s
    | GroupBuffer s (Array a) Int
    | GroupYield (Array a) s
    | GroupLastYield (Array a) Int
    | GroupFinish

-- | @fromStreamArraysOf n stream@ groups the input stream into a stream of
-- arrays of size n.
{-# INLINE_NORMAL fromStreamDArraysOf #-}
fromStreamDArraysOf ::
       (MonadIO m, Prim a) => Int -> D.Stream m a -> D.Stream m (Array a)
-- fromStreamDArraysOf n str = D.groupsOf n (writeN n) str
fromStreamDArraysOf n (D.Stream step state) = D.Stream step' (GroupStart state)

    where

    {-# INLINE_LATE step' #-}
    step' _ (GroupStart st) = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $
            "Streamly.Internal.Data.Array.Foreign.Mut.Type.fromStreamDArraysOf: the size of " ++
            "arrays [" ++ show n ++ "] must be a natural number"
        arr <- newArray n
        return $ D.Skip (GroupBuffer st arr 0)
    step' gst (GroupBuffer st arr i)
        | i < n = do
            r <- step (adaptState gst) st
            case r of
                D.Yield x s -> do
                    unsafeWriteIndex arr i x
                    return $ D.Skip (GroupBuffer s arr (i + 1))
                D.Skip s -> return $ D.Skip (GroupBuffer s arr i)
                D.Stop -> return $ D.Skip (GroupLastYield arr i)
        | otherwise = return $ D.Skip (GroupYield arr st)
    step' _ (GroupYield arr st) = do
        nArr <- newArray n
        return $ D.Yield arr (GroupBuffer st nArr 0)
    step' _ (GroupLastYield arr i)
        | i == 0 = return D.Stop
        | otherwise = do
            shrinkArray arr i
            return $ D.Yield arr GroupFinish
    step' _ GroupFinish = return D.Stop

data SpliceState s arr
    = SpliceInitial s
    | SpliceBuffering s arr
    | SpliceYielding arr (SpliceState s arr)
    | SpliceFinish

-- XXX can use general grouping combinators to achieve this?
-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size in bytes. Note that if a single array is bigger than
-- the specified size we do not split it to fit. When we coalesce multiple
-- arrays if the size would exceed the specified size we do not coalesce
-- therefore the actual array size may be less than the specified chunk size.
--
-- /Pre-release/
{-# INLINE_NORMAL packArraysChunksOf #-}
packArraysChunksOf ::
       (MonadIO m, Prim a)
    => Int
    -> D.Stream m (Array a)
    -> D.Stream m (Array a)
packArraysChunksOf n (D.Stream step state) =
    D.Stream step' (SpliceInitial state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (SpliceInitial st) = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.Array.Foreign.Mut.Type.packArraysChunksOf: the size of "
                 ++ "arrays [" ++ show n ++ "] must be a natural number"
        r <- step gst st
        case r of
            D.Yield arr s -> do
              len <- byteLength arr
              return $
                  if len >= n
                  then D.Skip $ SpliceYielding arr (SpliceInitial s)
                  else D.Skip $ SpliceBuffering s arr
            D.Skip s -> return $ D.Skip (SpliceInitial s)
            D.Stop -> return $ D.Stop

    step' gst (SpliceBuffering st buf) = do
        r <- step gst st
        case r of
            D.Yield arr s -> do
                blen <- byteLength buf
                alen <- byteLength arr
                let len = blen + alen
                if len > n
                then return $
                    D.Skip (SpliceYielding buf (SpliceBuffering s arr))
                else do
                    buf' <- spliceTwo buf arr
                    return $ D.Skip (SpliceBuffering s buf')
            D.Skip s -> return $ D.Skip (SpliceBuffering s buf)
            D.Stop -> return $ D.Skip (SpliceYielding buf SpliceFinish)

    step' _ SpliceFinish = return D.Stop

    step' _ (SpliceYielding arr next) = return $ D.Yield arr next

{-# INLINE_NORMAL lpackArraysChunksOf #-}
lpackArraysChunksOf ::
       (MonadIO m, Prim a) => Int -> Fold m (Array a) () -> Fold m (Array a) ()
lpackArraysChunksOf n (Fold step1 initial1 extract1) =
    Fold step initial extract

    where

    initial = do
        when (n <= 0)
            -- XXX we can pass the module string from the higher level API
          $ error
          $ "Streamly.Internal.Data.Array.Foreign.Mut.Type."
              ++ "packArraysChunksOf: the size of arrays ["
              ++ show n
              ++ "] must be a natural number"
        res <- initial1
        return $ first (Tuple' Nothing) res

    extract (Tuple' Nothing r1) = extract1 r1
    extract (Tuple' (Just buf) r1) = do
        r <- step1 r1 buf
        case r of
            FL.Partial rr -> extract1 rr
            FL.Done () -> return ()

    step (Tuple' Nothing r1) arr = do
        len <- byteLength arr
        if len >= n
        then do
            r <- step1 r1 arr
            case r of
                FL.Done () -> return $ FL.Done ()
                FL.Partial s -> do
                    extract1 s
                    res <- initial1
                    return $ first (Tuple' Nothing) res
        else return $ FL.Partial $ Tuple' (Just arr) r1
    step (Tuple' (Just buf) r1) arr = do
        blen <- byteLength buf
        alen <- byteLength arr
        let len = blen + alen
        buf' <- spliceTwo buf arr
        if len >= n
        then do
            r <- step1 r1 buf'
            case r of
                FL.Done () -> return $ FL.Done ()
                FL.Partial s -> do
                    extract1 s
                    res <- initial1
                    return $ first (Tuple' Nothing) res
        else return $ FL.Partial $ Tuple' (Just buf') r1
