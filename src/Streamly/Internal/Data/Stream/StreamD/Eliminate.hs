-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Eliminate
-- Copyright   : (c) 2018 Composewell Technologies
--               (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

-- A few functions in this module have been adapted from the vector package
-- (c) Roman Leshchinskiy.
--
module Streamly.Internal.Data.Stream.StreamD.Eliminate
    (
    -- * Running a 'Fold'
      foldOnce -- XXX rename to "fold"

    -- -- * Running a 'Parser'
 -- , parse
    , parseArray

    -- * Stream Deconstruction
    , uncons

    -- * Right Folds
    , foldrM
    , foldr
    , foldrMx
    , foldr1

    -- * Left Folds
    , foldlM'
    , foldl'
    , foldlMx'
    , foldlx'
    , parselMx'

    -- * Specific Fold Functions
    , drain
    , mapM_ -- Map and Fold
    , null
    , head
    , headElse
    , tail
    , last
    , elem
    , notElem
    , all
    , any
    , maximum
    , maximumBy
    , minimum
    , minimumBy
    , lookup
    , findM
    , find
    , (!!)
    , the

    -- * To containers
    , toList
    , toListRev

    -- * Multi-Stream Folds
    -- ** Comparisons
    -- | These should probably be expressed using zipping operations.
    , eqBy
    , cmpBy

    -- ** Substreams
    -- | These should probably be expressed using parsers.
    , isPrefixOf
    , isSubsequenceOf
    , stripPrefix
    )
where

#include "inline.hs"

import Control.Exception (assert)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Storable (Storable(..))
import GHC.Exts (SpecConstrAnnotation(..))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Parser (ParseError(..))

-- XXX We should evolve the array module so this isn't required
import Foreign.ForeignPtr (plusForeignPtr)

import qualified Streamly.Internal.Data.Array.Foreign.Types as Array
import qualified Streamly.Internal.Data.Parser.ParserD as PR

-- XXX Are these dependencies OK?
import qualified Streamly.Internal.Data.Stream.StreamD.Generate as Generate
import qualified Streamly.Internal.Data.Stream.StreamD.Nesting as Nesting

import Prelude hiding
       ( all, any, elem, foldr, foldr1, head, last, lookup, mapM, mapM_
       , maximum, minimum, notElem, null, splitAt, tail, (!!))
import Streamly.Internal.Data.Stream.StreamD.Type
import Streamly.Internal.Data.SVar

------------------------------------------------------------------------------
-- Elimination by Folds
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Right Folds
------------------------------------------------------------------------------

{-# INLINE_NORMAL foldr1 #-}
foldr1 :: Monad m => (a -> a -> a) -> Stream m a -> m (Maybe a)
foldr1 f m = do
     r <- uncons m
     case r of
         Nothing   -> return Nothing
         Just (h, t) -> fmap Just (foldr f h t)

------------------------------------------------------------------------------
-- Parsers
------------------------------------------------------------------------------

-- Inlined definition. Without the inline "serially/parser/take" benchmark
-- degrades and parseMany does not fuse. Even using "inline" at the callsite
-- does not help.
{-# INLINE splitAt #-}
splitAt :: Int -> [a] -> ([a],[a])
splitAt n ls
  | n <= 0 = ([], ls)
  | otherwise          = splitAt' n ls
    where
        splitAt' :: Int -> [a] -> ([a], [a])
        splitAt' _  []     = ([], [])
        splitAt' 1  (x:xs) = ([x], xs)
        splitAt' m  (x:xs) = (x:xs', xs'')
          where
            (xs', xs'') = splitAt' (m - 1) xs

-- GHC parser does not accept {-# ANN type [] NoSpecConstr #-}, so we need
-- to make a newtype.
{-# ANN type List NoSpecConstr #-}
newtype List a = List {getList :: [a]}

-- | Run a 'Parse' over a stream.
{-# INLINE_NORMAL parselMx' #-}
parselMx'
    :: MonadThrow m
    => (s -> a -> m (PR.Step s b))
    -> m s
    -> (s -> m b)
    -> Stream m a
    -> m b
parselMx' pstep initial extract (Stream step state) = do
    initial >>= go SPEC state (List [])

    where

    -- XXX currently we are using a dumb list based approach for backtracking
    -- buffer. This can be replaced by a sliding/ring buffer using Data.Array.
    -- That will allow us more efficient random back and forth movement.
    {-# INLINE go #-}
    go !_ st buf !pst = do
        r <- step defState st
        case r of
            Yield x s -> do
                pRes <- pstep pst x
                case pRes of
                    PR.Partial 0 pst1 -> go SPEC s (List []) pst1
                    PR.Partial n pst1 -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let src0 = Prelude.take n (x:getList buf)
                            src  = Prelude.reverse src0
                        gobuf SPEC s (List []) (List src) pst1
                    PR.Continue 0 pst1 -> go SPEC s (List (x:getList buf)) pst1
                    PR.Continue n pst1 -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let (src0, buf1) = splitAt n (x:getList buf)
                            src  = Prelude.reverse src0
                        gobuf SPEC s (List buf1) (List src) pst1
                    PR.Done _ b -> return b
                    PR.Error err -> throwM $ ParseError err
            Skip s -> go SPEC s buf pst
            Stop   -> extract pst

    gobuf !_ s buf (List []) !pst = go SPEC s buf pst
    gobuf !_ s buf (List (x:xs)) !pst = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                gobuf SPEC s (List []) (List xs) pst1
            PR.Partial n pst1 -> do
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC s (List []) (List src) pst1
            PR.Continue 0 pst1 ->
                gobuf SPEC s (List (x:getList buf)) (List xs) pst1
            PR.Continue n pst1 -> do
                assert (n <= length (x:getList buf)) (return ())
                let (src0, buf1) = splitAt n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC s (List buf1) (List src) pst1
            PR.Done _ b -> return b
            PR.Error err -> throwM $ ParseError err

-- We use a doubly linked list for parseArray. Below is the type (DLL) a few
-- helper functions (*DLL)

-- Representation of a doubly linked list
{-# ANN type DLL NoSpecConstr #-}
newtype DLL a = DLL ([a], a, [a])

{-# INLINE singleDLL #-}
singleDLL :: a -> DLL a
singleDLL a = DLL ([], a, [])

{-# INLINE bufferRDLL #-}
bufferRDLL :: DLL a -> [a]
bufferRDLL (DLL (_, _, br)) = br

{-# INLINE dropLDLL #-}
dropLDLL :: DLL a -> DLL a
dropLDLL (DLL (_, a, x)) = (DLL ([], a, x))

{-# INLINE nextDLL #-}
nextDLL :: DLL a -> DLL a
nextDLL (DLL (y, a, x:xs)) = (DLL (a:y, x, xs))
nextDLL _ = error "nextDLL: Empty right buffer"

{-# INLINE previousDLL #-}
previousDLL :: DLL a -> DLL a
previousDLL (DLL (y:ys, a, x)) = (DLL (ys, y, a:x))
previousDLL _ = error "previousDLL: Empty left buffer"

{-# INLINE currentDLL #-}
currentDLL :: DLL a -> a
currentDLL (DLL (_, x, _)) = x

-- Inefficient stuff but probably Ok for our use case.
{-# INLINE insertDLL #-}
insertDLL :: a -> DLL a -> DLL a
insertDLL a (DLL (y, a1, x)) = (DLL (y, a1, x ++ [a]))

-- This should be as efficient as parse
{-# INLINE_NORMAL parseArray #-}
parseArray ::
       forall m a b. (MonadIO m, Storable a)
    => PR.Parser m a b
    -> Stream m (Array.Array a)
    -> m (b, Stream m (Array.Array a))
parseArray (PR.Parser pstep pinitial pextract) (UnStream step state) = do
    ps <- pinitial
    goInit SPEC ps state

    where

    goInit !_ ps s = do
        res <- step defState s
        case res of
            Yield arr s1 ->
                go SPEC ps s1 (singleDLL arr) 0 (Array.length arr)
            Skip s1 -> goInit SPEC ps s1
            Stop -> do
                b <- pextract ps
                return (b, Generate.nil)

    goBreak !_ ps s dl i imax
        | i < 0 =
            let dl1 = previousDLL dl
                arr = currentDLL dl1
                len = Array.length arr
             in goBreak SPEC ps s dl1 (len + i) len
        | otherwise = go SPEC ps s (dropLDLL dl) i imax

    go !_ ps s dl i imax
        | i == imax = do
            res <- step defState s
            case res of
                Yield arr s1 -> do
                    let dl1 = nextDLL (insertDLL arr dl)
                        crr = currentDLL dl1
                    go SPEC ps s1 dl1 0 (Array.length crr)
                Skip s1 -> go SPEC ps s1 dl i imax
                Stop -> do
                    b <- pextract ps
                    return (b, Generate.nil)
        | i < 0 =
            let dl1 = previousDLL dl
                arr = currentDLL dl1
                len = Array.length arr
             in go SPEC ps s dl1 (len + i) len
        | otherwise = do
            let arr = currentDLL dl
                i1 = i + 1
            a <- liftIO $ Array.unsafeIndexIO arr i
            pRes <- pstep ps a
            case pRes of
                PR.Partial 0 ps1 -> go SPEC ps1 s (dropLDLL dl) i1 imax
                PR.Partial n ps1 -> goBreak SPEC ps1 s dl (i1 - n) imax
                PR.Continue 0 ps1 -> go SPEC ps1 s dl i1 imax
                PR.Continue n ps1 -> go SPEC ps1 s dl (i1 - n) imax
                PR.Done n b -> goFinal SPEC b s dl (i1 - n) imax
                PR.Error err -> error err

    goFinal !_ b s dl i imax
        | i == imax =
            let br = bufferRDLL dl
                streamL = Generate.fromList br
                streamR = Stream step s
             in return (b, Nesting.append streamL streamR)
        | i < 0 =
            let dl1 = previousDLL dl
                arr = currentDLL dl1
                len = Array.length arr
             in goFinal SPEC b s dl1 (len + i) len
        | otherwise = do
            let (Array.Array fp end) = currentDLL dl
                br = bufferRDLL dl
                elemSize = sizeOf (undefined :: a)
                newArr = Array.Array (fp `plusForeignPtr` (i * elemSize)) end
                streamL = Generate.fromList (newArr : br)
                streamR = Stream step s
             in return (b, Nesting.append streamL streamR)

------------------------------------------------------------------------------
-- Specialized Folds
------------------------------------------------------------------------------

-- | Run a streaming composition, discard the results.
{-# INLINE_LATE drain #-}
drain :: Monad m => Stream m a -> m ()
-- drain = foldrM (\_ xs -> xs) (return ())
drain (Stream step state) = go SPEC state
  where
    go !_ st = do
        r <- step defState st
        case r of
            Yield _ s -> go SPEC s
            Skip s    -> go SPEC s
            Stop      -> return ()

{-# INLINE_NORMAL null #-}
null :: Monad m => Stream m a -> m Bool
null m = foldrM (\_ _ -> return False) (return True) m

{-# INLINE_NORMAL head #-}
head :: Monad m => Stream m a -> m (Maybe a)
head m = foldrM (\x _ -> return (Just x)) (return Nothing) m

{-# INLINE_NORMAL headElse #-}
headElse :: Monad m => a -> Stream m a -> m a
headElse a m = foldrM (\x _ -> return x) (return a) m

-- Does not fuse, has the same performance as the StreamK version.
{-# INLINE_NORMAL tail #-}
tail :: Monad m => Stream m a -> m (Maybe (Stream m a))
tail (UnStream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield _ s -> return (Just $ Stream step s)
            Skip  s   -> go s
            Stop      -> return Nothing

-- XXX will it fuse? need custom impl?
{-# INLINE_NORMAL last #-}
last :: Monad m => Stream m a -> m (Maybe a)
last = foldl' (\_ y -> Just y) Nothing

{-# INLINE_NORMAL elem #-}
elem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
-- elem e m = foldrM (\x xs -> if x == e then return True else xs) (return False) m
elem e (Stream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield x s
              | x == e    -> return True
              | otherwise -> go s
            Skip s -> go s
            Stop   -> return False

{-# INLINE_NORMAL notElem #-}
notElem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
notElem e s = fmap not (elem e s)

{-# INLINE_NORMAL all #-}
all :: Monad m => (a -> Bool) -> Stream m a -> m Bool
-- all p m = foldrM (\x xs -> if p x then xs else return False) (return True) m
all p (Stream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield x s
              | p x       -> go s
              | otherwise -> return False
            Skip s -> go s
            Stop   -> return True

{-# INLINE_NORMAL any #-}
any :: Monad m => (a -> Bool) -> Stream m a -> m Bool
-- any p m = foldrM (\x xs -> if p x then return True else xs) (return False) m
any p (Stream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield x s
              | p x       -> return True
              | otherwise -> go s
            Skip s -> go s
            Stop   -> return False

{-# INLINE_NORMAL maximum #-}
maximum :: (Monad m, Ord a) => Stream m a -> m (Maybe a)
maximum (Stream step state) = go Nothing state
  where
    go Nothing st = do
        r <- step defState st
        case r of
            Yield x s -> go (Just x) s
            Skip  s   -> go Nothing s
            Stop      -> return Nothing
    go (Just acc) st = do
        r <- step defState st
        case r of
            Yield x s
              | acc <= x  -> go (Just x) s
              | otherwise -> go (Just acc) s
            Skip s -> go (Just acc) s
            Stop   -> return (Just acc)

{-# INLINE_NORMAL maximumBy #-}
maximumBy :: Monad m => (a -> a -> Ordering) -> Stream m a -> m (Maybe a)
maximumBy cmp (Stream step state) = go Nothing state
  where
    go Nothing st = do
        r <- step defState st
        case r of
            Yield x s -> go (Just x) s
            Skip  s   -> go Nothing s
            Stop      -> return Nothing
    go (Just acc) st = do
        r <- step defState st
        case r of
            Yield x s -> case cmp acc x of
                GT -> go (Just acc) s
                _  -> go (Just x) s
            Skip s -> go (Just acc) s
            Stop   -> return (Just acc)

{-# INLINE_NORMAL minimum #-}
minimum :: (Monad m, Ord a) => Stream m a -> m (Maybe a)
minimum (Stream step state) = go Nothing state
  where
    go Nothing st = do
        r <- step defState st
        case r of
            Yield x s -> go (Just x) s
            Skip  s   -> go Nothing s
            Stop      -> return Nothing
    go (Just acc) st = do
        r <- step defState st
        case r of
            Yield x s
              | acc <= x  -> go (Just acc) s
              | otherwise -> go (Just x) s
            Skip s -> go (Just acc) s
            Stop   -> return (Just acc)

{-# INLINE_NORMAL minimumBy #-}
minimumBy :: Monad m => (a -> a -> Ordering) -> Stream m a -> m (Maybe a)
minimumBy cmp (Stream step state) = go Nothing state
  where
    go Nothing st = do
        r <- step defState st
        case r of
            Yield x s -> go (Just x) s
            Skip  s   -> go Nothing s
            Stop      -> return Nothing
    go (Just acc) st = do
        r <- step defState st
        case r of
            Yield x s -> case cmp acc x of
                GT -> go (Just x) s
                _  -> go (Just acc) s
            Skip s -> go (Just acc) s
            Stop   -> return (Just acc)

{-# INLINE_NORMAL (!!) #-}
(!!) :: (Monad m) => Stream m a -> Int -> m (Maybe a)
(Stream step state) !! i = go i state
  where
    go n st = do
        r <- step defState st
        case r of
            Yield x s | n < 0 -> return Nothing
                      | n == 0 -> return $ Just x
                      | otherwise -> go (n - 1) s
            Skip s -> go n s
            Stop   -> return Nothing

{-# INLINE_NORMAL lookup #-}
lookup :: (Monad m, Eq a) => a -> Stream m (a, b) -> m (Maybe b)
lookup e m = foldrM (\(a, b) xs -> if e == a then return (Just b) else xs)
                   (return Nothing) m

{-# INLINE_NORMAL findM #-}
findM :: Monad m => (a -> m Bool) -> Stream m a -> m (Maybe a)
findM p m = foldrM (\x xs -> p x >>= \r -> if r then return (Just x) else xs)
                   (return Nothing) m

{-# INLINE find #-}
find :: Monad m => (a -> Bool) -> Stream m a -> m (Maybe a)
find p = findM (return . p)

{-# INLINE toListRev #-}
toListRev :: Monad m => Stream m a -> m [a]
toListRev = foldl' (flip (:)) []

------------------------------------------------------------------------------
-- Transformation comprehensions
------------------------------------------------------------------------------

{-# INLINE_NORMAL the #-}
the :: (Eq a, Monad m) => Stream m a -> m (Maybe a)
the (Stream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield x s -> go' x s
            Skip s    -> go s
            Stop      -> return Nothing
    go' n st = do
        r <- step defState st
        case r of
            Yield x s | x == n -> go' n s
                      | otherwise -> return Nothing
            Skip s -> go' n s
            Stop   -> return (Just n)

------------------------------------------------------------------------------
-- Map and Fold
------------------------------------------------------------------------------

-- | Execute a monadic action for each element of the 'Stream'
{-# INLINE_NORMAL mapM_ #-}
mapM_ :: Monad m => (a -> m b) -> Stream m a -> m ()
mapM_ m = drain . mapM m

------------------------------------------------------------------------------
-- Multi-stream folds
------------------------------------------------------------------------------

{-# INLINE_NORMAL isPrefixOf #-}
isPrefixOf :: (Eq a, Monad m) => Stream m a -> Stream m a -> m Bool
isPrefixOf (Stream stepa ta) (Stream stepb tb) = go (ta, tb, Nothing)
  where
    go (sa, sb, Nothing) = do
        r <- stepa defState sa
        case r of
            Yield x sa' -> go (sa', sb, Just x)
            Skip sa'    -> go (sa', sb, Nothing)
            Stop        -> return True

    go (sa, sb, Just x) = do
        r <- stepb defState sb
        case r of
            Yield y sb' ->
                if x == y
                    then go (sa, sb', Nothing)
                    else return False
            Skip sb' -> go (sa, sb', Just x)
            Stop     -> return False

{-# INLINE_NORMAL isSubsequenceOf #-}
isSubsequenceOf :: (Eq a, Monad m) => Stream m a -> Stream m a -> m Bool
isSubsequenceOf (Stream stepa ta) (Stream stepb tb) = go (ta, tb, Nothing)
  where
    go (sa, sb, Nothing) = do
        r <- stepa defState sa
        case r of
            Yield x sa' -> go (sa', sb, Just x)
            Skip sa'    -> go (sa', sb, Nothing)
            Stop        -> return True

    go (sa, sb, Just x) = do
        r <- stepb defState sb
        case r of
            Yield y sb' ->
                if x == y
                    then go (sa, sb', Nothing)
                    else go (sa, sb', Just x)
            Skip sb' -> go (sa, sb', Just x)
            Stop     -> return False

{-# INLINE_NORMAL stripPrefix #-}
stripPrefix
    :: (Eq a, Monad m)
    => Stream m a -> Stream m a -> m (Maybe (Stream m a))
stripPrefix (Stream stepa ta) (Stream stepb tb) = go (ta, tb, Nothing)
  where
    go (sa, sb, Nothing) = do
        r <- stepa defState sa
        case r of
            Yield x sa' -> go (sa', sb, Just x)
            Skip sa'    -> go (sa', sb, Nothing)
            Stop        -> return $ Just (Stream stepb sb)

    go (sa, sb, Just x) = do
        r <- stepb defState sb
        case r of
            Yield y sb' ->
                if x == y
                    then go (sa, sb', Nothing)
                    else return Nothing
            Skip sb' -> go (sa, sb', Just x)
            Stop     -> return Nothing
