{-# LANGUAGE UndecidableInstances #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.StreamK
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
-- Continuation passing style (CPS) stream implementation. The symbol 'K' below
-- denotes a function as well as a Kontinuation.
--
-- @
-- import qualified Streamly.Internal.Data.Stream.StreamK as K
-- @
--
module Streamly.Internal.Data.Stream.StreamK
    (
    -- * A class for streams
      IsStream (..)
    , adapt

    -- * The stream type
    , Stream(..)

    -- * Construction Primitives
    , mkStream
    , nil
    , nilM
    , cons
    , (.:)

    -- * Elimination Primitives
    , foldStream
    , foldStreamShared

    -- * Transformation Primitives
    , unShare

    -- * Deconstruction
    , uncons

    -- * Generation
    -- ** Unfolds
    , unfoldr
    , unfoldrM

    -- ** Specialized Generation
    , repeat
    , repeatM
    , replicate
    , replicateM
    , fromIndices
    , fromIndicesM
    , iterate
    , iterateM

    -- ** Conversions
    , yield
    , yieldM
    , fromFoldable
    , fromList
    , fromStreamK

    -- * foldr/build
    , foldrS
    , foldrSM
    , buildS
    , buildM
    , augmentS
    , augmentSM

    -- * Elimination
    -- ** General Folds
    , foldr
    , foldr1
    , foldrM
    , foldrT

    , foldl'
    , foldlM'
    , foldlS
    , foldlT
    , foldlx'
    , foldlMx'
    , fold

    -- ** Specialized Folds
    , drain
    , null
    , head
    , tail
    , init
    , elem
    , notElem
    , all
    , any
    , last
    , minimum
    , minimumBy
    , maximum
    , maximumBy
    , findIndices
    , lookup
    , findM
    , find
    , (!!)

    -- ** Map and Fold
    , mapM_

    -- ** Conversions
    , toList
    , toStreamK
    , hoist

    -- * Transformation
    -- ** By folding (scans)
    , scanl'
    , scanlx'

    -- ** Filtering
    , filter
    , take
    , takeWhile
    , drop
    , dropWhile

    -- ** Mapping
    , map
    , mapM
    , mapMSerial
    , sequence

    -- ** Inserting
    , intersperseM
    , intersperse
    , insertBy

    -- ** Deleting
    , deleteBy

    -- ** Reordering
    , reverse

    -- ** Map and Filter
    , mapMaybe

    -- ** Zipping
    , zipWith
    , zipWithM

    -- ** Merging
    , mergeBy
    , mergeByM

    -- ** Nesting
    , concatMapBy
    , concatMap
    , bindWith
    , concatPairsWith
    , apWith
    , apSerial
    , apSerialDiscardFst
    , apSerialDiscardSnd

    -- ** Transformation comprehensions
    , the

    -- * Semigroup Style Composition
    , serial

    -- * Utilities
    , consMStream
    , withLocal
    , mfix

    -- * Deprecated
    , Streaming -- deprecated
    , once      -- deprecated
    )
where

import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad (void, join)
import Control.Monad.Reader.Class  (MonadReader(..))
import Data.Function (fix)
import Prelude
       hiding (foldl, foldr, last, map, mapM, mapM_, repeat, sequence,
               take, filter, all, any, takeWhile, drop, dropWhile, minimum,
               maximum, elem, notElem, null, head, tail, init, zipWith, lookup,
               foldr1, (!!), replicate, reverse, concatMap, iterate)
import qualified Prelude

import Streamly.Internal.Data.SVar
import Streamly.Internal.Data.Stream.StreamK.Type

import qualified Streamly.Internal.Data.Fold.Type as FL

-- $setup
-- >>> :m

-------------------------------------------------------------------------------
-- Deconstruction
-------------------------------------------------------------------------------

{-# INLINE uncons #-}
uncons :: (IsStream t, Monad m) => t m a -> m (Maybe (a, t m a))
uncons m =
    let stop = return Nothing
        single a = return (Just (a, nil))
        yieldk a r = return (Just (a, r))
    in foldStream defState yieldk single stop m

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

{-# INLINE unfoldr #-}
unfoldr :: IsStream t => (b -> Maybe (a, b)) -> b -> t m a
unfoldr next s0 = build $ \yld stp ->
    let go s =
            case next s of
                Just (a, b) -> yld a (go b)
                Nothing -> stp
    in go s0

{-# INLINE unfoldrM #-}
unfoldrM :: (IsStream t, MonadAsync m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM step = go
    where
    go s = sharedM $ \yld _ stp -> do
                r <- step s
                case r of
                    Just (a, b) -> yld a (go b)
                    Nothing -> stp

{-
-- Generalization of concurrent streams/SVar via unfoldr.
--
-- Unfold a value into monadic actions and then run the resulting monadic
-- actions to generate a stream. Since the step of generating the monadic
-- action and running them are decoupled we can run the monadic actions
-- cooncurrently. For example, the seed could be a list of monadic actions or a
-- pure stream of monadic actions.
--
-- We can have different flavors of this depending on the stream type t. The
-- concurrent version could be async or ahead etc. Depending on how we queue
-- back the feedback portion b, it could be DFS or BFS style.
--
unfoldrA :: (IsStream t, MonadAsync m) => (b -> Maybe (m a, b)) -> b -> t m a
unfoldrA = undefined
-}

-------------------------------------------------------------------------------
-- Special generation
-------------------------------------------------------------------------------

-- | Same as yieldM
--
-- @since 0.2.0
{-# DEPRECATED once "Please use yieldM instead." #-}
{-# INLINE once #-}
once :: (Monad m, IsStream t) => m a -> t m a
once = yieldM

-- |
-- @
-- repeatM = fix . cons
-- repeatM = cycle1 . yield
-- @
--
-- Generate an infinite stream by repeating a monadic value.
--
-- /Pre-release/
repeatM :: (IsStream t, MonadAsync m) => m a -> t m a
repeatM = go
    where go m = m |: go m

-- Generate an infinite stream by repeating a pure value.
--
-- /Pre-release/
{-# INLINE repeat #-}
repeat :: IsStream t => a -> t m a
repeat a = let x = cons a x in x

{-# INLINE replicateM #-}
replicateM :: (IsStream t, MonadAsync m) => Int -> m a -> t m a
replicateM n m = go n
    where
    go cnt = if cnt <= 0 then nil else m |: go (cnt - 1)

{-# INLINE replicate #-}
replicate :: IsStream t => Int -> a -> t m a
replicate n a = go n
    where
    go cnt = if cnt <= 0 then nil else a `cons` go (cnt - 1)

{-# INLINE fromIndicesM #-}
fromIndicesM :: (IsStream t, MonadAsync m) => (Int -> m a) -> t m a
fromIndicesM gen = go 0
  where
    go i = mkStream $ \st stp sng yld -> do
        foldStreamShared st stp sng yld (gen i |: go (i + 1))

{-# INLINE fromIndices #-}
fromIndices :: IsStream t => (Int -> a) -> t m a
fromIndices gen = go 0
  where
    go n = (gen n) `cons` go (n + 1)

{-# INLINE iterate #-}
iterate :: IsStream t => (a -> a) -> a -> t m a
iterate step = fromStream . go
    where
        go s = cons s (go (step s))

{-# INLINE iterateM #-}
iterateM :: (IsStream t, MonadAsync m) => (a -> m a) -> m a -> t m a
iterateM step = go
    where
    go s = mkStream $ \st stp sng yld -> do
        next <- s
        foldStreamShared st stp sng yld (return next |: go (step next))

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- |
-- @
-- fromFoldable = 'Prelude.foldr' 'cons' 'nil'
-- @
--
-- Construct a stream from a 'Foldable' containing pure values:
--
-- @since 0.2.0
{-# INLINE fromFoldable #-}
fromFoldable :: (IsStream t, Foldable f) => f a -> t m a
fromFoldable = Prelude.foldr cons nil

{-# INLINE fromList #-}
fromList :: IsStream t => [a] -> t m a
fromList = fromFoldable

{-# INLINE fromStreamK #-}
fromStreamK :: IsStream t => Stream m a -> t m a
fromStreamK = fromStream

-------------------------------------------------------------------------------
-- Elimination by Folding
-------------------------------------------------------------------------------

-- | Lazy right associative fold.
{-# INLINE foldr #-}
foldr :: (IsStream t, Monad m) => (a -> b -> b) -> b -> t m a -> m b
foldr step acc = foldrM (\x xs -> xs >>= \b -> return (step x b)) (return acc)

-- | Right associative fold to an arbitrary transformer monad.
{-# INLINE foldrT #-}
foldrT :: (IsStream t, Monad m, Monad (s m), MonadTrans s)
    => (a -> s m b -> s m b) -> s m b -> t m a -> s m b
foldrT step final m = go m
  where
    go m1 = do
        res <- lift $ uncons m1
        case res of
            Just (h, t) -> step h (go t)
            Nothing -> final

{-# INLINE foldr1 #-}
foldr1 :: (IsStream t, Monad m) => (a -> a -> a) -> t m a -> m (Maybe a)
foldr1 step m = do
    r <- uncons m
    case r of
        Nothing -> return Nothing
        Just (h, t) -> fmap Just (go h t)
    where
    go p m1 =
        let stp = return p
            single a = return $ step a p
            yieldk a r = fmap (step p) (go a r)
         in foldStream defState yieldk single stp m1

-- XXX replace the recursive "go" with explicit continuations.
-- | Like 'foldx', but with a monadic step function.
{-# INLINABLE foldlMx' #-}
foldlMx' :: (IsStream t, Monad m)
    => (x -> a -> m x) -> m x -> (x -> m b) -> t m a -> m b
foldlMx' step begin done m = go begin m
    where
    go !acc m1 =
        let stop = acc >>= done
            single a = acc >>= \b -> step b a >>= done
            yieldk a r = acc >>= \b -> step b a >>= \x -> go (return x) r
         in foldStream defState yieldk single stop m1

{-# INLINABLE fold #-}
fold :: (IsStream t, Monad m) => FL.Fold m a b -> t m a -> m b
fold (FL.Fold step begin done) m = do
    res <- begin
    case res of
        FL.Partial fs -> go fs m
        FL.Done fb -> return fb

    where
    go !acc m1 =
        let stop = done acc
            single a = step acc a
              >>= \case
                        FL.Partial s -> done s
                        FL.Done b1 -> return b1
            yieldk a r = step acc a
              >>= \case
                        FL.Partial s -> go s r
                        FL.Done b1 -> return b1
         in foldStream defState yieldk single stop m1

-- | Like 'foldl'' but with a monadic step function.
{-# INLINE foldlM' #-}
foldlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> m b -> t m a -> m b
foldlM' step begin = foldlMx' step begin return

-- | Lazy left fold to a stream.
{-# INLINE foldlS #-}
foldlS :: IsStream t => (t m b -> a -> t m b) -> t m b -> t m a -> t m b
foldlS step begin m = go begin m
    where
    go acc rest = mkStream $ \st yld sng stp ->
        let run x = foldStream st yld sng stp x
            stop = run acc
            single a = run $ step acc a
            yieldk a r = run $ go (step acc a) r
         in foldStream (adaptState st) yieldk single stop rest

-- | Lazy left fold to an arbitrary transformer monad.
{-# INLINE foldlT #-}
foldlT :: (IsStream t, Monad m, Monad (s m), MonadTrans s)
    => (s m b -> a -> s m b) -> s m b -> t m a -> s m b
foldlT step begin m = go begin m
  where
    go acc m1 = do
        res <- lift $ uncons m1
        case res of
            Just (h, t) -> go (step acc h) t
            Nothing -> acc

------------------------------------------------------------------------------
-- Specialized folds
------------------------------------------------------------------------------

-- XXX use foldrM to implement folds where possible
-- XXX This (commented) definition of drain and mapM_ perform much better on
-- some benchmarks but worse on others. Need to investigate why, may there is
-- an optimization opportunity that we can exploit.
-- drain = foldrM (\_ xs -> return () >> xs) (return ())

-- |
-- > drain = foldl' (\_ _ -> ()) ()
-- > drain = mapM_ (\_ -> return ())
{-# INLINE drain #-}
drain :: (Monad m, IsStream t) => t m a -> m ()
drain = foldrM (\_ xs -> xs) (return ())
{-
drain = go
    where
    go m1 =
        let stop = return ()
            single _ = return ()
            yieldk _ r = go r
         in foldStream defState yieldk single stop m1
-}

{-# INLINE null #-}
null :: (IsStream t, Monad m) => t m a -> m Bool
-- null = foldrM (\_ _ -> return True) (return False)
null m =
    let stop      = return True
        single _  = return False
        yieldk _ _ = return False
    in foldStream defState yieldk single stop m

{-# INLINE head #-}
head :: (IsStream t, Monad m) => t m a -> m (Maybe a)
-- head = foldrM (\x _ -> return $ Just x) (return Nothing)
head m =
    let stop      = return Nothing
        single a  = return (Just a)
        yieldk a _ = return (Just a)
    in foldStream defState yieldk single stop m

{-# INLINE tail #-}
tail :: (IsStream t, Monad m) => t m a -> m (Maybe (t m a))
tail m =
    let stop      = return Nothing
        single _  = return $ Just nil
        yieldk _ r = return $ Just r
    in foldStream defState yieldk single stop m

{-# INLINE headPartial #-}
headPartial :: (IsStream t, Monad m) => t m a -> m a
headPartial = foldrM (\x _ -> return x) (error "head of nil")

{-# INLINE tailPartial #-}
tailPartial :: IsStream t => t m a -> t m a
tailPartial m = mkStream $ \st yld sng stp ->
    let stop      = error "tail of nil"
        single _  = stp
        yieldk _ r = foldStream st yld sng stp r
    in foldStream st yieldk single stop m

-- | We can define cyclic structures using @let@:
--
-- >>> let (a, b) = ([1, b], head a) in (a, b)
-- ([1,1],1)
--
-- The function @fix@ defined as:
--
-- > fix f = let x = f x in x
--
-- ensures that the argument of a function and its output refer to the same
-- lazy value @x@ i.e.  the same location in memory.  Thus @x@ can be defined
-- in terms of itself, creating structures with cyclic references.
--
-- >>> import Data.Function (fix)
-- >>> f ~(a, b) = ([1, b], head a)
-- >>> fix f
-- ([1,1],1)
--
-- 'Control.Monad.mfix' is essentially the same as @fix@ but for monadic
-- values.
--
-- Using 'mfix' for streams we can construct a stream in which each element of
-- the stream is defined in a cyclic fashion. The argument of the function
-- being fixed represents the current element of the stream which is being
-- returned by the stream monad. Thus, we can use the argument to construct
-- itself.
--
-- In the following example, the argument @action@ of the function @f@
-- represents the tuple @(x,y)@ returned by it in a given iteration. We define
-- the first element of the tuple in terms of the second.
--
-- @
-- import Streamly.Internal.Data.Stream.IsStream as Stream
-- import System.IO.Unsafe (unsafeInterleaveIO)
--
-- main = do
--     Stream.mapM_ print $ Stream.mfix f
--
--     where
--
--     f action = do
--         let incr n act = fmap ((+n) . snd) $ unsafeInterleaveIO act
--         x <- Stream.fromListM [incr 1 action, incr 2 action]
--         y <- Stream.fromList [4,5]
--         return (x, y)
-- @
--
-- Note: you cannot achieve this by just changing the order of the monad
-- statements because that would change the order in which the stream elements
-- are generated.
--
-- Note that the function @f@ must be lazy in its argument, that's why we use
-- 'unsafeInterleaveIO' on @action@ because IO monad is strict.
--
-- /Pre-release/

mfix :: (IsStream t, Monad m) => (m a -> t m a) -> t m a
mfix f = mkStream $ \st yld sng stp ->
    let single a  = foldStream st yld sng stp $ a `cons` ys
        yieldk a _ = foldStream st yld sng stp $ a `cons` ys
    in foldStream st yieldk single stp xs

    where

    -- fix the head element of the stream
    xs = fix  (f . headPartial)

    -- now fix the tail recursively
    ys = mfix (tailPartial . f)

{-# INLINE init #-}
init :: (IsStream t, Monad m) => t m a -> m (Maybe (t m a))
init m = go1 m
    where
    go1 m1 = do
        r <- uncons m1
        case r of
            Nothing -> return Nothing
            Just (h, t) -> return . Just $ go h t
    go p m1 = mkStream $ \_ yld sng stp ->
        let single _ = sng p
            yieldk a x = yld p $ go a x
         in foldStream defState yieldk single stp m1

{-# INLINE elem #-}
elem :: (IsStream t, Monad m, Eq a) => a -> t m a -> m Bool
elem e m = go m
    where
    go m1 =
        let stop      = return False
            single a  = return (a == e)
            yieldk a r = if a == e then return True else go r
        in foldStream defState yieldk single stop m1

{-# INLINE notElem #-}
notElem :: (IsStream t, Monad m, Eq a) => a -> t m a -> m Bool
notElem e m = go m
    where
    go m1 =
        let stop      = return True
            single a  = return (a /= e)
            yieldk a r = if a == e then return False else go r
        in foldStream defState yieldk single stop m1

{-# INLINABLE all #-}
all :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> m Bool
all p m = go m
    where
    go m1 =
        let single a   | p a       = return True
                       | otherwise = return False
            yieldk a r | p a       = go r
                       | otherwise = return False
         in foldStream defState yieldk single (return True) m1

{-# INLINABLE any #-}
any :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> m Bool
any p m = go m
    where
    go m1 =
        let single a   | p a       = return True
                       | otherwise = return False
            yieldk a r | p a       = return True
                       | otherwise = go r
         in foldStream defState yieldk single (return False) m1

-- | Extract the last element of the stream, if any.
{-# INLINE last #-}
last :: (IsStream t, Monad m) => t m a -> m (Maybe a)
last = foldlx' (\_ y -> Just y) Nothing id

{-# INLINE minimum #-}
minimum :: (IsStream t, Monad m, Ord a) => t m a -> m (Maybe a)
minimum m = go Nothing m
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r = go (Just a) r
        in foldStream defState yieldk single stop m1

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  =
                if res <= a
                then return (Just res)
                else return (Just a)
            yieldk a r =
                if res <= a
                then go (Just res) r
                else go (Just a) r
        in foldStream defState yieldk single stop m1

{-# INLINE minimumBy #-}
minimumBy
    :: (IsStream t, Monad m)
    => (a -> a -> Ordering) -> t m a -> m (Maybe a)
minimumBy cmp m = go Nothing m
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r = go (Just a) r
        in foldStream defState yieldk single stop m1

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  = case cmp res a of
                GT -> return (Just a)
                _  -> return (Just res)
            yieldk a r = case cmp res a of
                GT -> go (Just a) r
                _  -> go (Just res) r
        in foldStream defState yieldk single stop m1

{-# INLINE maximum #-}
maximum :: (IsStream t, Monad m, Ord a) => t m a -> m (Maybe a)
maximum m = go Nothing m
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r = go (Just a) r
        in foldStream defState yieldk single stop m1

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  =
                if res <= a
                then return (Just a)
                else return (Just res)
            yieldk a r =
                if res <= a
                then go (Just a) r
                else go (Just res) r
        in foldStream defState yieldk single stop m1

{-# INLINE maximumBy #-}
maximumBy :: (IsStream t, Monad m) => (a -> a -> Ordering) -> t m a -> m (Maybe a)
maximumBy cmp m = go Nothing m
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r = go (Just a) r
        in foldStream defState yieldk single stop m1

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  = case cmp res a of
                GT -> return (Just res)
                _  -> return (Just a)
            yieldk a r = case cmp res a of
                GT -> go (Just res) r
                _  -> go (Just a) r
        in foldStream defState yieldk single stop m1

{-# INLINE (!!) #-}
(!!) :: (IsStream t, Monad m) => t m a -> Int -> m (Maybe a)
m !! i = go i m
    where
    go n m1 =
      let single a | n == 0 = return $ Just a
                   | otherwise = return Nothing
          yieldk a x | n < 0 = return Nothing
                     | n == 0 = return $ Just a
                     | otherwise = go (n - 1) x
      in foldStream defState yieldk single (return Nothing) m1

{-# INLINE lookup #-}
lookup :: (IsStream t, Monad m, Eq a) => a -> t m (a, b) -> m (Maybe b)
lookup e m = go m
    where
    go m1 =
        let single (a, b) | a == e = return $ Just b
                          | otherwise = return Nothing
            yieldk (a, b) x | a == e = return $ Just b
                            | otherwise = go x
        in foldStream defState yieldk single (return Nothing) m1

{-# INLINE findM #-}
findM :: (IsStream t, Monad m) => (a -> m Bool) -> t m a -> m (Maybe a)
findM p m = go m
    where
    go m1 =
        let single a = do
                b <- p a
                if b then return $ Just a else return Nothing
            yieldk a x = do
                b <- p a
                if b then return $ Just a else go x
        in foldStream defState yieldk single (return Nothing) m1

{-# INLINE find #-}
find :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> m (Maybe a)
find p = findM (return . p)

{-# INLINE findIndices #-}
findIndices :: IsStream t => (a -> Bool) -> t m a -> t m Int
findIndices p = go 0
    where
    go offset m1 = mkStream $ \st yld sng stp ->
        let single a | p a = sng offset
                     | otherwise = stp
            yieldk a x | p a = yld offset $ go (offset + 1) x
                       | otherwise = foldStream (adaptState st) yld sng stp $
                            go (offset + 1) x
        in foldStream (adaptState st) yieldk single stp m1

------------------------------------------------------------------------------
-- Map and Fold
------------------------------------------------------------------------------

-- | Apply a monadic action to each element of the stream and discard the
-- output of the action.
{-# INLINE mapM_ #-}
mapM_ :: (IsStream t, Monad m) => (a -> m b) -> t m a -> m ()
mapM_ f m = go m
    where
    go m1 =
        let stop = return ()
            single a = void (f a)
            yieldk a r = f a >> go r
         in foldStream defState yieldk single stop m1

------------------------------------------------------------------------------
-- Converting folds
------------------------------------------------------------------------------

{-# INLINABLE toList #-}
toList :: (IsStream t, Monad m) => t m a -> m [a]
toList = foldr (:) []

{-# INLINE toStreamK #-}
toStreamK :: Stream m a -> Stream m a
toStreamK = id

-- Based on suggestions by David Feuer and Pranay Sashank
{-# INLINE hoist #-}
hoist :: (IsStream t, Monad m, Monad n)
    => (forall x. m x -> n x) -> t m a -> t n a
hoist f str =
    mkStream $ \st yld sng stp ->
            let single = return . sng
                yieldk a s = return $ yld a (hoist f s)
                stop = return stp
                state = adaptState st
             in join . f $ foldStreamShared state yieldk single stop str

-------------------------------------------------------------------------------
-- Transformation by folding (Scans)
-------------------------------------------------------------------------------

{-# INLINE scanlx' #-}
scanlx' :: IsStream t => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scanlx' step begin done m =
    cons (done begin) $ go m begin
    where
    go m1 !acc = mkStream $ \st yld sng stp ->
        let single a = sng (done $ step acc a)
            yieldk a r =
                let s = step acc a
                in yld (done s) (go r s)
        in foldStream (adaptState st) yieldk single stp m1

{-# INLINE scanl' #-}
scanl' :: IsStream t => (b -> a -> b) -> b -> t m a -> t m b
scanl' step begin = scanlx' step begin id

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# INLINE filter #-}
filter :: IsStream t => (a -> Bool) -> t m a -> t m a
filter p m = go m
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single a   | p a       = sng a
                       | otherwise = stp
            yieldk a r | p a       = yld a (go r)
                       | otherwise = foldStream st yieldk single stp r
         in foldStream st yieldk single stp m1

{-# INLINE take #-}
take :: IsStream t => Int -> t m a -> t m a
take n m = go n m
    where
    go n1 m1 = mkStream $ \st yld sng stp ->
        let yieldk a r = yld a (go (n1 - 1) r)
        in if n1 <= 0
           then stp
           else foldStream st yieldk sng stp m1

{-# INLINE takeWhile #-}
takeWhile :: IsStream t => (a -> Bool) -> t m a -> t m a
takeWhile p m = go m
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single a   | p a       = sng a
                       | otherwise = stp
            yieldk a r | p a       = yld a (go r)
                       | otherwise = stp
         in foldStream st yieldk single stp m1

{-# INLINE drop #-}
drop :: IsStream t => Int -> t m a -> t m a
drop n m = fromStream $ unShare (go n (toStream m))
    where
    go n1 m1 = mkStream $ \st yld sng stp ->
        let single _ = stp
            yieldk _ r = foldStreamShared st yld sng stp $ go (n1 - 1) r
        -- Somehow "<=" check performs better than a ">"
        in if n1 <= 0
           then foldStreamShared st yld sng stp m1
           else foldStreamShared st yieldk single stp m1

{-# INLINE dropWhile #-}
dropWhile :: IsStream t => (a -> Bool) -> t m a -> t m a
dropWhile p m = go m
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single a   | p a       = stp
                       | otherwise = sng a
            yieldk a r | p a = foldStream st yieldk single stp r
                       | otherwise = yld a r
         in foldStream st yieldk single stp m1

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

-- Be careful when modifying this, this uses a consM (|:) deliberately to allow
-- other stream types to overload it.
{-# INLINE sequence #-}
sequence :: (IsStream t, MonadAsync m) => t m (m a) -> t m a
sequence m = go m
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single ma = ma >>= sng
            yieldk ma r = foldStreamShared st yld sng stp $ ma |: go r
         in foldStream (adaptState st) yieldk single stp m1

-------------------------------------------------------------------------------
-- Inserting
-------------------------------------------------------------------------------

{-# INLINE intersperseM #-}
intersperseM :: (IsStream t, MonadAsync m) => m a -> t m a -> t m a
intersperseM a m = prependingStart m
    where
    prependingStart m1 = mkStream $ \st yld sng stp ->
        let yieldk i x = foldStreamShared st yld sng stp $ return i |: go x
         in foldStream st yieldk sng stp m1
    go m2 = mkStream $ \st yld sng stp ->
        let single i = foldStreamShared st yld sng stp $ a |: yield i
            yieldk i x = foldStreamShared st yld sng stp $ a |: return i |: go x
         in foldStream st yieldk single stp m2

{-# INLINE intersperse #-}
intersperse :: (IsStream t, MonadAsync m) => a -> t m a -> t m a
intersperse a = intersperseM (return a)

{-# INLINE insertBy #-}
insertBy :: IsStream t => (a -> a -> Ordering) -> a -> t m a -> t m a
insertBy cmp x m = go m
  where
    go m1 = mkStream $ \st yld _ _ ->
        let single a = case cmp x a of
                GT -> yld a (yield x)
                _  -> yld x (yield a)
            stop = yld x nil
            yieldk a r = case cmp x a of
                GT -> yld a (go r)
                _  -> yld x (a `cons` r)
         in foldStream st yieldk single stop m1

------------------------------------------------------------------------------
-- Deleting
------------------------------------------------------------------------------

{-# INLINE deleteBy #-}
deleteBy :: IsStream t => (a -> a -> Bool) -> a -> t m a -> t m a
deleteBy eq x m = go m
  where
    go m1 = mkStream $ \st yld sng stp ->
        let single a = if eq x a then stp else sng a
            yieldk a r = if eq x a
              then foldStream st yld sng stp r
              else yld a (go r)
         in foldStream st yieldk single stp m1

------------------------------------------------------------------------------
-- Reordering
------------------------------------------------------------------------------

{-# INLINE reverse #-}
reverse :: IsStream t => t m a -> t m a
reverse = foldlS (flip cons) nil

-------------------------------------------------------------------------------
-- Map and Filter
-------------------------------------------------------------------------------

{-# INLINE mapMaybe #-}
mapMaybe :: IsStream t => (a -> Maybe b) -> t m a -> t m b
mapMaybe f m = go m
  where
    go m1 = mkStream $ \st yld sng stp ->
        let single a = case f a of
                Just b  -> sng b
                Nothing -> stp
            yieldk a r = case f a of
                Just b  -> yld b $ go r
                Nothing -> foldStream (adaptState st) yieldk single stp r
        in foldStream (adaptState st) yieldk single stp m1

------------------------------------------------------------------------------
-- Serial Zipping
------------------------------------------------------------------------------

-- | Zip two streams serially using a pure zipping function.
--
-- @since 0.1.0
{-# INLINABLE zipWith #-}
zipWith :: IsStream t => (a -> b -> c) -> t m a -> t m b -> t m c
zipWith f = go
    where
    go mx my = mkStream $ \st yld sng stp -> do
        let merge a ra =
                let single2 b = sng (f a b)
                    yield2 b rb = yld (f a b) (go ra rb)
                 in foldStream (adaptState st) yield2 single2 stp my
        let single1 a = merge a nil
            yield1 = merge
        foldStream (adaptState st) yield1 single1 stp mx

-- | Zip two streams serially using a monadic zipping function.
--
-- @since 0.1.0
{-# INLINABLE zipWithM #-}
zipWithM :: (IsStream t, Monad m) => (a -> b -> m c) -> t m a -> t m b -> t m c
zipWithM f m1 m2 = go m1 m2
    where
    go mx my = mkStream $ \st yld sng stp -> do
        let merge a ra =
                let runIt x = foldStream st yld sng stp x
                    single2 b   = f a b >>= sng
                    yield2 b rb = f a b >>= \x -> runIt (x `cons` go ra rb)
                 in foldStream (adaptState st) yield2 single2 stp my
        let single1 a = merge a nil
            yield1 = merge
        foldStream (adaptState st) yield1 single1 stp mx

------------------------------------------------------------------------------
-- Merging
------------------------------------------------------------------------------

{-# INLINE mergeByM #-}
mergeByM
    :: (IsStream t, Monad m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeByM cmp = go
    where
    go mx my = mkStream $ \st yld sng stp -> do
        let mergeWithY a ra =
                let stop2 = foldStream st yld sng stp mx
                    single2 b = do
                        r <- cmp a b
                        case r of
                            GT -> yld b (go (a `cons` ra) nil)
                            _  -> yld a (go ra (b `cons` nil))
                    yield2 b rb = do
                        r <- cmp a b
                        case r of
                            GT -> yld b (go (a `cons` ra) rb)
                            _  -> yld a (go ra (b `cons` rb))
                 in foldStream st yield2 single2 stop2 my
        let stopX = foldStream st yld sng stp my
            singleX a = mergeWithY a nil
            yieldX = mergeWithY
        foldStream st yieldX singleX stopX mx

{-# INLINABLE mergeBy #-}
mergeBy
    :: (IsStream t, Monad m)
    => (a -> a -> Ordering) -> t m a -> t m a -> t m a
mergeBy cmp = mergeByM (\a b -> return $ cmp a b)

------------------------------------------------------------------------------
-- Transformation comprehensions
------------------------------------------------------------------------------

{-# INLINE the #-}
the :: (Eq a, IsStream t, Monad m) => t m a -> m (Maybe a)
the m = do
    r <- uncons m
    case r of
        Nothing -> return Nothing
        Just (h, t) -> go h t
    where
    go h m1 =
        let single a   | h == a    = return $ Just h
                       | otherwise = return Nothing
            yieldk a r | h == a    = go h r
                       | otherwise = return Nothing
         in foldStream defState yieldk single (return $ Just h) m1

------------------------------------------------------------------------------
-- Alternative & MonadPlus
------------------------------------------------------------------------------

_alt :: Stream m a -> Stream m a -> Stream m a
_alt m1 m2 = mkStream $ \st yld sng stp ->
    let stop  = foldStream st yld sng stp m2
    in foldStream st yld sng stop m1

------------------------------------------------------------------------------
-- MonadReader
------------------------------------------------------------------------------

{-# INLINABLE withLocal #-}
withLocal :: MonadReader r m => (r -> r) -> Stream m a -> Stream m a
withLocal f m =
    mkStream $ \st yld sng stp ->
        let single = local f . sng
            yieldk a r = local f $ yld a (withLocal f r)
        in foldStream st yieldk single (local f stp) m

------------------------------------------------------------------------------
-- MonadError
------------------------------------------------------------------------------

{-
-- XXX handle and test cross thread state transfer
withCatchError
    :: MonadError e m
    => Stream m a -> (e -> Stream m a) -> Stream m a
withCatchError m h =
    mkStream $ \_ stp sng yld ->
        let run x = unStream x Nothing stp sng yieldk
            handle r = r `catchError` \e -> run $ h e
            yieldk a r = yld a (withCatchError r h)
        in handle $ run m
-}
