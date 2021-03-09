-- |
-- Module      : Streamly.Internal.Data.Fold.Types
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- = Stream Consumers
--
-- We can classify stream consumers in the following categories in order of
-- increasing complexity and power:
--
-- == Accumulators
--
-- These are the simplest folds that never fail and never terminate, they
-- accumulate the input values forever and can always accept new inputs (never
-- terminate) and always have a valid result value.  A
-- 'Streamly.Internal.Data.Fold.sum' operation is an example of an accumulator.
-- Traditional Haskell left folds like 'foldl' are accumulators.
--
-- We can distribute an input stream to two or more accumulators using a @tee@
-- style composition.  Accumulators cannot be applied on a stream one after the
-- other, which we call a @serial@ append style composition of folds. This is
-- because accumulators never terminate, since the first accumulator in a
-- series will never terminate, the next one will never get to run.
--
-- == Terminating Folds
--
-- Terminating folds are accumulators that can terminate. Once a fold
-- terminates it no longer accepts any more inputs.  Terminating folds can be
-- used in a @serial@ append style composition where one fold can be applied
-- after the other on an input stream. We can apply a terminating fold
-- repeatedly on an input stream, splitting the stream and consuming it in
-- fragments.  Terminating folds never fail, therefore, they do not need
-- backtracking.
--
-- The 'Streamly.Internal.Data.Fold.take' operation is an example of a
-- terminating fold  It terminates after consuming @n@ items. Coupled with an
-- accumulator (e.g. sum) it can be used to split and process the stream into
-- chunks of fixed size.
--
-- == Terminating Folds with Leftovers
--
-- The next upgrade after terminating folds is terminating folds with leftover
-- inputs.  Consider the example of @takeWhile@ operation, it needs to inspect
-- an element for termination decision. However, it does not consume the
-- element on which it terminates. To implement @takeWhile@ a terminating fold
-- will have to implement a way to return unconsumed input to the fold driver.
--
-- Single element leftover case is the most common and its easy to implement it
-- in terminating folds using a @Done1@ constructor in the 'Step' type which
-- indicates that the last element was not consumed by the fold. The following
-- additional operations can be implemented as terminating folds if we do that.
--
-- @
-- takeWhile
-- groupBy
-- wordBy
-- @
--
-- However, it creates several complications.  The 'many' combinator  requires
-- a @Partial1@ ('Partial' with leftover) to handle a @Done1@ from the top
-- level fold, for efficient implementation.  If the collecting fold in "many"
-- returns a @Partial1@ or @Done1@ then what to do with all the elements that
-- have been consumed?
--
-- Similarly, in distribute, if one fold consumes a value and others say its a
-- leftover then what do we do?  Folds like "many" require the leftover to be
-- fed to it again. So in a distribute operation those folds which gave a
-- leftover will have to be fed the leftover while the folds that consumed will
-- have to be fed the next input.  This is very complicated to implement. We
-- have the same issue in backtracking parsers being used in a distribute
-- operation.
--
-- To avoid these issues we want to enforce by typing that the collecting folds
-- can never return a leftover. So we need a fold type without @Done1@ or
-- @Partial1@. This leads us to design folds to never return a leftover and the
-- use cases of single leftover are transferred to parsers where we have
-- general backtracking mechanism and single leftover is just a special case of
-- backtracking.
--
-- This means: takeWhile, groupBy, wordBy would be implemented as parsers.
-- "take 0" can implemented as a fold if we make initial return @Step@ type.
-- "takeInterval" can be implemented without @Done1@.
--
-- == Parsers
--
-- The next upgrade after terminating folds with a leftover are parsers.
-- Parsers are terminating folds that can fail and backtrack. Parsers can be
-- composed using an @alternative@ style composition where they can backtrack
-- and apply another parser if one parser fails.
-- 'Streamly.Internal.Data.Parser.satisfy' is a simple example of a parser, it
-- would succeed if the condition is satisfied and it would fail otherwise, on
-- failure an alternative parser can be used on the same input.
--
-- = Types for Stream Consumers
--
-- In streamly, there is no separate type for accumulators. Terminating folds
-- are a superset of accumulators and to avoid too many types we represent both
-- using the same type, 'Fold'.
--
-- We do not club the leftovers functionality with terminating folds because of
-- the reasons explained earlier. Instead combinators that require leftovers
-- are implemented as the 'Streamly.Internal.Data.Parser.Parser' type.  This is
-- a sweet spot to balance ease of use, type safety and performance.  Using
-- separate Accumulator and terminating fold types would encode more
-- information in types but it would make ease of use, implementation,
-- maintenance effort worse. Combining Accumulator, terminating folds and
-- Parser into a single 'Streamly.Internal.Data.Parser.Parser' type would make
-- ease of use even better but type safety and performance worse.
--
-- One of the design requirements that we have placed for better ease of use
-- and code reuse is that 'Streamly.Internal.Data.Parser.Parser' type should be
-- a strict superset of the 'Fold' type i.e. it can do everything that a 'Fold'
-- can do and more. Therefore, folds can be easily upgraded to parsers and we
-- can use parser combinators on folds as well when needed.
--
-- = Fold Design
--
-- A fold is represented by a collection of "initial", "step" and "extract"
-- functions. The "initial" action generates the initial state of the fold. The
-- state is internal to the fold and maintains the accumulated output. The
-- "step" function is invoked using the current state and the next input value
-- and results in a @Partial@ or @Done@. A @Partial@ returns the next intermediate
-- state of the fold, a @Done@ indicates that the fold has terminated and
-- returns the final value of the accumulator.
--
-- Every @Partial@ indicates that a new accumulated output is available.  The
-- accumulated output can be extracted from the state at any point using
-- "extract". "extract" can never fail. A fold returns a valid output even
-- without any input i.e. even if you call "extract" on "initial" state it
-- provides an output. This is not true for parsers.
--
-- In general, "extract" is used in two cases:
--
-- * When the fold is used as a scan @extract@ is called on the intermediate
-- state every time it is yielded by the fold, the resulting value is yielded
-- as a stream.
-- * When the fold is used as a regular fold, @extract@ is called once when
-- we are done feeding input to the fold.
--
-- = Alternate Designs
--
-- An alternate and simpler design would be to return the intermediate output
-- via @Partial@ along with the state, instead of using "extract" on the yielded
-- state and remove the extract function altogether.
--
-- This may even facilitate more efficient implementation.  Extract from the
-- intermediate state after each yield may be more costly compared to the fold
-- step itself yielding the output. The fold may have more efficient ways to
-- retrieve the output rather than stuffing it in the state and using extract
-- on the state.
--
-- However, removing extract altogether may lead to less optimal code in some
-- cases because the driver of the fold needs to thread around the intermediate
-- output to return it if the stream stops before the fold could @Done@.  When
-- using this approach, the @parseMany (FL.take filesize)@ benchmark shows a
-- 2x worse performance even after ensuring everything fuses.  So we keep the
-- "extract" approach to ensure better perf in all cases.
--
-- But we could still yield both state and the output in @Partial@, the output
-- can be used for the scan use case, instead of using extract. Extract would
-- then be used only for the case when the stream stops before the fold
-- completes.

module Streamly.Internal.Data.Fold.Types
    (
    -- * Types
      Step (..)
    , Fold (..)

    -- * Fold constructors
    , mkFoldl
    , mkFoldlM
    , mkFoldr
    , mkFoldrM
    , mkAccum
    , mkAccum_
    , mkAccumM
    , mkAccumM_
    , mkFold
    , mkFold_
    , mkFoldM
    , mkFoldM_

    -- * Fold2
    , Fold2 (..)
    , simplify

    -- * Basic Folds
    , drain
    , toList

    -- * Generators
    , yield
    , yieldM

    -- * Transformations
    , map
    , lmap
    , lmapM
    , filter
    , filterM
    , catMaybes
    , take
    , takeInterval

    -- * Distributing
    , teeWith
    , teeWithFst
    , teeWithMin
    , shortest
    , longest

    -- * Serial Application
    , serialWith
    , serial_

    -- * Nested Application
    , concatMap
    , many
    , intervalsOf
    , chunksOf
    , chunksOf2

    , duplicate
    , initialize
    , runStep

    -- * Misc
    , GenericRunner(..) -- Is used in multiple step functions
    )
where

import Control.Monad (void, (>=>))
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newMVar, swapMVar, readMVar)
import Control.Exception (SomeException(..), catch, mask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (control)
import Data.Bifunctor (Bifunctor(..))
import Data.Maybe (isJust, fromJust)
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))
import Streamly.Internal.Data.SVar (MonadAsync)

import Prelude hiding (concatMap, filter, map, take)

-- $setup
-- >>> :m
-- >>> import Prelude hiding (concatMap, filter, map)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold

------------------------------------------------------------------------------
-- Monadic left folds
------------------------------------------------------------------------------

-- | Represents the result of the @step@ of a 'Fold'.  'Partial' returns an
-- intermediate state of the fold, the fold step can be called again with the
-- state or the driver can use @extract@ on the state to get the result out.
-- 'Done' returns the final result and the fold cannot be driven further.
--
-- /Pre-release/
--
{-# ANN type Step Fuse #-}
data Step s b
    = Partial !s
    | Done !b

-- | A bifunctor instance on 'Step'. @first@ maps on the value held by 'Partial'
-- and @second@ maps on the result held by 'Done'.
--
-- /Pre-release/
--
instance Bifunctor Step where
    {-# INLINE bimap #-}
    bimap f _ (Partial a) = Partial (f a)
    bimap _ g (Done b) = Done (g b)

    {-# INLINE first #-}
    first f (Partial a) = Partial (f a)
    first _ (Done x) = Done x

    {-# INLINE second #-}
    second _ (Partial x) = Partial x
    second f (Done a) = Done (f a)

-- | Maps the function over the result held by 'Done'.
-- @
-- fmap = 'second'
-- @
--
-- /Pre-release/
--
instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap = second

-- The Step functor around b allows expressing early termination like a right
-- fold. Traditional list right folds use function composition and laziness to
-- terminate early whereas we use data constructors. It allows stream fusion in
-- contrast to the foldr/build fusion when composing with functions.

-- | The type @Fold m a b@ having constructor @Fold step initial extract@
-- represents a left fold over an input stream of values of type @a@ to a
-- single value of type @b@ in 'Monad' @m@. The constructor is not exposed via
-- exposed modules, smart constructors are provided to create folds.
--
-- The fold uses an intermediate state @s@ as accumulator, the type @s@ is
-- specific to the fold. The initial value of the fold state @s@ is returned by
-- @initial@. The @step@ function consumes an input and either returns the
-- final result @b@ if the fold is done or the next intermediate state (see
-- 'Step'). At any point the fold driver can extract the result from the
-- intermediate state using the @extract@ function.
--
-- NOTE: If you think you need the constructor of this type please consider
-- using the smart constructors in "Streamly.Internal.Data.Fold' instead.
--
-- @since 0.7.0

data Fold m a b =
  -- | @Fold @ @ step @ @ initial @ @ extract@
  forall s. Fold (s -> a -> m (Step s b)) (m (Step s b)) (s -> m b)

------------------------------------------------------------------------------
-- Left fold constructors
------------------------------------------------------------------------------

-- | Make a fold from a left fold style pure step function and initial value of
-- the accumulator.
--
-- /Pre-release/
--
{-# INLINE mkFoldl #-}
mkFoldl :: Monad m => (b -> a -> b) -> b -> Fold m a b
mkFoldl step initial =
    Fold
        (\s a -> return $ Partial $ step s a)
        (return (Partial initial))
        return

-- | Make a fold from a left fold style monadic step function and initial value
-- of the accumulator.
--
-- /Pre-release/
--
{-# INLINE mkFoldlM #-}
mkFoldlM :: Monad m => (b -> a -> m b) -> m b -> Fold m a b
mkFoldlM step initial =
    Fold (\s a -> Partial <$> step s a) (Partial <$> initial) return

-- | Make an accumulating (non-terminating) fold using a pure step function, a
-- pure initial state and a pure state extraction function.
--
-- If your 'Fold' returns only 'Partial' (i.e. never returns a 'Done') then you
-- can use @mkAccum*@ constructors.
--
-- > mkAccum step initial extract = fmap extract (mkFoldl step initial)
--
-- /Internal/
--
{-# INLINE mkAccum #-}
mkAccum :: Monad m => (s -> a -> s) -> s -> (s -> b) -> Fold m a b
mkAccum step initial extract =
    Fold
        (\s a -> return $ Partial $ step s a)
        (return (Partial initial))
        (return . extract)

-- | Similar to 'mkAccum' but the final state extracted is identical to the
-- intermediate state.
--
-- @
-- mkAccum_ step initial = mkAccum step initial id
-- @
--
-- /Internal/
--
{-# INLINE mkAccum_ #-}
mkAccum_ :: Monad m => (b -> a -> b) -> b -> Fold m a b
mkAccum_ = mkFoldl

-- | Make an accumulating (non-terminating) fold with an effectful step
-- function, an initial state, and a state extraction function.
--
-- > mkAccumM step initial extract = rmapM extract (mkFoldlM step initial)
--
-- /Internal/
--
{-# INLINE mkAccumM #-}
mkAccumM :: Functor m => (s -> a -> m s) -> m s -> (s -> m b) -> Fold m a b
mkAccumM step initial =
    Fold (\s a -> Partial <$> step s a) (Partial <$> initial)

-- | Similar to 'mkAccumM' but the final state extracted is identical to the
-- intermediate state.
--
-- @
-- mkAccumM_ step initial = mkAccumM step initial return
-- @
--
-- /Internal/
--
{-# INLINE mkAccumM_ #-}
mkAccumM_ :: Monad m => (b -> a -> m b) -> m b -> Fold m a b
mkAccumM_ = mkFoldlM

------------------------------------------------------------------------------
-- Right fold constructors
------------------------------------------------------------------------------

-- | Make a fold using a right fold style step function and a terminal value.
-- It performs a right fold via a left fold using function composition.
-- This can be useful for constructing structures. For reductions this may be
-- very inefficient compared to using a direct fold implementation using
-- 'mkFold'.
--
-- For example,
--
-- > toList = mkFoldr (:) []
--
-- /Pre-release/
{-# INLINE mkFoldr #-}
mkFoldr :: Monad m => (a -> b -> b) -> b -> Fold m a b
mkFoldr g z = mkAccum (\f x -> f . g x) id ($ z)

-- | Like 'mkFoldr' but with a monadic step function.
--
-- For example,
--
-- > toList = mkFoldrM (\a xs -> return $ a : xs) (return [])
--
-- /Pre-release/
{-# INLINE mkFoldrM #-}
mkFoldrM :: Monad m => (a -> b -> m b) -> m b -> Fold m a b
mkFoldrM g z = mkAccumM (\f x -> return $ g x >=> f) (return return) (z >>=)

------------------------------------------------------------------------------
-- General fold constructors
------------------------------------------------------------------------------

-- | Make a terminating fold using a pure step function, a pure initial state
-- and a pure state extraction function.
--
-- /Pre-release/
--
{-# INLINE mkFold #-}
mkFold :: Monad m => (s -> a -> Step s b) -> Step s b -> (s -> b) -> Fold m a b
mkFold step initial extract =
    Fold (\s a -> return $ step s a) (return initial) (return . extract)

-- | Similar to 'mkFold' but the final state extracted is identical to the
-- intermediate state.
--
-- @
-- mkFold_ step initial = mkFold step initial id
-- @
--
-- /Pre-release/
--
{-# INLINE mkFold_ #-}
mkFold_ :: Monad m => (b -> a -> Step b b) -> Step b b -> Fold m a b
mkFold_ step initial = mkFold step initial id

-- | Make a terminating fold with an effectful step function and initial state,
-- and a state extraction function.
--
-- > mkFoldM = Fold
--
--  We can just use 'Fold' but it is provided for completeness.
--
-- /Pre-release/
--
{-# INLINE mkFoldM #-}
mkFoldM :: (s -> a -> m (Step s b)) -> m (Step s b) -> (s -> m b) -> Fold m a b
mkFoldM = Fold

-- | Similar to 'mkFoldM' but the final state extracted is identical to the
-- intermediate state.
--
-- @
-- mkFoldM_ step initial = mkFoldM step initial return
-- @
--
-- /Pre-release/
--
{-# INLINE mkFoldM_ #-}
mkFoldM_ :: Monad m => (b -> a -> m (Step b b)) -> m (Step b b) -> Fold m a b
mkFoldM_ step initial = mkFoldM step initial return

------------------------------------------------------------------------------
-- Fold2
------------------------------------------------------------------------------

-- | Experimental type to provide a side input to the fold for generating the
-- initial state. For example, if we have to fold chunks of a stream and write
-- each chunk to a different file, then we can generate the file name using a
-- monadic action. This is a generalized version of 'Fold'.
--
-- /Internal/
data Fold2 m c a b =
  -- | @Fold @ @ step @ @ inject @ @ extract@
  forall s. Fold2 (s -> a -> m s) (c -> m s) (s -> m b)

-- | Convert more general type 'Fold2' into a simpler type 'Fold'
--
-- /Internal/
simplify :: Functor m => Fold2 m c a b -> c -> Fold m a b
simplify (Fold2 step inject extract) c =
    Fold (\x a -> Partial <$> step x a) (Partial <$> inject c) extract

------------------------------------------------------------------------------
-- Basic Folds
------------------------------------------------------------------------------

-- | A fold that drains all its input, running the effects and discarding the
-- results.
--
-- > drain = drainBy (const (return ()))
--
-- @since 0.7.0
{-# INLINABLE drain #-}
drain :: Monad m => Fold m a ()
drain = mkAccum_ (\_ _ -> ()) ()

-- | Folds the input stream to a list.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Data.Array.Foreign"
-- instead.
--
-- @since 0.7.0
{-# INLINABLE toList #-}
toList :: Monad m => Fold m a [a]
toList = mkFoldr (:) []

------------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------------

-- | Maps a function on the output of the fold (the type @b@).
instance Functor m => Functor (Fold m a) where
    {-# INLINE fmap #-}
    fmap f (Fold step1 initial1 extract) = Fold step initial (fmap2 f extract)

        where

        initial = fmap2 f initial1
        step s b = fmap2 f (step1 s b)
        fmap2 g = fmap (fmap g)

-- This is the dual of stream "value".
--
-- | A fold that always yields a pure value without consuming any input.
--
-- /Pre-release/
--
{-# INLINE yield #-}
yield :: Applicative m => b -> Fold m a b
yield b = Fold undefined (pure $ Done b) pure

-- This is the dual of stream "yieldM".
--
-- | A fold that always yields the result of an effectful action without
-- consuming any input.
--
-- /Pre-release/
--
{-# INLINE yieldM #-}
yieldM :: Applicative m => m b -> Fold m a b
yieldM b = Fold undefined (Done <$> b) pure

{-# ANN type Step Fuse #-}
data SeqFoldState sl f sr = SeqFoldL !sl | SeqFoldR !f !sr

-- | Sequential fold application. Apply two folds sequentially to an input
-- stream.  The input is provided to the first fold, when it is done the
-- remaining input is provided to the second fold. When the second fold is done
-- or if the input stream is over, the outputs of the two folds are combined
-- using the supplied function.
--
-- Note: This is a folding dual of appending streams using
-- 'Streamly.Prelude.serial', it splits the streams using two folds and zips
-- the results. This has the same caveats as ParseD's @serialWith@
--
-- /Pre-release/
--
{-# INLINE serialWith #-}
serialWith :: Monad m => (a -> b -> c) -> Fold m x a -> Fold m x b -> Fold m x c
serialWith func (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where

    initial = do
        resL <- initialL
        case resL of
            Partial sl -> return $ Partial $ SeqFoldL sl
            Done bl -> do
                resR <- initialR
                return $ bimap (SeqFoldR (func bl)) (func bl) resR

    step (SeqFoldL st) a = do
        r <- stepL st a
        case r of
            Partial s -> return $ Partial (SeqFoldL s)
            Done b -> do
                res <- initialR
                return $ bimap (SeqFoldR (func b)) (func b) res
    step (SeqFoldR f st) a = do
        r <- stepR st a
        return
            $ case r of
                  Partial s -> Partial (SeqFoldR f s)
                  Done b -> Done (f b)

    extract (SeqFoldR f sR) = fmap f (extractR sR)
    extract (SeqFoldL sL) = do
        rL <- extractL sL
        res <- initialR
        case res of
            Partial sR -> do
                rR <- extractR sR
                return $ func rL rR
            Done rR -> return $ func rL rR

-- | Same as applicative '*>'. Run two folds serially one after the other
-- discarding the result of the first.
--
-- /Unimplemented/
--
{-# INLINE serial_ #-}
serial_ :: -- Monad m =>
    Fold m x a -> Fold m x b -> Fold m x b
serial_ _f1 _f2 = undefined

{-# ANN type GenericRunner Fuse #-}
data GenericRunner sL sR bL bR
    = RunBoth !sL !sR
    | RunLeft !sL !bR
    | RunRight !bL !sR

-- | @teeWith k f1 f2@ distributes its input to both @f1@ and @f2@ until both
-- of them terminate and combines their output using @k@.
--
-- @since 0.8.0
--
{-# INLINE teeWith #-}
teeWith :: Monad m => (a -> b -> c) -> Fold m x a -> Fold m x b -> Fold m x c
teeWith f (Fold stepL beginL doneL) (Fold stepR beginR doneR) =
    Fold step begin done

    where

    begin = do
        resL <- beginL
        resR <- beginR
        return
            $ case resL of
                  Partial sl ->
                      Partial
                          $ case resR of
                                Partial sr -> RunBoth sl sr
                                Done br -> RunLeft sl br
                  Done bl -> bimap (RunRight bl) (f bl) resR

    step (RunBoth sL sR) a = do
        resL <- stepL sL a
        resR <- stepR sR a
        case resL of
            Partial sL1 ->
                return
                    $ Partial
                    $ case resR of
                          Partial sR1 -> RunBoth sL1 sR1
                          Done bR -> RunLeft sL1 bR
            Done bL ->
                return
                    $ case resR of
                          Partial sR1 -> Partial $ RunRight bL sR1
                          Done bR -> Done $ f bL bR
    step (RunLeft sL bR) a = do
        resL <- stepL sL a
        return
            $ case resL of
                  Partial sL1 -> Partial $ RunLeft sL1 bR
                  Done bL -> Done $ f bL bR
    step (RunRight bL sR) a = do
        resR <- stepR sR a
        return
            $ case resR of
                  Partial sR1 -> Partial $ RunRight bL sR1
                  Done bR -> Done $ f bL bR

    done (RunBoth sL sR) = do
        bL <- doneL sL
        bR <- doneR sR
        return $ f bL bR
    done (RunLeft sL bR) = do
        bL <- doneL sL
        return $ f bL bR
    done (RunRight bL sR) = do
        bR <- doneR sR
        return $ f bL bR

-- | Like 'teeWith' but terminates when the first fold terminates.
--
-- /Unimplemented/
--
{-# INLINE teeWithFst #-}
teeWithFst :: (b -> c -> d) -> Fold m a b -> Fold m a c -> Fold m a d
teeWithFst = undefined

-- | Like 'teeWith' but terminates when any fold terminates.
--
-- /Unimplemented/
--
{-# INLINE teeWithMin #-}
teeWithMin :: (b -> c -> d) -> Fold m a b -> Fold m a c -> Fold m a d
teeWithMin = undefined

-- | Shortest alternative. Apply both folds in parallel but choose the result
-- from the one which consumed least input i.e. take the shortest succeeding
-- fold.
--
-- /Unimplemented/
--
{-# INLINE shortest #-}
shortest :: -- Monad m =>
    Fold m x a -> Fold m x a -> Fold m x a
shortest _f1 _f2 = undefined

-- | Longest alternative. Apply both folds in parallel but choose the result
-- from the one which consumed more input i.e. take the longest succeeding
-- fold.
--
-- /Unimplemented/
--
{-# INLINE longest #-}
longest :: -- Monad m =>
    Fold m x a -> Fold m x a -> Fold m x a
longest _f1 _f2 = undefined

data ConcatMapState m sa a c
    = B !sa
    | forall s. C (s -> a -> m (Step s c)) !s (s -> m c)

-- | Map a 'Fold' returning function on the result of a 'Fold' and run the
-- returned fold.
--
-- >>> import Data.Maybe (fromJust)
-- >>> Stream.fold (Fold.concatMap (flip Fold.take Fold.sum) (Fold.rmapM (return . fromJust) Fold.head)) $ Stream.fromList [10,9..1]
-- 45
--
-- /Pre-release/
--
{-# INLINE concatMap #-}
concatMap :: Monad m => (b -> Fold m a c) -> Fold m a b -> Fold m a c
concatMap f (Fold stepa initiala extracta) = Fold stepc initialc extractc
  where
    initialc = do
        r <- initiala
        case r of
            Partial s -> return $ Partial (B s)
            Done b -> initInnerFold (f b)

    stepc (B s) a = do
        r <- stepa s a
        case r of
            Partial s1 -> return $ Partial (B s1)
            Done b -> initInnerFold (f b)

    stepc (C stepInner s extractInner) a = do
        r <- stepInner s a
        return $ case r of
            Partial sc -> Partial (C stepInner sc extractInner)
            Done c -> Done c

    extractc (B s) = do
        r <- extracta s
        initExtract (f r)
    extractc (C _ sInner extractInner) = extractInner sInner

    initInnerFold (Fold step i e) = do
        r <- i
        return $ case r of
            Partial s -> Partial (C step s e)
            Done c -> Done c

    initExtract (Fold _ i e) = do
        r <- i
        case r of
            Partial s -> e s
            Done c -> return c

------------------------------------------------------------------------------
-- Mapping on input
------------------------------------------------------------------------------

-- | @(lmap f fold)@ maps the function @f@ on the input of the fold.
--
-- >>> Stream.fold (Fold.lmap (\x -> x * x) Fold.sum) (Stream.enumerateFromTo 1 100)
-- 338350
--
-- /Pre-release/
{-# INLINABLE lmap #-}
lmap :: (a -> b) -> Fold m b r -> Fold m a r
lmap f (Fold step begin done) = Fold step' begin done
    where
    step' x a = step x (f a)

-- XXX should be removed
-- |
-- /Internal/
{-# INLINE map #-}
map :: (a -> b) -> Fold m b r -> Fold m a r
map = lmap

-- | @(lmapM f fold)@ maps the monadic function @f@ on the input of the fold.
--
-- /Pre-release/
{-# INLINABLE lmapM #-}
lmapM :: Monad m => (a -> m b) -> Fold m b r -> Fold m a r
lmapM f (Fold step begin done) = Fold step' begin done
    where
    step' x a = f a >>= step x

------------------------------------------------------------------------------
-- Filtering
------------------------------------------------------------------------------

-- | Include only those elements that pass a predicate.
--
-- >>> Stream.fold (Fold.filter (> 5) Fold.sum) $ Stream.fromList [1..10]
-- 40
--
-- >>> Stream.fold (Fold.filter (< 5) Fold.sum) $ Stream.fromList [1..10]
-- 10
--
-- @since 0.7.0
{-# INLINABLE filter #-}
filter :: Monad m => (a -> Bool) -> Fold m a r -> Fold m a r
filter f (Fold step begin done) = Fold step' begin done
    where
    step' x a = if f a then step x a else return $ Partial x

-- | Like 'filter' but with a monadic predicate.
--
-- @since 0.7.0
{-# INLINABLE filterM #-}
filterM :: Monad m => (a -> m Bool) -> Fold m a r -> Fold m a r
filterM f (Fold step begin done) = Fold step' begin done
    where
    step' x a = do
      use <- f a
      if use then step x a else return $ Partial x

-- | Modify a fold to receive a 'Maybe' input, the 'Just' values are unwrapped
-- and sent to the original fold, 'Nothing' values are discarded.
--
-- /Pre-release/
{-# INLINE catMaybes #-}
catMaybes :: Monad m => Fold m a b -> Fold m (Maybe a) b
catMaybes = filter isJust . map fromJust

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- | Take at most @n@ input elements and fold them using the supplied fold.
--
-- >>> Stream.fold (Fold.take 1 Fold.toList) $ Stream.fromList [1]
-- [1]
--
-- >>> Stream.fold (Fold.take (-1) Fold.toList) $ Stream.fromList [1]
-- []
--
-- /Pre-release/
{-# INLINE take #-}
take :: Monad m => Int -> Fold m a b -> Fold m a b
take n (Fold fstep finitial fextract) = Fold step initial extract

    where

    initial = do
        res <- finitial
        case res of
            Partial s ->
                if n > 0
                then return $ Partial $ Tuple' 0 s
                else Done <$> fextract s
            Done b -> return $ Done b

    step (Tuple' i r) a = do
        res <- fstep r a
        case res of
            Partial sres -> do
                let i1 = i + 1
                    s1 = Tuple' i1 sres
                if i1 < n
                then return $ Partial s1
                else Done <$> fextract sres
            Done bres -> return $ Done bres

    extract (Tuple' _ r) = fextract r

------------------------------------------------------------------------------
-- Nesting
------------------------------------------------------------------------------

-- | Modify the fold such that it returns a new 'Fold' instead of the output.
-- If the fold was already done the returned fold would always yield the
-- result. If the fold was partial, the returned fold starts from where we left
-- i.e. it uses the last accumulator value as the initial value of the
-- accumulator. Thus we can resume the fold later and feed it more input.
--
-- >> do
-- >    more <- S.fold (FL.duplicate FL.sum) (S.enumerateFromTo 1 10)
-- >    evenMore <- S.fold (FL.duplicate more) (S.enumerateFromTo 11 20)
-- >    S.fold evenMore (S.enumerateFromTo 21 30)
-- > 465
--
-- /Pre-release/
{-# INLINABLE duplicate #-}
duplicate :: Monad m => Fold m a b -> Fold m a (Fold m a b)
duplicate (Fold step1 initial1 extract1) =
    Fold step initial (\s -> pure $ Fold step1 (pure $ Partial s) extract1)

    where

    initial = second yield <$> initial1

    step s a = second yield <$> step1 s a

-- | Run the initialization effect of a fold. The returned fold would use the
-- value returned by this effect as its initial value.
--
-- /Pre-release/
{-# INLINABLE initialize #-}
initialize :: Monad m => Fold m a b -> m (Fold m a b)
initialize (Fold step initial extract) = do
    i <- initial
    return $ Fold step (return i) extract

-- | Run one step of a fold and store the accumulator as an initial value in
-- the returned fold.
--
-- /Pre-release/
{-# INLINABLE runStep #-}
runStep :: Monad m => Fold m a b -> a -> m (Fold m a b)
runStep (Fold step initial extract) a = return $ Fold step initial1 extract

    where

    initial1 = do
        res <- initial
        case res of
              Partial fs -> step fs a
              b@(Done _) -> return b

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- All the grouping transformation that we apply to a stream can also be
-- applied to a fold input stream. groupBy et al can be written as terminating
-- folds and then we can apply "many" to use those repeatedly on a stream.

-- | Collect zero or more applications of a fold.  @many split collect@ applies
-- the @split@ fold repeatedly on the input stream and accumulates zero or more
-- fold results using @collect@.
--
-- Stops when @collect@ stops.
--
-- /Pre-release/
--
-- /See also: Streamly.Prelude.concatMap, Streamly.Prelude.foldMany/
--
{-# INLINE many #-}
many :: Monad m => Fold m a b -> Fold m b c -> Fold m a c
many (Fold sstep sinitial sextract) (Fold cstep cinitial cextract) =
    Fold step initial extract

    where

    -- cs = collect state
    -- ss = split state
    -- cres = collect state result
    -- sres = split state result
    -- cb = collect done
    -- sb = split done

    -- Caution! There is mutual recursion here, inlining the right functions is
    -- important.

    {-# INLINE handleSplitStep #-}
    handleSplitStep cs sres =
        case sres of
            Partial ss1 -> return $ Partial $ Tuple' ss1 cs
            Done sb -> runCollector cs sb

    {-# INLINE handleCollectStep #-}
    handleCollectStep cres =
        case cres of
            Partial cs -> do
                sres <- sinitial
                handleSplitStep cs sres
            Done cb -> return $ Done cb

    -- Do not inline this
    runCollector cs sb = cstep cs sb >>= handleCollectStep

    initial = cinitial >>= handleCollectStep

    {-# INLINE step #-}
    step (Tuple' ss cs) a = do
        sres <- sstep ss a
        handleSplitStep cs sres

    extract (Tuple' ss cs) = do
        sb <- sextract ss
        cres <- cstep cs sb
        case cres of
            Partial s -> cextract s
            Done b -> return b

-- | @chunksOf n split collect@ repeatedly applies the @split@ fold to chunks
-- of @n@ items in the input stream and supplies the result to the @collect@
-- fold.
--
-- > chunksOf n split = many (take n split)
--
-- Stops when @collect@ stops.
--
-- /Pre-release/
--
{-# INLINE chunksOf #-}
chunksOf :: Monad m => Int -> Fold m a b -> Fold m b c -> Fold m a c
chunksOf n split = many (take n split)

-- |
--
-- /Internal/
{-# INLINE chunksOf2 #-}
chunksOf2 :: Monad m => Int -> Fold m a b -> Fold2 m x b c -> Fold2 m x a c
chunksOf2 n (Fold step1 initial1 extract1) (Fold2 step2 inject2 extract2) =
    Fold2 step' inject' extract'

    where

    loopUntilPartial s = do
        res <- initial1
        case res of
            Partial fs -> return $ Tuple3' 0 fs s
            Done _ -> loopUntilPartial s

    inject' x = inject2 x >>= loopUntilPartial

    step' (Tuple3' i r1 r2) a =
        if i < n
        then do
            res <- step1 r1 a
            case res of
                Partial s -> return $ Tuple3' (i + 1) s r2
                Done b -> step2 r2 b >>= loopUntilPartial
        else extract1 r1 >>= step2 r2 >>= loopUntilPartial

    extract' (Tuple3' _ r1 r2) = extract1 r1 >>= step2 r2 >>= extract2

-- XXX We can use asyncClock here. A parser can be used to return an input that
-- arrives after the timeout.
-- XXX If n is 0 return immediately in initial.
--
-- | @takeInterval n fold@ uses @fold@ to fold the input items arriving within
-- a window of first @n@ seconds.
--
-- Stops when @fold@ stops or when the timeout occurs. Note that the fold needs
-- an input after the timeout to stop. For example, if no input is pushed to
-- the fold until one hour after the timeout had occurred, then the fold will
-- be done only after consuming that input.
--
-- /Pre-release/
--
{-# INLINE takeInterval #-}
takeInterval :: MonadAsync m => Double -> Fold m a b -> Fold m a b
takeInterval n (Fold step initial done) = Fold step' initial' done'

    where

    initial' = do
        res <- initial
        case res of
            Partial s -> do
                mv <- liftIO $ newMVar False
                t <-
                    control $ \run ->
                        mask $ \restore -> do
                            tid <-
                                forkIO
                                  $ catch
                                        (restore $ void $ run (timerThread mv))
                                        (handleChildException mv)
                            run (return tid)
                return $ Partial $ Tuple3' s mv t
            Done b -> return $ Done b

    step' (Tuple3' s mv t) a = do
        val <- liftIO $ readMVar mv
        if val
        then do
            res <- step s a
            case res of
                Partial sres -> Done <$> done sres
                Done bres -> return $ Done bres
        else do
            res <- step s a
            case res of
                Partial fs -> return $ Partial $ Tuple3' fs mv t
                Done b -> liftIO (killThread t) >> return (Done b)

    done' (Tuple3' s _ _) = done s

    timerThread mv = do
        liftIO $ threadDelay (round $ n * 1000000)
        -- Use IORef + CAS? instead of MVar since its a Bool?
        liftIO $ void $ swapMVar mv True

    handleChildException :: MVar Bool -> SomeException -> IO ()
    handleChildException mv _ = void $ swapMVar mv True

-- | Group the input stream into windows of n second each and then fold each
-- group using the provided fold function.
--
-- For example, we can copy and distribute a stream to multiple folds where
-- each fold can group the input differently e.g. by one second, one minute and
-- one hour windows respectively and fold each resulting stream of folds.
--
-- @
--
-- -----Fold m a b----|-Fold n a c-|-Fold n a c-|-...-|----Fold m a c
--
-- @
--
-- > intervalsOf n split = many (takeInterval n split)
--
-- /Pre-release/
--
{-# INLINE intervalsOf #-}
intervalsOf :: MonadAsync m => Double -> Fold m a b -> Fold m b c -> Fold m a c
intervalsOf n split = many (takeInterval n split)
