#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Parser.ParserK.Types
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- CPS style implementation of parsers.
--
-- The CPS representation allows linear performance for Applicative, sequenceA,
-- Monad, sequence, and Alternative, choice operations compared to the
-- quadratic complexity of the corresponding direct style operations. However,
-- direct style operations allow fusion with ~10x better performance than CPS.
--
-- The direct style representation does not allow for recursive definitions of
-- "some" and "many" whereas CPS allows that.

module Streamly.Internal.Data.Parser.ParserK.Types
    (
      Parser (..)
    , yield
    , yieldM
    , die

    -- * Conversion
    , toParserK
    , fromParserK
    )
where

import Control.Applicative (Alternative(..), liftA2)
import Control.Exception (assert, Exception(..))
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.Catch (MonadCatch, MonadThrow(..), try)
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
#if !(MIN_VERSION_base(4,10,0))
import Data.Semigroup ((<>))
#endif
import Streamly.Internal.Control.Exception

import qualified Streamly.Internal.Data.Parser.ParserD.Types as D

-- | The parse driver result. The driver may stop with a final result, pause
-- with a continuation to resume, or fail with an error.
--
-- /Pre-release/
--
data Driver m a r =
      Stop !Int r
      -- XXX we can use a "resume" and a "stop" continuations instead of Maybe.
      -- measure if that works any better.
    | Partial !Int (Maybe a -> m (Driver m a r))
    | Continue !Int (Maybe a -> m (Driver m a r))
    | Failed String

instance Functor m => Functor (Driver m a) where
    fmap f (Stop n r) = Stop n (f r)
    fmap f (Partial n yld) = Partial n (fmap (fmap f) . yld)
    fmap f (Continue n yld) = Continue n (fmap (fmap f) . yld)
    fmap _ (Failed e) = Failed e

-- The parser's result.
--
-- /Pre-release/
--
data Parse b =
      Done !Int !b      -- Done, no more input needed
    | Error !String     -- Failed

instance Functor Parse where
    fmap f (Done n b) = Done n (f b)
    fmap _ (Error e) = Error e

-- | A continuation passing style parser representation.
newtype Parser m a b = MkParser
    { runParser :: forall r.
           -- The number of elements that were not used by the previous
           -- consumer and should be carried forward.
           Int
           -- (nesting level, used elem count). Nesting level is increased
           -- whenever we enter an Alternative composition and decreased when
           -- it is done. The used element count is a count of elements
           -- consumed by the Alternative. If the Alternative fails we need to
           -- backtrack by this amount.
        -> (Int, Int)
           -- The first argument is the (nest level, used count) tuple as
           -- described above. The leftover element count carried as part of
           -- 'Done' constructor of 'Parse'.
        -> ((Int, Int) -> Parse b -> m (Driver m a r))
        -> m (Driver m a r)
    }

-------------------------------------------------------------------------------
-- Convert direct style 'D.Parser' to CPS style 'Parser'
-------------------------------------------------------------------------------

-- XXX Unlike the direct style folds/parsers, the initial action in CPS parsers
-- is not performed when the fold is initialized. It is performed when the
-- first element is processed by the fold or if no elements are processed then
-- at the extraction. We should either make the direct folds like this or make
-- the CPS folds behavior also like the direct ones.
--
-- | Convert a direct style parser ('D.Parser') to a CPS style parser
-- ('Parser').
--
{-# INLINE_NORMAL parseDToK #-}
parseDToK
    :: MonadCatch m
    => (s -> a -> m (D.Step s b))
    -> m (D.Initial s b)
    -> (s -> m b)
    -> Int
    -> (Int, Int)
    -> ((Int, Int) -> Parse b -> m (Driver m a r))
    -> m (Driver m a r)

parseDToK pstep initial extract leftover (0, _) cont = do
    res <- initial
    case res of
        D.IPartial r -> return $ Continue leftover (parseCont (return r))
        D.IDone b -> cont (0,0) (Done 0 b)
        D.IError err -> cont (0,0) (Error err)

    where

    parseCont pst (Just x) = do
        r <- pst
        pRes <- pstep r x
        case pRes of
            D.Done n b -> cont (0,0) (Done n b)
            D.Error err -> cont (0,0) (Error err)
            D.Partial n pst1 -> return $ Partial n (parseCont (return pst1))
            D.Continue n pst1 -> return $ Continue n (parseCont (return pst1))

    parseCont acc Nothing = do
        pst <- acc
        r <- try $ extract pst
        case r of
            Left (e :: D.ParseError) -> cont (0,0) (Error (displayException e))
            Right b -> cont (0,0) (Done 0 b)

parseDToK pstep initial extract leftover (level, count) cont = do
    res <- initial
    case res of
        D.IPartial r -> return $ Continue leftover (parseCont count (return r))
        D.IDone b -> cont (level,count) (Done 0 b)
        D.IError err -> cont (level,count) (Error err)

    where

    parseCont !cnt pst (Just x) = do
        let !cnt1 = cnt + 1
        r <- pst
        pRes <- pstep r x
        case pRes of
            D.Done n b -> do
                assert (n <= cnt1) (return ())
                cont (level, cnt1 - n) (Done n b)
            D.Error err ->
                cont (level, cnt1) (Error err)
            D.Partial n pst1 -> do
                assert (n <= cnt1) (return ())
                return $ Partial n (parseCont (cnt1 - n) (return pst1))
            D.Continue n pst1 -> do
                assert (n <= cnt1) (return ())
                return $ Continue n (parseCont (cnt1 - n) (return pst1))
    parseCont cnt acc Nothing = do
        pst <- acc
        r <- try $ extract pst
        let s = (level, cnt)
        case r of
            Left (e :: D.ParseError) -> cont s (Error (displayException e))
            Right b -> cont s (Done 0 b)

-- | Convert a direct style 'D.Parser' to a CPS style 'Parser'.
--
-- /Pre-release/
--
{-# INLINE_LATE toParserK #-}
toParserK :: MonadCatch m => D.Parser m a b -> Parser m a b
toParserK (D.Parser step initial extract) =
    MkParser $ parseDToK step initial extract

-------------------------------------------------------------------------------
-- Convert CPS style 'Parser' to direct style 'D.Parser'
-------------------------------------------------------------------------------

-- | A continuation to extract the result when a CPS parser is done.
{-# INLINE parserDone #-}
parserDone :: Monad m => (Int, Int) -> Parse b -> m (Driver m a b)
parserDone (0,_) (Done n b) = return $ Stop n b
parserDone st (Done _ _) =
    error $ "Bug: fromParserK: inside alternative: " ++ show st
parserDone _ (Error e) = return $ Failed e

-- | When there is no more input to feed, extract the result from the Parser.
--
-- /Pre-release/
--
extractParse :: MonadThrow m => (Maybe a -> m (Driver m a b)) -> m b
extractParse cont = do
    r <- cont Nothing
    case r of
        Stop _ b -> return b
        Partial _ _ -> error "Bug: extractParse got Partial"
        Continue _ cont1 -> extractParse cont1
        Failed e -> throwM $ D.ParseError e

data FromParserK b c = FPKDone !Int !b | FPKCont c

-- | Convert a CPS style 'Parser' to a direct style 'D.Parser'.
--
-- "initial" returns a continuation which can be called one input at a time
-- using the "step" function.
--
-- /Pre-release/
--
{-# INLINE_LATE fromParserK #-}
fromParserK :: MonadThrow m => Parser m a b -> D.Parser m a b
fromParserK parser = D.Parser step initial extract

    where

    initial = do
        r <- runParser parser 0 (0,0) parserDone
        return $ case r of
            Stop n b -> D.IPartial $ FPKDone n b
            Failed e -> D.IError e
            Partial _ cont -> D.IPartial $ FPKCont cont -- XXX can we get this?
            Continue _ cont -> D.IPartial $ FPKCont cont

    -- Note, we can only reach FPKDone and FPKError from "initial". FPKCont
    -- always transitions to only FPKCont.  The input remains unconsumed in
    -- this case so we use "n + 1".
    step (FPKDone n b) _ = do
        assertM (n == 0)
        return $ D.Done (n + 1) b
    step (FPKCont cont) a = do
        r <- cont (Just a)
        return $ case r of
            Stop n b -> D.Done n b
            Failed e -> D.Error e
            Partial n cont1 -> D.Partial n (FPKCont cont1)
            Continue n cont1 -> D.Continue n (FPKCont cont1)

    -- Note, we can only reach FPKDone and FPKError from "initial".
    extract (FPKDone _ b) = return b
    extract (FPKCont cont) = extractParse cont

#ifndef DISABLE_FUSION
{-# RULES "fromParserK/toParserK fusion" [2]
    forall s. toParserK (fromParserK s) = s #-}
{-# RULES "toParserK/fromParserK fusion" [2]
    forall s. fromParserK (toParserK s) = s #-}
#endif

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

-- | Maps a function over the output of the parser.
--
instance Functor m => Functor (Parser m a) where
    {-# INLINE fmap #-}
    fmap f parser = MkParser $ \lo st yieldk ->
        let yld s res = yieldk s (fmap f res)
         in runParser parser lo st yld

-------------------------------------------------------------------------------
-- Sequential applicative
-------------------------------------------------------------------------------

-- This is the dual of stream "value".
--
-- | A parser that always yields a pure value without consuming any input.
--
-- /Pre-release/
--
{-# INLINE yield #-}
yield :: b -> Parser m a b
yield b = MkParser $ \lo st yieldk -> yieldk st (Done lo b)

-- | See 'Streamly.Internal.Data.Parser.yieldM'.
--
-- /Pre-release/
--
{-# INLINE yieldM #-}
yieldM :: Monad m => m b -> Parser m a b
yieldM eff = MkParser $ \lo st yieldk -> eff >>= \b -> yieldk st (Done lo b)

-- | 'Applicative' form of 'Streamly.Internal.Data.Parser.serialWith'. Note that
-- this operation does not fuse, use 'Streamly.Internal.Data.Parser.serialWith'
-- when fusion is important.
--
instance Monad m => Applicative (Parser m a) where
    {-# INLINE pure #-}
    pure = yield

    {-# INLINE (<*>) #-}
    (<*>) = ap

    {-# INLINE (*>) #-}
    m1 *> m2 = MkParser $ \lo st yieldk ->
        let yield1 s (Done n _) = runParser m2 n s yieldk
            yield1 s (Error e) = yieldk s (Error e)
        in runParser m1 lo st yield1

    {-# INLINE (<*) #-}
    m1 <* m2 = MkParser $ \lo st yieldk ->
        let yield1 s (Done n b) =
                let yield2 s1 (Done n1 _) = yieldk s1 (Done n1 b)
                    yield2 s1 (Error e) = yieldk s1 (Error e)
                in runParser m2 n s yield2
            yield1 s (Error e) = yieldk s (Error e)
        in runParser m1 lo st yield1

#if MIN_VERSION_base(4,10,0)
    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)
#endif

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

-- This is the dual of "nil".
--
-- | A parser that always fails with an error message without consuming
-- any input.
--
-- /Pre-release/
--
{-# INLINE die #-}
die :: String -> Parser m a b
die err = MkParser (\_ st yieldk -> yieldk st (Error err))

-- | Monad composition can be used for lookbehind parsers, we can make the
-- future parses depend on the previously parsed values.
--
-- If we have to parse "a9" or "9a" but not "99" or "aa" we can use the
-- following parser:
--
-- @
-- backtracking :: MonadCatch m => PR.Parser m Char String
-- backtracking =
--     sequence [PR.satisfy isDigit, PR.satisfy isAlpha]
--     '<|>'
--     sequence [PR.satisfy isAlpha, PR.satisfy isDigit]
-- @
--
-- We know that if the first parse resulted in a digit at the first place then
-- the second parse is going to fail.  However, we waste that information and
-- parse the first character again in the second parse only to know that it is
-- not an alphabetic char.  By using lookbehind in a 'Monad' composition we can
-- avoid redundant work:
--
-- @
-- data DigitOrAlpha = Digit Char | Alpha Char
--
-- lookbehind :: MonadCatch m => PR.Parser m Char String
-- lookbehind = do
--     x1 \<-    Digit '<$>' PR.satisfy isDigit
--          '<|>' Alpha '<$>' PR.satisfy isAlpha
--
--     -- Note: the parse depends on what we parsed already
--     x2 <- case x1 of
--         Digit _ -> PR.satisfy isAlpha
--         Alpha _ -> PR.satisfy isDigit
--
--     return $ case x1 of
--         Digit x -> [x,x2]
--         Alpha x -> [x,x2]
-- @
--
-- See also 'Streamly.Internal.Data.Parser.concatMap'. This monad instance
-- does not fuse, use 'Streamly.Internal.Data.Parser.concatMap' when you need
-- fusion.
--
instance Monad m => Monad (Parser m a) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    m >>= k = MkParser $ \lo st yieldk ->
        let yield1 s (Done n b) = runParser (k b) n s yieldk
            yield1 s (Error e) = yieldk s (Error e)
         in runParser m lo st yield1

    {-# INLINE (>>) #-}
    (>>) = (*>)

#if !(MIN_VERSION_base(4,13,0))
    -- This is redefined instead of just being Fail.fail to be
    -- compatible with base 4.8.
    {-# INLINE fail #-}
    fail = die
#endif

#if MIN_VERSION_base(4,9,0)
instance Monad m => Fail.MonadFail (Parser m a) where
    {-# INLINE fail #-}
    fail = die
#endif

-------------------------------------------------------------------------------
-- Alternative
-------------------------------------------------------------------------------

-- | 'Alternative' form of 'Streamly.Internal.Data.Parser.alt'. Backtrack and
-- run the second parser if the first one fails.
--
-- The "some" and "many" operations of alternative accumulate results in a pure
-- list which is not scalable and streaming. Instead use
-- 'Streamly.Internal.Data.Parser.some' and
-- 'Streamly.Internal.Data.Parser.many' for fusible operations with composable
-- accumulation of results.
--
-- See also 'Streamly.Internal.Data.Parser.alt'. This 'Alternative' instance
-- does not fuse, use 'Streamly.Internal.Data.Parser.alt' when you need
-- fusion.
--
instance Monad m => Alternative (Parser m a) where
    {-# INLINE empty #-}
    empty = die "empty"

    {-# INLINE (<|>) #-}
    m1 <|> m2 = MkParser $ \lo (level, _) yieldk ->
        let yield1 (0, _) _ = error "0 nest level in Alternative"
            yield1 (lvl, _) (Done n b) = yieldk (lvl - 1, 0) (Done n b)
            yield1 (lvl, cnt) (Error _) = runParser m2 cnt (lvl - 1, 0) yieldk
        in runParser m1 lo (level + 1, 0) yield1

    -- some and many are implemented here instead of using default definitions
    -- so that we can use INLINE on them. It gives 50% performance improvement.

    {-# INLINE many #-}
    many v = many_v

        where

        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v

    {-# INLINE some #-}
    some v = some_v

        where

        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v

-- | 'mzero' is same as 'empty', it aborts the parser. 'mplus' is same as
-- '<|>', it selects the first succeeding parser.
--
-- /Pre-release/
--
instance Monad m => MonadPlus (Parser m a) where
    {-# INLINE mzero #-}
    mzero = die "mzero"

    {-# INLINE mplus #-}
    mplus = (<|>)
