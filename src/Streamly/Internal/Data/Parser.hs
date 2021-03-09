{-# OPTIONS_GHC -Wno-orphans  #-}

-- |
-- Module      : Streamly.Internal.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast backtracking parsers with stream fusion and native streaming
-- capability.
--
-- 'Applicative' and 'Control.Applicative.Alternative' type class based
-- combinators from the
-- <http://hackage.haskell.org/package/parser-combinators parser-combinators>
-- package can also be used with the 'Parser' type. However, there are two
-- important differences between @parser-combinators@ and the equivalent ones
-- provided in this module in terms of performance:
--
-- 1) @parser-combinators@ use plain Haskell lists to collect the results, in a
-- strict Monad like IO, the results are necessarily buffered before they can
-- be consumed.  This may not perform optimally in streaming applications
-- processing large amounts of data.  Equivalent combinators in this module can
-- consume the results of parsing using a 'Fold', thus providing a scalability
-- and a composable consumer.
--
-- 2) Several combinators in this module can be many times faster because of
-- stream fusion. For example, 'Streamly.Internal.Data.Parser.many' combinator
-- in this module is much faster than the 'Control.Applicative.many' combinator
-- of 'Control.Applicative.Alternative' type class.
--
-- = Errors
--
-- Failing parsers in this module throw the 'D.ParseError' exception.
--
-- = Naming
--
-- As far as possible, try that the names of the combinators in this module are
-- consistent with:
--
-- * <https://hackage.haskell.org/package/base/docs/Text-ParserCombinators-ReadP.html base/Text.ParserCombinators.ReadP>
-- * <http://hackage.haskell.org/package/parser-combinators parser-combinators>
-- * <http://hackage.haskell.org/package/megaparsec megaparsec>
-- * <http://hackage.haskell.org/package/attoparsec attoparsec>
-- * <http://hackage.haskell.org/package/parsec parsec>

module Streamly.Internal.Data.Parser
    (
      K.Parser (..)
    , D.ParseError (..)
    , D.Step (..)

    -- First order parsers
    -- * Accumulators
    , fromFold
    , yield
    , yieldM
    , die
    , dieM

    -- * Element parsers
    , peek
    , eof
    , satisfy
    , maybe
    , either

    -- * Sequence parsers
    --
    -- | Parsers chained in series, if one parser terminates the composition
    -- terminates.

    -- | Grab a sequence of input elements without inspecting them
    , takeBetween
    -- , take   -- takeBetween 0 n
    , takeEQ -- takeBetween n n
    , takeGE -- takeBetween n maxBound

    -- Grab a sequence of input elements by inspecting them
    , lookAhead
    , takeWhileP
    , takeWhile
    -- $takeWhile
    , takeWhile1
    , drainWhile

    , sliceSepByP
    , sliceBeginWith
    , sliceSepWith
    , escapedSliceSepBy
    , escapedFrameBy
    , wordBy
    , groupBy
    , groupByRolling
    , eqBy
    -- | Unimplemented
    --
    -- @
    -- , prefixOf -- match any prefix of a given string
    -- , suffixOf -- match any suffix of a given string
    -- , infixOf -- match any substring of a given string
    -- @

    -- Second order parsers (parsers using parsers)
    -- * Binary Combinators

    -- ** Sequential Applicative
    , serialWith
    , split_

    -- ** Parallel Applicatives
    , teeWith
    , teeWithFst
    , teeWithMin
    -- , teeTill -- like manyTill but parallel

    -- ** Sequential Interleaving
    -- Use two folds, run a primary parser, its rejected values go to the
    -- secondary parser.
    , deintercalate

    -- ** Sequential Alternative
    , alt

    -- ** Parallel Alternatives
    , shortest
    , longest
    -- , fastest

    -- * N-ary Combinators
    -- ** Sequential Collection
    , concatSequence
    , concatMap

    -- ** Sequential Repetition
    , count
    , countBetween

    , manyP
    , many
    , some
    , manyTillP
    , manyTill
    , manyThen

    -- ** Special cases
    -- | TODO: traditional implmentations of these may be of limited use. For
    -- example, consider parsing lines separated by @\\r\\n@. The main parser
    -- will have to detect and exclude the sequence @\\r\\n@ anyway so that we
    -- can apply the "sep" parser.
    --
    -- We can instead implement these as special cases of deintercalate.
    --
    -- @
    -- , endBy
    -- , sepBy
    -- , sepEndBy
    -- , beginBy
    -- , sepBeginBy
    -- , sepAroundBy
    -- @

    -- * Distribution
    --
    -- | A simple and stupid impl would be to just convert the stream to an
    -- array and give the array reference to all consumers. The array can be
    -- grown on demand by any consumer and truncated when nonbody needs it.

    -- ** Distribute to collection
    -- ** Distribute to repetition

    -- ** Interleaved collection
    -- |
    --
    -- 1. Round robin
    -- 2. Priority based
    , roundRobin

    -- ** Collection of Alternatives
    -- | Unimplemented
    --
    -- @
    -- , shortestN
    -- , longestN
    -- , fastestN -- first N successful in time
    -- , choiceN  -- first N successful in position
    -- @
    , choice   -- first successful in position

    -- ** Repeated Alternatives
    , retryMaxTotal
    , retryMaxSuccessive
    , retry
    )
where

import Control.Monad.Catch (MonadCatch)
import Prelude
       hiding (any, all, take, takeWhile, sequence, concatMap, maybe, either)

import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Parser.ParserK.Types (Parser)

import qualified Streamly.Internal.Data.Fold.Types as FL
import qualified Streamly.Internal.Data.Parser.ParserD as D
import qualified Streamly.Internal.Data.Parser.ParserK.Types as K

--
-- $setup
-- >>> :m
-- >>> import Prelude hiding (any, all, take, takeWhile, sequence, concatMap, maybe, either)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream (parse, parseMany)
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Parser as Parser

-------------------------------------------------------------------------------
-- Upgrade folds to parses
-------------------------------------------------------------------------------
--
-- | Make a 'Parser' from a 'Fold'.
--
-- /Pre-release/
--
{-# INLINE fromFold #-}
fromFold :: MonadCatch m => Fold m a b -> Parser m a b
fromFold = K.toParserK . D.fromFold

-------------------------------------------------------------------------------
-- Terminating but not failing folds
-------------------------------------------------------------------------------
--
-- This is the dual of stream "value".
--
-- | A parser that always yields a pure value without consuming any input.
--
-- /Pre-release/
--
{-# INLINE [3] yield #-}
yield :: MonadCatch m => b -> Parser m a b
yield = K.toParserK . D.yield
{-# RULES "yield fallback to CPS" [2]
    forall a. K.toParserK (D.yield a) = K.yield a #-}

-- This is the dual of stream "yieldM".
--
-- | A parser that always yields the result of an effectful action without
-- consuming any input.
--
-- /Pre-release/
--
{-# INLINE yieldM #-}
yieldM :: MonadCatch m => m b -> Parser m a b
yieldM = K.yieldM -- K.toParserK . D.yieldM

-- This is the dual of "nil".
--
-- | A parser that always fails with an error message without consuming
-- any input.
--
-- /Pre-release/
--
{-# INLINE [3] die #-}
die :: MonadCatch m => String -> Parser m a b
die = K.toParserK . D.die
{-# RULES "die fallback to CPS" [2]
    forall a. K.toParserK (D.die a) = K.die a #-}

-- This is the dual of "nilM".
--
-- | A parser that always fails with an effectful error message and without
-- consuming any input.
--
-- /Pre-release/
--
{-# INLINE dieM #-}
dieM :: MonadCatch m => m String -> Parser m a b
dieM = K.toParserK . D.dieM

-------------------------------------------------------------------------------
-- Failing Parsers
-------------------------------------------------------------------------------

-- | Peek the head element of a stream, without consuming it. Fails if it
-- encounters end of input.
--
-- >>> Stream.parse ((,) <$> Parser.peek <*> Parser.satisfy (> 0)) $ Stream.fromList [1]
-- (1,1)
--
-- @
-- peek = lookAhead (satisfy True)
-- @
--
-- /Pre-release/
--
{-# INLINE peek #-}
peek :: MonadCatch m => Parser m a a
peek = K.toParserK D.peek

-- | Succeeds if we are at the end of input, fails otherwise.
--
-- >>> Stream.parse ((,) <$> Parser.satisfy (> 0) <*> Parser.eof) $ Stream.fromList [1]
-- (1,())
--
-- /Pre-release/
--
{-# INLINE eof #-}
eof :: MonadCatch m => Parser m a ()
eof = K.toParserK D.eof

-- | Returns the next element if it passes the predicate, fails otherwise.
--
-- >>> Stream.parse (Parser.satisfy (== 1)) $ Stream.fromList [1,0,1]
-- 1
--
-- /Pre-release/
--
{-# INLINE satisfy #-}
satisfy :: MonadCatch m => (a -> Bool) -> Parser m a a
satisfy = K.toParserK . D.satisfy

-- | Map a 'Maybe' returning function on the next element in the stream. The
-- parser fails if the function returns 'Nothing' otherwise returns the 'Just'
-- value.
--
-- /Pre-release/
--
{-# INLINE maybe #-}
maybe :: MonadCatch m => (a -> Maybe b) -> Parser m a b
maybe = K.toParserK . D.maybe

-- | Map an 'Either' returning function on the next element in the stream.  If
-- the function returns 'Left err', the parser fails with the error message
-- @err@ otherwise returns the 'Right' value.
--
-- /Pre-release/
--
{-# INLINE either #-}
either :: MonadCatch m => (a -> Either String b) -> Parser m a b
either = K.toParserK . D.either

-------------------------------------------------------------------------------
-- Taking elements
-------------------------------------------------------------------------------

-- | @takeBetween m n@ takes a minimum of @m@ and a maximum of @n@ input
-- elements and folds them using the supplied fold.
--
-- Stops after @n@ elements.
-- Fails if the stream ends before @m@ elements could be taken.
--
-- Examples: -
--
-- @
-- >>> :{
--   takeBetween' low high ls = Stream.parse prsr (Stream.fromList ls)
--     where prsr = Parser.takeBetween low high Fold.toList
-- :}
--
-- @
--
-- >>> takeBetween' 2 4 [1, 2, 3, 4, 5]
-- [1,2,3,4]
--
-- >>> takeBetween' 2 4 [1, 2]
-- [1,2]
--
-- >>> takeBetween' 2 4 [1]
-- *** Exception: ParseError "takeBetween: Expecting alteast 2 elements, got 1"
--
-- >>> takeBetween' 0 0 [1, 2]
-- []
--
-- >>> takeBetween' 0 1 []
-- []
--
-- @takeBetween@ is the most general take operation, other take operations can
-- be defined in terms of takeBetween. For example:
--
-- @
-- take = takeBetween 0 n  -- equivalent of take
-- take1 = takeBetween 1 n -- equivalent of takeLE1
-- takeEQ = takeBetween n n
-- takeGE = takeBetween n maxBound
-- @
--
-- /Pre-release/
--
{-# INLINE takeBetween #-}
takeBetween ::  MonadCatch m =>
    Int -> Int -> Fold m a b -> Parser m a b
takeBetween m n = K.toParserK . D.takeBetween m n

-- | Stops after taking exactly @n@ input elements.
--
-- * Stops - after consuming @n@ elements.
-- * Fails - if the stream or the collecting fold ends before it can collect
--           exactly @n@ elements.
--
-- >>> Stream.parse (Parser.takeEQ 4 Fold.toList) $ Stream.fromList [1,0,1]
-- *** Exception: ParseError "takeEQ: Expecting exactly 4 elements, input terminated on 3"
--
-- /Pre-release/
--
{-# INLINE takeEQ #-}
takeEQ :: MonadCatch m => Int -> Fold m a b -> Parser m a b
takeEQ n = K.toParserK . D.takeEQ n

-- | Take at least @n@ input elements, but can collect more.
--
-- * Stops - when the collecting fold stops.
-- * Fails - if the stream or the collecting fold ends before producing @n@
--           elements.
--
-- >>> Stream.parse (Parser.takeGE 4 Fold.toList) $ Stream.fromList [1,0,1]
-- *** Exception: ParseError "takeGE: Expecting at least 4 elements, input terminated on 3"
--
-- >>> Stream.parse (Parser.takeGE 4 Fold.toList) $ Stream.fromList [1,0,1,0,1]
-- [1,0,1,0,1]
--
-- /Pre-release/
--
{-# INLINE takeGE #-}
takeGE :: MonadCatch m => Int -> Fold m a b -> Parser m a b
takeGE n = K.toParserK . D.takeGE n

-- $takeWhile
-- Note: This is called @takeWhileP@ and @munch@ in some parser libraries.

-- | Like 'takeWhile' but uses a 'Parser' instead of a 'Fold' to collect the
-- input. The combinator stops when the condition fails or if the collecting
-- parser stops.
--
-- This is a generalized version of takeWhile, for example 'takeWhile1' can be
-- implemented in terms of this:
--
-- @
-- takeWhile1 cond p = takeWhile cond (takeBetween 1 maxBound p)
-- @
--
-- Stops: when the condition fails or the collecting parser stops.
-- Fails: when the collecting parser fails.
--
-- /Unimplemented/
--
{-# INLINE takeWhileP #-}
takeWhileP :: -- MonadCatch m =>
    (a -> Bool) -> Parser m a b -> Parser m a b
takeWhileP _cond = undefined -- K.toParserK . D.takeWhileP cond

-- | Collect stream elements until an element fails the predicate. The element
-- on which the predicate fails is returned back to the input stream.
--
-- * Stops - when the predicate fails or the collecting fold stops.
-- * Fails - never.
--
-- >>> Stream.parse (Parser.takeWhile (== 0) Fold.toList) $ Stream.fromList [0,0,1,0,1]
-- [0,0]
--
-- We can implement a @breakOn@ using 'takeWhile':
--
-- @
-- breakOn p = takeWhile (not p)
-- @
--
-- /Pre-release/
--
{-# INLINE takeWhile #-}
takeWhile :: MonadCatch m => (a -> Bool) -> Fold m a b -> Parser m a b
takeWhile cond = K.toParserK . D.takeWhile cond

-- | Like 'takeWhile' but takes at least one element otherwise fails.
--
-- /Pre-release/
--
{-# INLINE takeWhile1 #-}
takeWhile1 :: MonadCatch m => (a -> Bool) -> Fold m a b -> Parser m a b
takeWhile1 cond = K.toParserK . D.takeWhile1 cond

-- | Drain the input as long as the predicate succeeds, running the effects and
-- discarding the results.
--
-- This is also called @skipWhile@ in some parsing libraries.
--
-- /Pre-release/
--
{-# INLINE drainWhile #-}
drainWhile :: MonadCatch m => (a -> Bool) -> Parser m a ()
drainWhile p = takeWhile p FL.drain

-- | @sliceSepByP cond parser@ parses a slice of the input using @parser@ until
-- @cond@ succeeds or the parser stops.
--
-- This is a generalized slicing parser which can be used to implement other
-- parsers e.g.:
--
-- @
-- sliceSepByMax cond n p = sliceSepByP cond (take n p)
-- sliceSepByBetween cond m n p = sliceSepByP cond (takeBetween m n p)
-- @
--
-- /Pre-release/
--
{-# INLINABLE sliceSepByP #-}
sliceSepByP ::
    MonadCatch m =>
    (a -> Bool) -> Parser m a b -> Parser m a b
sliceSepByP cond = K.toParserK . D.sliceSepByP cond . K.fromParserK

-- | Like 'sliceSepBy' but does not drop the separator element, instead
-- separator is emitted as a separate element in the output.
--
-- /Unimplemented/
{-# INLINABLE sliceSepWith #-}
sliceSepWith :: -- MonadCatch m =>
    (a -> Bool) -> Fold m a b -> Parser m a b
sliceSepWith _cond = undefined -- K.toParserK . D.sliceSepBy cond

-- | Collect stream elements until an elements passes the predicate, return the
-- last element on which the predicate succeeded back to the input stream.  If
-- the predicate succeeds on the first element itself then the parser does not
-- terminate there. The succeeding element in the leading position
-- is treated as a prefix separator which is kept in the output segment.
--
-- * Stops - when the predicate succeeds in non-leading position.
-- * Fails - never.
--
-- S.splitWithPrefix pred f = S.parseMany (PR.sliceBeginWith pred f)
--
-- Examples: -
--
-- >>> :{
--  sliceBeginWithOdd ls = Stream.parse prsr (Stream.fromList ls)
--      where prsr = Parser.sliceBeginWith odd Fold.toList
-- :}
--
--
-- >>> sliceBeginWithOdd [2, 4, 6, 3]
-- *** Exception: sliceBeginWith : slice begins with an element which fails the predicate
-- ...
--
-- >>> sliceBeginWithOdd [3, 5, 7, 4]
-- [3]
--
-- >>> sliceBeginWithOdd [3, 4, 6, 8, 5]
-- [3,4,6,8]
--
-- >>> sliceBeginWithOdd []
-- []
--
-- /Pre-release/
--
{-# INLINABLE sliceBeginWith #-}
sliceBeginWith ::
    MonadCatch m =>
    (a -> Bool) -> Fold m a b -> Parser m a b
sliceBeginWith cond = K.toParserK . D.sliceBeginWith cond

-- | Like 'sliceSepBy' but the separator elements can be escaped using an
-- escape char determined by the second predicate.
--
-- /Unimplemented/
{-# INLINABLE escapedSliceSepBy #-}
escapedSliceSepBy :: -- MonadCatch m =>
    (a -> Bool) -> (a -> Bool) -> Fold m a b -> Parser m a b
escapedSliceSepBy _cond _esc = undefined
    -- K.toParserK . D.escapedSliceSepBy cond esc

-- | @escapedFrameBy begin end escape@ parses a string framed using @begin@ and
-- @end@ as the frame begin and end marker elements and @escape@ as an escaping
-- element to escape the occurrence of the framing elements within the frame.
-- Nested frames are allowed, but nesting is removed when parsing.
--
-- For example,
--
-- @
-- > Stream.parse (Parser.escapedFrameBy (== '{') (== '}') (== '\\') Fold.toList) $ Stream.fromList "{hello}"
-- "hello"
--
-- > Stream.parse (Parser.escapedFrameBy (== '{') (== '}') (== '\\') Fold.toList) $ Stream.fromList "{hello {world}}"
-- "hello world"
--
-- > Stream.parse (Parser.escapedFrameBy (== '{') (== '}') (== '\\') Fold.toList) $ Stream.fromList "{hello \\{world\\}}"
-- "hello {world}"
--
-- > Stream.parse (Parser.escapedFrameBy (== '{') (== '}') (== '\\') Fold.toList) $ Stream.fromList "{hello {world}"
-- ParseError "Unterminated '{'"
--
-- @
--
-- /Unimplemented/
{-# INLINABLE escapedFrameBy #-}
escapedFrameBy :: -- MonadCatch m =>
    (a -> Bool) -> (a -> Bool) -> (a -> Bool) -> Fold m a b -> Parser m a b
escapedFrameBy _begin _end _escape _p = undefined
    -- K.toParserK . D.frameBy begin end escape p

-- | Like 'splitOn' but strips leading, trailing, and repeated separators.
-- Therefore, @".a..b."@ having '.' as the separator would be parsed as
-- @["a","b"]@.  In other words, its like parsing words from whitespace
-- separated text.
--
-- * Stops - when it finds a word separator after a non-word element
-- * Fails - never.
--
-- @
-- S.wordsBy pred f = S.parseMany (PR.wordBy pred f)
-- @
--
{-# INLINE wordBy #-}
wordBy :: MonadCatch m => (a -> Bool) -> Fold m a b -> Parser m a b
wordBy f = K.toParserK . D.wordBy f

-- | Given an input stream @[a,b,c,...]@ and a comparison function @cmp@, the
-- parser assigns the element @a@ to the first group, then if @a \`cmp` b@ is
-- 'True' @b@ is also assigned to the same group.  If @a \`cmp` c@ is 'True'
-- then @c@ is also assigned to the same group and so on. When the comparison
-- fails the parser is terminated. Each group is folded using the 'Fold' @f@ and
-- the result of the fold is the result of the parser.
--
-- * Stops - when the comparison fails.
-- * Fails - never.
--
-- >>> :{
--  runGroupsBy eq =
--      Stream.toList
--          . Stream.parseMany (Parser.groupBy eq Fold.toList)
--          . Stream.fromList
-- :}
--
-- >>> runGroupsBy (<) []
-- []
--
-- >>> runGroupsBy (<) [1]
-- [[1]]
--
-- >>> runGroupsBy (<) [3, 5, 4, 1, 2, 0]
-- [[3,5,4],[1,2],[0]]
--
-- /Pre-release/
--
{-# INLINABLE groupBy #-}
groupBy :: MonadCatch m => (a -> a -> Bool) -> Fold m a b -> Parser m a b
groupBy eq = K.toParserK . D.groupBy eq

-- | Unlike 'groupBy' this combinator performs a rolling comparison of two
-- successive elements in the input stream.  Assuming the input stream to the
-- parser is @[a,b,c,...]@ and the comparison function is @cmp@, the parser
-- first assigns the element @a@ to the first group, then if @a \`cmp` b@ is
-- 'True' @b@ is also assigned to the same group.  If @b \`cmp` c@ is 'True'
-- then @c@ is also assigned to the same group and so on. When the comparison
-- fails the parser is terminated. Each group is folded using the 'Fold' @f@ and
-- the result of the fold is the result of the parser.
--
-- * Stops - when the comparison fails.
-- * Fails - never.
--
-- >>> :{
--  runGroupsByRolling eq =
--      Stream.toList
--          . Stream.parseMany (Parser.groupByRolling eq Fold.toList)
--          . Stream.fromList
-- :}
--
-- >>> runGroupsByRolling (<) []
-- []
--
-- >>> runGroupsByRolling (<) [1]
-- [[1]]
--
-- >>> runGroupsByRolling (<) [3, 5, 4, 1, 2, 0]
-- [[3,5],[4],[1,2],[0]]
--
-- /Pre-release/
--
{-# INLINABLE groupByRolling #-}
groupByRolling :: MonadCatch m => (a -> a -> Bool) -> Fold m a b -> Parser m a b
groupByRolling eq = K.toParserK . D.groupByRolling eq

-- | Match the given sequence of elements using the given comparison function.
--
-- >>> Stream.parse (Parser.eqBy (==) "string") $ Stream.fromList "string"
--
-- >>> Stream.parse (Parser.eqBy (==) "mismatch") $ Stream.fromList "match"
-- *** Exception: ParseError "eqBy: failed, yet to match 7 elements"
--
-- /Pre-release/
--
{-# INLINE eqBy #-}
eqBy :: MonadCatch m => (a -> a -> Bool) -> [a] -> Parser m a ()
eqBy cmp = K.toParserK . D.eqBy cmp

-------------------------------------------------------------------------------
-- nested parsers
-------------------------------------------------------------------------------

-- | Sequential parser application. Apply two parsers sequentially to an input
-- stream.  The input is provided to the first parser, when it is done the
-- remaining input is provided to the second parser. If both the parsers
-- succeed their outputs are combined using the supplied function. The
-- operation fails if any of the parsers fail.
--
-- Note: This is a parsing dual of appending streams using
-- 'Streamly.Prelude.serial', it splits the streams using two parsers and zips
-- the results.
--
-- This implementation is strict in the second argument, therefore, the
-- following will fail:
--
-- >>> Stream.parse (Parser.serialWith const (Parser.satisfy (> 0)) undefined) $ Stream.fromList [1]
-- *** Exception: Prelude.undefined
-- ...
--
-- Compare with 'Applicative' instance method '<*>'. This implementation allows
-- stream fusion but has quadratic complexity. This can fuse with other
-- operations and can be faster than 'Applicative' instance for small number
-- (less than 8) of compositions.
--
-- Many combinators can be expressed using @serialWith@ and other parser
-- primitives. Some common idioms are described below,
--
-- @
-- span :: (a -> Bool) -> Fold m a b -> Fold m a b -> Parser m a b
-- span pred f1 f2 = serialWith (,) ('takeWhile' pred f1) ('fromFold' f2)
-- @
--
-- @
-- spanBy :: (a -> a -> Bool) -> Fold m a b -> Fold m a b -> Parser m a b
-- spanBy eq f1 f2 = serialWith (,) ('groupBy' eq f1) ('fromFold' f2)
-- @
--
-- @
-- spanByRolling :: (a -> a -> Bool) -> Fold m a b -> Fold m a b -> Parser m a b
-- spanByRolling eq f1 f2 = serialWith (,) ('groupByRolling' eq f1) ('fromFold' f2)
-- @
--
-- /Pre-release/
--
{-# INLINE serialWith #-}
serialWith :: MonadCatch m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
serialWith f p1 p2 =
    K.toParserK $ D.serialWith f (K.fromParserK p1) (K.fromParserK p2)

-- | Sequential parser application ignoring the output of the first parser.
-- Apply two parsers sequentially to an input stream.  The input is provided to
-- the first parser, when it is done the remaining input is provided to the
-- second parser. The output of the parser is the output of the second parser.
-- The operation fails if any of the parsers fail.
--
-- This implementation is strict in the second argument, therefore, the
-- following will fail:
--
-- >>> Stream.parse (Parser.split_ (Parser.satisfy (> 0)) undefined) $ Stream.fromList [1]
-- *** Exception: Prelude.undefined
-- ...
--
-- Compare with 'Applicative' instance method '*>'. This implementation allows
-- stream fusion but has quadratic complexity. This can fuse with other
-- operations, and can be faster than 'Applicative' instance for small
-- number (less than 8) of compositions.
--
-- /Pre-release/
--
{-# INLINE split_ #-}
split_ :: MonadCatch m => Parser m x a -> Parser m x b -> Parser m x b
split_ p1 p2 = K.toParserK $ D.split_ (K.fromParserK p1) (K.fromParserK p2)

-- | @teeWith f p1 p2@ distributes its input to both @p1@ and @p2@ until both
-- of them succeed or anyone of them fails and combines their output using @f@.
-- The parser succeeds if both the parsers succeed.
--
-- /Pre-release/
--
{-# INLINE teeWith #-}
teeWith :: MonadCatch m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
teeWith f p1 p2 =
    K.toParserK $ D.teeWith f (K.fromParserK p1) (K.fromParserK p2)

-- | Like 'teeWith' but ends parsing and zips the results, if available,
-- whenever the first parser ends.
--
-- /Pre-release/
--
{-# INLINE teeWithFst #-}
teeWithFst :: MonadCatch m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
teeWithFst f p1 p2 =
    K.toParserK $ D.teeWithFst f (K.fromParserK p1) (K.fromParserK p2)

-- | Like 'teeWith' but ends parsing and zips the results, if available,
-- whenever any of the parsers ends or fails.
--
-- /Unimplemented/
--
{-# INLINE teeWithMin #-}
teeWithMin :: MonadCatch m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
teeWithMin f p1 p2 =
    K.toParserK $ D.teeWithMin f (K.fromParserK p1) (K.fromParserK p2)

-- | Sequential alternative. Apply the input to the first parser and return the
-- result if the parser succeeds. If the first parser fails then backtrack and
-- apply the same input to the second parser and return the result.
--
-- Note: This implementation is not lazy in the second argument. The following
-- will fail:
--
-- >>> Stream.parse (Parser.satisfy (> 0) `Parser.alt` undefined) $ Stream.fromList [1..10]
-- 1
--
-- Compare with 'Alternative' instance method '<|>'. This implementation allows
-- stream fusion but has quadratic complexity. This can fuse with other
-- operations and can be much faster than 'Alternative' instance for small
-- number (less than 8) of alternatives.
--
-- /Pre-release/
--
{-# INLINE alt #-}
alt :: MonadCatch m => Parser m x a -> Parser m x a -> Parser m x a
alt p1 p2 = K.toParserK $ D.alt (K.fromParserK p1) (K.fromParserK p2)

-- | Shortest alternative. Apply both parsers in parallel but choose the result
-- from the one which consumed least input i.e. take the shortest succeeding
-- parse.
--
-- /Pre-release/
--
{-# INLINE shortest #-}
shortest :: MonadCatch m
    => Parser m x a -> Parser m x a -> Parser m x a
shortest p1 p2 = K.toParserK $ D.shortest (K.fromParserK p1) (K.fromParserK p2)

-- | Longest alternative. Apply both parsers in parallel but choose the result
-- from the one which consumed more input i.e. take the longest succeeding
-- parse.
--
-- /Pre-release/
--
{-# INLINE longest #-}
longest :: MonadCatch m
    => Parser m x a -> Parser m x a -> Parser m x a
longest p1 p2 = K.toParserK $ D.longest (K.fromParserK p1) (K.fromParserK p2)

-- | Run a parser without consuming the input.
--
-- /Pre-release/
--
{-# INLINE lookAhead #-}
lookAhead :: MonadCatch m => Parser m a b -> Parser m a b
lookAhead p = K.toParserK $ D.lookAhead $ K.fromParserK p

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------
--
-- To deinterleave we can chain two parsers one behind the other. The input is
-- given to the first parser and the input definitively rejected by the first
-- parser is given to the second parser.
--
-- We can either have the parsers themselves buffer the input or use the shared
-- global buffer to hold it until none of the parsers need it. When the first
-- parser returns Skip (i.e. rewind) we let the second parser consume the
-- rejected input and when it is done we move the cursor forward to the first
-- parser again. This will require a "move forward" command as well.
--
-- To implement grep we can use three parsers, one to find the pattern, one
-- to store the context behind the pattern and one to store the context in
-- front of the pattern. When a match occurs we need to emit the accumulator of
-- all the three parsers. One parser can count the line numbers to provide the
-- line number info.
--
-- | Apply two parsers alternately to an input stream. The input stream is
-- considered an interleaving of two patterns. The two parsers represent the
-- two patterns.
--
-- This undoes a "gintercalate" of two streams.
--
-- /Unimplemented/
--
{-# INLINE deintercalate #-}
deintercalate ::
    -- Monad m =>
       Fold m a y -> Parser m x a
    -> Fold m b z -> Parser m x b
    -> Parser m x (y, z)
deintercalate = undefined

-------------------------------------------------------------------------------
-- Sequential Collection
-------------------------------------------------------------------------------
--
-- | @concatSequence f t@ collects sequential parses of parsers in the
-- container @t@ using the fold @f@. Fails if the input ends or any of the
-- parsers fail.
--
-- This is same as 'Data.Traversable.sequence' but more efficient.
--
-- /Unimplemented/
--
{-# INLINE concatSequence #-}
concatSequence ::
    -- Foldable t =>
    Fold m b c -> t (Parser m a b) -> Parser m a c
concatSequence _f _p = undefined

-- | Map a 'Parser' returning function on the result of a 'Parser'.
--
-- Compare with 'Monad' instance method '>>='. This implementation allows
-- stream fusion but has quadratic complexity. This can fuse with other
-- operations and can be much faster than 'Monad' instance for small number
-- (less than 8) of compositions.
--
-- /Pre-release/
--
{-# INLINE concatMap #-}
concatMap :: MonadCatch m
    => (b -> Parser m a c) -> Parser m a b -> Parser m a c
concatMap f p = K.toParserK $ D.concatMap (K.fromParserK . f) (K.fromParserK p)

-------------------------------------------------------------------------------
-- Alternative Collection
-------------------------------------------------------------------------------
--
-- | @choice parsers@ applies the @parsers@ in order and returns the first
-- successful parse.
--
-- This is same as 'asum' but more efficient.
--
-- /Unimplemented/
--
{-# INLINE choice #-}
choice ::
    -- Foldable t =>
    t (Parser m a b) -> Parser m a b
choice _ps = undefined

-------------------------------------------------------------------------------
-- Sequential Repetition
-------------------------------------------------------------------------------
--
-- $many
-- TODO "many" is essentially a Fold because it cannot fail. So it can be
-- downgraded to a Fold. Or we can make the return type a Fold instead and
-- upgrade that to a parser when needed.

-- | Like 'many' but uses a 'Parser' instead of a 'Fold' to collect the
-- results. Parsing stops or fails if the collecting parser stops or fails.
--
-- /Unimplemented/
--
{-# INLINE manyP #-}
manyP :: -- MonadCatch m =>
    Parser m a b -> Parser m b c -> Parser m a c
manyP _p _f = undefined -- K.toParserK $ D.manyP (K.fromParserK p) f

-- | Collect zero or more parses. Apply the supplied parser repeatedly on the
-- input stream and push the parse results to a downstream fold.
--
--  Stops: when the downstream fold stops or the parser fails.
--  Fails: never, produces zero or more results.
--
-- Compare with 'Control.Applicative.many'.
--
-- /Pre-release/
--
{-# INLINE many #-}
many :: MonadCatch m => Parser m a b -> Fold m b c -> Parser m a c
many p f = K.toParserK $ D.many (K.fromParserK p) f
-- many = countBetween 0 maxBound

-- Note: many1 would perhaps be a better name for this and consistent with
-- other names like takeWhile1. But we retain the name "some" for
-- compatibility.
--
-- | Collect one or more parses. Apply the supplied parser repeatedly on the
-- input stream and push the parse results to a downstream fold.
--
--  Stops: when the downstream fold stops or the parser fails.
--  Fails: if it stops without producing a single result.
--
-- @some fld parser = manyP (takeGE 1 fld) parser@
--
-- Compare with 'Control.Applicative.some'.
--
-- /Pre-release/
--
{-# INLINE some #-}
some :: MonadCatch m => Parser m a b -> Fold m b c -> Parser m a c
some p f = K.toParserK $ D.some (K.fromParserK p) f
-- some p f = manyP p (takeGE 1 f)
-- many = countBetween 1 maxBound

-- | @countBetween m n f p@ collects between @m@ and @n@ sequential parses of
-- parser @p@ using the fold @f@. Stop after collecting @n@ results. Fails if
-- the input ends or the parser fails before @m@ results are collected.
--
-- /Unimplemented/
--
{-# INLINE countBetween #-}
countBetween ::
    -- MonadCatch m =>
    Int -> Int -> Parser m a b -> Fold m b c -> Parser m a c
countBetween _m _n _p = undefined
-- countBetween m n p f = manyP (takeBetween m n f) p

-- | @count n f p@ collects exactly @n@ sequential parses of parser @p@ using
-- the fold @f@.  Fails if the input ends or the parser fails before @n@
-- results are collected.
--
-- /Unimplemented/
--
{-# INLINE count #-}
count ::
    -- MonadCatch m =>
    Int -> Parser m a b -> Fold m b c -> Parser m a c
count n = countBetween n n
-- count n p f = manyP (takeEQ n f) p

-- | Like 'manyTill' but uses a 'Parser' to collect the results instead of a
-- 'Fold'.  Parsing stops or fails if the collecting parser stops or fails.
--
-- We can implemnent parsers like the following using 'manyTillP':
--
-- @
-- countBetweenTill m n f p = manyTillP (takeBetween m n f) p
-- @
--
-- /Unimplemented/
--
{-# INLINE manyTillP #-}
manyTillP :: -- MonadCatch m =>
    Parser m a b -> Parser m a x -> Parser m b c -> Parser m a c
manyTillP _p1 _p2 _f = undefined
    -- K.toParserK $ D.manyTillP (K.fromParserK p1) (K.fromParserK p2) f

-- | @manyTill f collect test@ tries the parser @test@ on the input, if @test@
-- fails it backtracks and tries @collect@, after @collect@ succeeds @test@ is
-- tried again and so on. The parser stops when @test@ succeeds.  The output of
-- @test@ is discarded and the output of @collect@ is accumulated by the
-- supplied fold. The parser fails if @collect@ fails.
--
-- Stops when the fold @f@ stops.
--
-- /Pre-release/
--
{-# INLINE manyTill #-}
manyTill :: MonadCatch m
    => Parser m a b -> Parser m a x -> Fold m b c -> Parser m a c
manyTill p1 p2 f =
    K.toParserK $ D.manyTill f (K.fromParserK p1) (K.fromParserK p2)

-- | @manyThen f collect recover@ repeats the parser @collect@ on the input and
-- collects the output in the supplied fold. If the the parser @collect@ fails,
-- parser @recover@ is run until it stops and then we start repeating the
-- parser @collect@ again. The parser fails if the recovery parser fails.
--
-- For example, this can be used to find a key frame in a video stream after an
-- error.
--
-- /Unimplemented/
--
{-# INLINE manyThen #-}
manyThen :: -- (Foldable t, MonadCatch m) =>
    Parser m a b -> Parser m a x -> Fold m b c -> Parser m a c
manyThen _parser _recover _f = undefined

-------------------------------------------------------------------------------
-- Interleaving a collection of parsers
-------------------------------------------------------------------------------
--
-- | Apply a collection of parsers to an input stream in a round robin fashion.
-- Each parser is applied until it stops and then we repeat starting with the
-- the first parser again.
--
-- /Unimplemented/
--
{-# INLINE roundRobin #-}
roundRobin :: -- (Foldable t, MonadCatch m) =>
    t (Parser m a b) -> Fold m b c -> Parser m a c
roundRobin _ps _f = undefined

-------------------------------------------------------------------------------
-- Repeated Alternatives
-------------------------------------------------------------------------------

-- | Keep trying a parser up to a maximum of @n@ failures.  When the parser
-- fails the input consumed till now is dropped and the new instance is tried
-- on the fresh input.
--
-- /Unimplemented/
--
{-# INLINE retryMaxTotal #-}
retryMaxTotal :: -- (MonadCatch m) =>
    Int -> Parser m a b -> Fold m b c -> Parser m a c
retryMaxTotal _n _p _f  = undefined

-- | Like 'retryMaxTotal' but aborts after @n@ successive failures.
--
-- /Unimplemented/
--
{-# INLINE retryMaxSuccessive #-}
retryMaxSuccessive :: -- (MonadCatch m) =>
    Int -> Parser m a b -> Fold m b c -> Parser m a c
retryMaxSuccessive _n _p _f = undefined

-- | Keep trying a parser until it succeeds.  When the parser fails the input
-- consumed till now is dropped and the new instance is tried on the fresh
-- input.
--
-- /Unimplemented/
--
{-# INLINE retry #-}
retry :: -- (MonadCatch m) =>
    Parser m a b -> Parser m a b
retry _p = undefined
