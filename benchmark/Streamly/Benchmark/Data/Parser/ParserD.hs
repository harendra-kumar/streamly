-- |
-- Module      : Streamly.Benchmark.Data.ParserD
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fspec-constr-recursive=4 #-}

module Main
  (
    main
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import System.Random (randomRIO)
import Prelude hiding (any, all, take, sequence, sequenceA, sequence_, takeWhile, span)

import qualified Data.Traversable as TR
import qualified Data.Foldable as F
import qualified Control.Applicative as AP
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser.ParserD as PR
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Producer.Source as Source
import qualified Streamly.Internal.Data.Stream.IsStream as IP

import Gauge
import Streamly.Prelude (SerialT, MonadAsync, IsStream)
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- XXX these can be moved to the common module

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (IsStream t, MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrM value n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: (IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrM value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE drainWhile #-}
drainWhile :: MonadThrow m => (a -> Bool) -> PR.Parser m a ()
drainWhile p = PR.takeWhile p FL.drain

{-# INLINE sliceBeginWith #-}
sliceBeginWith :: MonadCatch m => Int -> SerialT m Int -> m ()
sliceBeginWith value stream = do
    stream1 <- return . fromMaybe (S.value (value + 1)) =<< S.tail stream
    let stream2 = value `S.cons` stream1
    IP.parseD (PR.sliceBeginWith (== value) FL.drain) stream2

{-# INLINE takeWhile #-}
takeWhile :: MonadThrow m => Int -> SerialT m Int -> m ()
takeWhile value = IP.parseD (drainWhile (<= value))

{-# INLINE groupBy #-}
groupBy :: MonadThrow m => SerialT m Int -> m ()
groupBy = IP.parseD (PR.groupBy (<=) FL.drain)

{-# INLINE groupByRolling #-}
groupByRolling :: MonadThrow m => SerialT m Int -> m ()
groupByRolling = IP.parseD (PR.groupByRolling (<=) FL.drain)

{-# INLINE wordBy #-}
wordBy :: MonadThrow m => Int -> SerialT m Int -> m ()
wordBy value = IP.parseD (PR.wordBy (>= value) FL.drain)

{-# INLINE manyWordByEven #-}
manyWordByEven :: MonadCatch m => SerialT m Int -> m ()
manyWordByEven =
    IP.parseD (PR.many (PR.wordBy (Prelude.even) FL.drain) FL.drain)

{-# INLINE many #-}
many :: MonadCatch m => SerialT m Int -> m Int
many = IP.parseD (PR.many (PR.satisfy (> 0)) FL.length)

{-# INLINE manyAlt #-}
manyAlt :: MonadCatch m => SerialT m Int -> m Int
manyAlt xs = do
    x <- IP.parseD (AP.many (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE some #-}
some :: MonadCatch m => SerialT m Int -> m Int
some = IP.parseD (PR.some (PR.satisfy (> 0)) FL.length)

{-# INLINE someAlt #-}
someAlt :: MonadCatch m => SerialT m Int -> m Int
someAlt xs = do
    x <- IP.parseD (AP.some (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-#INLINE sliceSepByP #-}
sliceSepByP :: MonadCatch m => Int -> SerialT m Int -> m ()
sliceSepByP value = IP.parseD (PR.sliceSepByP (>= value) (PR.fromFold FL.drain))

{-# INLINE manyTill #-}
manyTill :: MonadCatch m => Int -> SerialT m Int -> m Int
manyTill value =
    let p = PR.satisfy (> 0)
        pcond = PR.satisfy (== value)
    in IP.parseD (PR.manyTill FL.length p pcond)

{-# INLINE serialWith #-}
serialWith :: MonadThrow m
    => Int -> SerialT m Int -> m ((), ())
serialWith value =
    IP.parseD
        ((,)
            <$> drainWhile (<= (value `div` 2))
            <*> drainWhile (<= value)
        )

{-# INLINE teeAllAny #-}
teeAllAny :: MonadThrow m
    => Int -> SerialT m Int -> m ((), ())
teeAllAny value =
    IP.parseD
        (PR.teeWith (,)
            (drainWhile (<= value))
            (drainWhile (<= value))
        )

{-# INLINE teeFstAllAny #-}
teeFstAllAny :: MonadThrow m
    => Int -> SerialT m Int -> m ((), ())
teeFstAllAny value =
    IP.parseD
        (PR.teeWithFst (,)
            (drainWhile (<= value))
            (drainWhile (<= value))
        )

{-# INLINE shortestAllAny #-}
shortestAllAny :: MonadThrow m
    => Int -> SerialT m Int -> m ()
shortestAllAny value =
    IP.parseD
        (PR.shortest
            (drainWhile (<= value))
            (drainWhile (<= value))
        )

{-# INLINE longestAllAny #-}
longestAllAny :: MonadCatch m
    => Int -> SerialT m Int -> m ()
longestAllAny value =
    IP.parseD
        (PR.longest
            (drainWhile (<= value))
            (drainWhile (<= value))
        )

-------------------------------------------------------------------------------
-- Spanning
-------------------------------------------------------------------------------

{-# INLINE span #-}
span :: MonadThrow m => Int -> SerialT m Int -> m ((), ())
span value = IP.parseD (PR.span (<= (value `div` 2)) FL.drain FL.drain)

{-# INLINE spanBy #-}
spanBy :: MonadThrow m => Int -> SerialT m Int -> m ((), ())
spanBy value =
    IP.parseD (PR.spanBy (\_ i -> i <= (value `div` 2)) FL.drain FL.drain)

{-# INLINE spanByRolling #-}
spanByRolling :: MonadThrow m => Int -> SerialT m Int -> m ((), ())
spanByRolling value =
    IP.parseD (PR.spanByRolling (\_ i -> i <= value `div` 2) FL.drain FL.drain)

-------------------------------------------------------------------------------
-- Nested
-------------------------------------------------------------------------------

{-# INLINE parseMany #-}
parseMany :: MonadThrow m => Int -> SerialT m Int -> m ()
parseMany value = IP.drain . IP.parseManyD (drainWhile (<= value + 1))

{-# INLINE parseManyGroups #-}
parseManyGroups :: MonadThrow m => Bool -> SerialT m Int -> m ()
parseManyGroups b = IP.drain . IP.parseManyD (PR.groupBy (\_ _ -> b) FL.drain)

{-# INLINE parseManyGroupsRolling #-}
parseManyGroupsRolling :: MonadThrow m => Bool -> SerialT m Int -> m ()
parseManyGroupsRolling b =
    IP.drain . IP.parseManyD (PR.groupByRolling (\_ _ -> b) FL.drain)

-------------------------------------------------------------------------------
-- Parsing with unfolds
-------------------------------------------------------------------------------

{-# INLINE parseManyUnfoldArrays #-}
parseManyUnfoldArrays :: Int -> [Array.Array Int] -> IO ()
parseManyUnfoldArrays count arrays = do
    let src = Source.source (Just (Producer.OuterLoop arrays))
    let parser = PR.fromFold (FL.take count FL.drain)
    let readSrc =
            Source.producer
                $ Producer.concat Producer.fromList Array.producer
    let streamParser =
            Producer.simplify (Source.parseManyD parser readSrc)
    S.drain $ S.unfold streamParser src

-------------------------------------------------------------------------------
-- Parsers in which -fspec-constr-recursive=16 is problematic
-------------------------------------------------------------------------------

-- XXX -fspec-constr-recursive=16 makes GHC go beserk when compiling these.
-- We need to fix GHC so that we can have better control over that option or do
-- not have to rely on it.
--
{-# INLINE lookAhead #-}
lookAhead :: MonadThrow m => Int -> SerialT m Int -> m ()
lookAhead value =
    IP.parseD (PR.lookAhead (PR.takeWhile (<= value) FL.drain) $> ())

{-# INLINE sequenceA_ #-}
sequenceA_ :: MonadThrow m => Int -> SerialT m Int -> m ()
sequenceA_ value =
    IP.parseD (F.sequenceA_ $ replicate value (PR.satisfy (> 0)))

-- quadratic complexity
{-# INLINE sequenceA #-}
sequenceA :: MonadThrow m => Int -> SerialT m Int -> m Int
sequenceA value xs = do
    x <- IP.parseD (TR.sequenceA (replicate value (PR.satisfy (> 0)))) xs
    return $ length x

-- quadratic complexity
{-# INLINE sequence #-}
sequence :: MonadThrow m => Int -> SerialT m Int -> m Int
sequence value xs = do
    x <- IP.parseD (TR.sequence (replicate value (PR.satisfy (> 0)))) xs
    return $ length x

{-# INLINE sequence_ #-}
sequence_ :: MonadCatch m => Int -> SerialT m Int -> m ()
sequence_ value xs =
    IP.parseD (foldr f (return ()) (replicate value (PR.takeBetween 0 1 FL.drain))) xs

    where

    {-# INLINE f #-}
    f m k = m >>= (\_ -> k)

-- choice using the "Alternative" instance with direct style parser type has
-- quadratic performance complexity.
--
{-# INLINE choice #-}
choice :: MonadCatch m => Int -> SerialT m Int -> m Int
choice value =
    IP.parseD (asum (replicate value (PR.satisfy (< 0)))
        AP.<|> PR.satisfy (> 0))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Parser.ParserD"

o_1_space_serial :: Int -> [Benchmark]
o_1_space_serial value =
    [ benchIOSink value "takeWhile" $ takeWhile value
    , benchIOSink value "sliceBeginWith" $ sliceBeginWith value
    , benchIOSink value "groupBy" $ groupBy
    , benchIOSink value "groupByRolling" $ groupByRolling
    , benchIOSink value "wordBy" $ wordBy value
    , benchIOSink value "serialWith" $ serialWith value
    , benchIOSink value "many" many
    , benchIOSink value "many (wordBy even)" $ manyWordByEven
    , benchIOSink value "some" some
    , benchIOSink value "sliceSepByP" $ sliceSepByP value
    , benchIOSink value "manyTill" $ manyTill value
    , benchIOSink value "tee (all,any)" $ teeAllAny value
    , benchIOSink value "teeFst (all,any)" $ teeFstAllAny value
    , benchIOSink value "shortest (all,any)" $ shortestAllAny value
    , benchIOSink value "longest (all,any)" $ longestAllAny value
    ]

o_1_space_serial_spanning :: Int -> [Benchmark]
o_1_space_serial_spanning value =
    [ benchIOSink value "span" $ span value
    , benchIOSink value "spanBy" $ spanBy value
    , benchIOSink value "spanByRolling" $ spanByRolling value
    ]

o_1_space_serial_nested :: Int -> [Benchmark]
o_1_space_serial_nested value =
    [ benchIOSink value "parseMany" $ parseMany value
    , benchIOSink value "parseMany groupBy (bound groups)"
          $ (parseManyGroups False)
    , benchIOSink value "parseMany groupRollingBy (bound groups)"
          $ (parseManyGroupsRolling False)
    , benchIOSink value "parseMany groupBy (1 group)" $ (parseManyGroups True)
    , benchIOSink value "parseMany groupRollingBy (1 group)"
          $ (parseManyGroupsRolling True)
    ]

o_1_space_serial_unfold :: Int -> [Array.Array Int] -> [Benchmark]
o_1_space_serial_unfold bound arrays =
    [ bench "parseMany/Unfold/1000 arrays/1 parse"
        $ nfIO $ parseManyUnfoldArrays bound arrays
    , bench "parseMany/Unfold/1000 arrays/100000 parses"
        $ nfIO $ parseManyUnfoldArrays 1 arrays
    ]

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [
    -- lookahead benchmark holds the entire input till end
      benchIOSink value "lookAhead" $ lookAhead value

    -- These show non-linear time complexity
    -- They accumulate the results in a list.
    , benchIOSink value "manyAlt" manyAlt
    , benchIOSink value "someAlt" someAlt
    ]

-- accumulate results in a list in IO
o_n_space_serial :: Int -> [Benchmark]
o_n_space_serial value =
    [ benchIOSink value "sequenceA/100" $ sequenceA (value `div` 100)
    , benchIOSink value "sequenceA_/100" $ sequenceA_ (value `div` 100)
    , benchIOSink value "sequence/100" $ sequence (value `div` 100)
    , benchIOSink value "sequence_/100" $ sequence_ (value `div` 100)
    , benchIOSink value "choice/100" $ choice (value `div` 100)
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    arrays <- IP.toList $ IP.arraysOf 100 $ sourceUnfoldrM value 0
    value `seq` runMode (mode cfg) cfg benches (allBenchmarks value arrays)

    where

    allBenchmarks value arrays =
        [ bgroup (o_1_space_prefix moduleName) (o_1_space_serial value)
        , bgroup (o_1_space_prefix moduleName) (o_1_space_serial_spanning value)
        , bgroup (o_1_space_prefix moduleName) (o_1_space_serial_nested value)
        , bgroup (o_1_space_prefix moduleName)
            (o_1_space_serial_unfold value arrays)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        , bgroup (o_n_space_prefix moduleName) (o_n_space_serial value)
        ]
