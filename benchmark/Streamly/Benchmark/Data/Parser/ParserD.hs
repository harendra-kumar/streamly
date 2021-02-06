-- |
-- Module      : Streamly.Benchmark.Data.ParserD
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fspec-constr-recursive=4 #-}

module Main
  (
    main
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Data.Foldable (asum)
import Data.Functor (($>))
import System.Random (randomRIO)
import Prelude hiding (any, all, take, sequence, sequenceA, takeWhile)

import qualified Data.Traversable as TR
import qualified Data.Foldable as F
import qualified Control.Applicative as AP
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser.ParserD as PR
import qualified Streamly.Internal.Data.Stream.IsStream as IP
import qualified Streamly.Internal.Data.Array.Foreign as Array

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

{-# INLINE benchIO #-}
benchIO
    :: NFData b
    => String -> (Int -> t IO a) -> (t IO a -> IO b) -> Benchmark
benchIO name src sink =
    bench name $ nfIO $ randomRIO (1,1) >>= sink . src

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE drainWhile #-}
drainWhile :: MonadThrow m => (a -> Bool) -> PR.Parser m a ()
drainWhile p = PR.takeWhile p FL.drain

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
    IP.parseD (PR.many FL.drain (PR.wordBy (Prelude.even) FL.drain))

{-# INLINE many #-}
many :: MonadCatch m => SerialT m Int -> m Int
many = IP.parseD (PR.many FL.length (PR.satisfy (> 0)))

{-# INLINE manyAlt #-}
manyAlt :: MonadCatch m => SerialT m Int -> m Int
manyAlt xs = do
    x <- IP.parseD (AP.many (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE some #-}
some :: MonadCatch m => SerialT m Int -> m Int
some = IP.parseD (PR.some FL.length (PR.satisfy (> 0)))

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

{-# INLINE splitWith #-}
splitWith :: MonadThrow m
    => Int -> SerialT m Int -> m ((), ())
splitWith value =
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
-- Array parsers
-------------------------------------------------------------------------------

{-# INLINE parseArray #-}
parseArray :: Int -> SerialT IO (Array.Array Int) -> IO ()
parseArray value s = void $ IP.parseArrayD (drainWhile (< value)) s

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
    , benchIOSink value "groupBy" $ groupBy
    , benchIOSink value "groupByRolling" $ groupByRolling
    , benchIOSink value "wordBy" $ wordBy value
    , benchIOSink value "splitWith" $ splitWith value
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

o_1_space_serial_array :: Int -> [Benchmark]
o_1_space_serial_array bound =
    [ benchIO "parseArray (100)" (arrayStream 100) $ parseArray bound
    , benchIO "parseArray (bound)" (arrayStream bound) $ parseArray bound
    ]

    where

    {-# INLINE arrayStream #-}
    arrayStream chunkSize start =
        IP.chunksOf chunkSize (Array.writeN chunkSize)
            $ sourceUnfoldrM bound start

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
    , benchIOSink value "choice/100" $ choice (value `div` 100)
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    value `seq` runMode (mode cfg) cfg benches (allBenchmarks value)

    where

    allBenchmarks value =
        [ bgroup (o_1_space_prefix moduleName) (o_1_space_serial value)
        , bgroup (o_1_space_prefix moduleName) (o_1_space_serial_array value)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        , bgroup (o_n_space_prefix moduleName) (o_n_space_serial value)
        ]
