-- |
-- Module      : Streamly.Benchmark.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fspec-constr-recursive=4 #-}

module Main
  (
    main
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Data.Foldable (asum)
import System.Random (randomRIO)
import Prelude hiding (any, all, take, sequence, sequenceA, takeWhile)

import qualified Data.Traversable as TR
import qualified Control.Applicative as AP
import qualified Streamly as S hiding (runStream)
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Prelude as IP

import Gauge
import Streamly hiding (runStream)
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- XXX these can be moved to the common module

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
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

{-# INLINE any #-}
any :: (MonadThrow m, Ord a) => a -> SerialT m a -> m Bool
any value = IP.parse (PR.any (> value))

{-# INLINE all #-}
all :: (MonadThrow m, Ord a) => a -> SerialT m a -> m Bool
all value = IP.parse (PR.all (<= value))

{-# INLINE take #-}
take :: MonadThrow m => Int -> SerialT m a -> m ()
take value = IP.parse (PR.take value FL.drain)

{-# INLINE takeWhile #-}
takeWhile :: MonadThrow m => Int -> SerialT m Int -> m ()
takeWhile value = IP.parse (PR.takeWhile (<= value) FL.drain)

{-# INLINE many #-}
many :: MonadCatch m => SerialT m Int -> m Int
many = IP.parse (PR.many FL.length (PR.satisfy (> 0)))

{-# INLINE manyAlt #-}
manyAlt :: MonadCatch m => SerialT m Int -> m Int
manyAlt xs = do
    x <- IP.parse (AP.many (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE some #-}
some :: MonadCatch m => SerialT m Int -> m Int
some = IP.parse (PR.some FL.length (PR.satisfy (> 0)))

{-# INLINE someAlt #-}
someAlt :: MonadCatch m => SerialT m Int -> m Int
someAlt xs = do
    x <- IP.parse (AP.some (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE manyTill #-}
manyTill :: MonadCatch m => Int -> SerialT m Int -> m Int
manyTill value =
    IP.parse (PR.manyTill FL.length (PR.satisfy (> 0)) (PR.satisfy (== value)))

{-# INLINE splitAllAny #-}
splitAllAny :: MonadThrow m
    => Int -> SerialT m Int -> m (Bool, Bool)
splitAllAny value =
    IP.parse ((,) <$> PR.all (<= (value `div` 2)) <*> PR.any (> value))

{-# INLINE teeAllAny #-}
teeAllAny :: (MonadThrow m, Ord a)
    => a -> SerialT m a -> m (Bool, Bool)
teeAllAny value =
    IP.parse (PR.teeWith (,) (PR.all (<= value)) (PR.any (> value)))

{-# INLINE teeFstAllAny #-}
teeFstAllAny :: (MonadThrow m, Ord a)
    => a -> SerialT m a -> m (Bool, Bool)
teeFstAllAny value =
    IP.parse (PR.teeWithFst (,) (PR.all (<= value)) (PR.any (> value)))

{-# INLINE shortestAllAny #-}
shortestAllAny :: (MonadThrow m, Ord a)
    => a -> SerialT m a -> m Bool
shortestAllAny value =
    IP.parse (PR.shortest (PR.all (<= value)) (PR.any (> value)))

{-# INLINE longestAllAny #-}
longestAllAny :: (MonadCatch m, Ord a)
    => a -> SerialT m a -> m Bool
longestAllAny value =
    IP.parse (PR.longest (PR.all (<= value)) (PR.any (> value)))

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
    IP.parse (PR.lookAhead (PR.takeWhile (<= value) FL.drain) *> pure ())

-- quadratic complexity
{-# INLINE sequenceA #-}
sequenceA :: MonadThrow m => Int -> SerialT m Int -> m Int
sequenceA value xs = do
    x <- IP.parse (TR.sequenceA (replicate value (PR.satisfy (> 0)))) xs
    return $ length x

-- quadratic complexity
{-# INLINE sequence #-}
sequence :: MonadThrow m => Int -> SerialT m Int -> m Int
sequence value xs = do
    x <- IP.parse (TR.sequence (replicate value (PR.satisfy (> 0)))) xs
    return $ length x

-- choice using the "Alternative" instance with direct style parser type has
-- quadratic performance complexity.
--
{-# INLINE choice #-}
choice :: MonadCatch m => Int -> SerialT m Int -> m Int
choice value = do
    IP.parse (asum (replicate value (PR.satisfy (< 0)))
        AP.<|> PR.satisfy (> 0))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

o_1_space_serial_parse :: Int -> [Benchmark]
o_1_space_serial_parse value =
    [ benchIOSink value "any" $ any value
    , benchIOSink value "all" $ all value
    , benchIOSink value "take" $ take value
    , benchIOSink value "takeWhile" $ takeWhile value
    , benchIOSink value "lookAhead" $ lookAhead value
    , benchIOSink value "split (all,any)" $ splitAllAny value
    , benchIOSink value "many" many
    , benchIOSink value "some" some
    , benchIOSink value "manyAlt" manyAlt
    , benchIOSink value "someAlt" someAlt
    , benchIOSink value "manyTill" $ manyTill value
    , benchIOSink value "choice/100" $ choice (value `div` 100)
    , benchIOSink value "tee (all,any)" $ teeAllAny value
    , benchIOSink value "teeFst (all,any)" $ teeFstAllAny value
    , benchIOSink value "shortest (all,any)" $ shortestAllAny value
    , benchIOSink value "longest (all,any)" $ longestAllAny value
    , benchIOSink value "sequenceA/100" $ sequenceA (value `div` 100)
    , benchIOSink value "sequence/100" $ sequence (value `div` 100)
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
        [ bgroup "o1"
            [ bgroup "parser" $ concat
                [
                  o_1_space_serial_parse value
                ]
            ]
        ]
