-- |
-- Module      : Streamly.Benchmark.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Main
  (
    main
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.Catch (MonadCatch)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..))
import GHC.Magic (inline)
#if __GLASGOW_HASKELL__ >= 802
import GHC.Magic (noinline)
#else
#define noinline
#endif
import System.IO (Handle)
import System.Random (randomRIO)
import Prelude
       hiding (any, all, take, sequence, sequence_, sequenceA, takeWhile)

import qualified Data.Traversable as TR
import qualified Data.Foldable as F
import qualified Control.Applicative as AP
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream.IsStream as IP
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Producer.Source as Source

import Gauge hiding (env)
import Streamly.Prelude (SerialT)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- XXX these can be moved to the common module

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
    :: (S.IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrM value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE takeBetween #-}
takeBetween :: MonadCatch m => Int -> SerialT m a -> m ()
takeBetween value =  IP.parse (PR.takeBetween 0 value FL.drain)

{-# INLINE takeEQ #-}
takeEQ :: MonadCatch m => Int -> SerialT m a -> m ()
takeEQ value = IP.parse (PR.takeEQ value FL.drain)

{-# INLINE drainWhile #-}
drainWhile :: MonadCatch m => Int -> SerialT m Int -> m ()
drainWhile value = IP.parse (PR.drainWhile (<= value))

{-# INLINE sliceBeginWith #-}
sliceBeginWith :: MonadCatch m => Int -> SerialT m Int -> m ()
sliceBeginWith value stream = do
    stream1 <- return . fromMaybe (S.value (value + 1)) =<< S.tail stream
    let stream2 = value `S.cons` stream1
    IP.parse (PR.sliceBeginWith (== value) FL.drain) stream2

{-# INLINE takeWhile #-}
takeWhile :: MonadCatch m => Int -> SerialT m Int -> m ()
takeWhile value = IP.parse (PR.takeWhile (<= value) FL.drain)

{-# INLINE groupBy #-}
groupBy :: MonadCatch m => SerialT m Int -> m ()
groupBy = IP.parse (PR.groupBy (<=) FL.drain)

{-# INLINE groupByRolling #-}
groupByRolling :: MonadCatch m => SerialT m Int -> m ()
groupByRolling = IP.parse (PR.groupByRolling (<=) FL.drain)

{-# INLINE wordBy #-}
wordBy :: MonadCatch m => Int -> SerialT m Int -> m ()
wordBy value = IP.parse (PR.wordBy (>= value) FL.drain)

{-# INLINE manyWordByEven #-}
manyWordByEven :: MonadCatch m => SerialT m Int -> m ()
manyWordByEven = IP.parse (PR.many (PR.wordBy (Prelude.even) FL.drain) FL.drain)

{-# INLINE many #-}
many :: MonadCatch m => SerialT m Int -> m Int
many = IP.parse (PR.many (PR.satisfy (> 0)) FL.length)

{-# INLINE manyAlt #-}
manyAlt :: MonadCatch m => SerialT m Int -> m Int
manyAlt xs = do
    x <- IP.parse (AP.many (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE some #-}
some :: MonadCatch m => SerialT m Int -> m Int
some = IP.parse (PR.some (PR.satisfy (> 0)) FL.length)

{-# INLINE someAlt #-}
someAlt :: MonadCatch m => SerialT m Int -> m Int
someAlt xs = do
    x <- IP.parse (AP.some (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE manyTill #-}
manyTill :: MonadCatch m => Int -> SerialT m Int -> m Int
manyTill value =
    IP.parse (PR.manyTill (PR.satisfy (> 0)) (PR.satisfy (== value)) FL.length)

{-# INLINE splitAp #-}
splitAp :: MonadCatch m
    => Int -> SerialT m Int -> m ((), ())
splitAp value =
    IP.parse
        ((,)
            <$> PR.drainWhile (<= (value `div` 2))
            <*> PR.drainWhile (<= value)
        )

{-# INLINE splitApBefore #-}
splitApBefore :: MonadCatch m
    => Int -> SerialT m Int -> m ()
splitApBefore value =
    IP.parse
        (  PR.drainWhile (<= (value `div` 2))
        *> PR.drainWhile (<= value)
        )

{-# INLINE splitApAfter #-}
splitApAfter :: MonadCatch m
    => Int -> SerialT m Int -> m ()
splitApAfter value =
    IP.parse
        (  PR.drainWhile (<= (value `div` 2))
        <* PR.drainWhile (<= value)
        )

{-# INLINE serialWith #-}
serialWith :: MonadCatch m
    => Int -> SerialT m Int -> m ((), ())
serialWith value =
    IP.parse
        (PR.serialWith (,)
            (PR.drainWhile (<= (value `div` 2)))
            (PR.drainWhile (<= value))
        )

{-# INLINE split_ #-}
split_ :: MonadCatch m
    => Int -> SerialT m Int -> m ()
split_ value =
    IP.parse
        (PR.split_
            (PR.drainWhile (<= (value `div` 2)))
            (PR.drainWhile (<= value))
        )

{-# INLINE sliceSepByP #-}
sliceSepByP :: MonadCatch m
    => Int -> SerialT m Int -> m()
sliceSepByP value = IP.parse (PR.sliceSepByP (>= value) (PR.fromFold FL.drain))

{-# INLINE teeAllAny #-}
teeAllAny :: MonadCatch m
    => Int -> SerialT m Int -> m ((), ())
teeAllAny value =
    IP.parse
        (PR.teeWith (,)
            (PR.drainWhile (<= value))
            (PR.drainWhile (<= value))
        )

{-# INLINE teeFstAllAny #-}
teeFstAllAny :: MonadCatch m
    => Int -> SerialT m Int -> m ((), ())
teeFstAllAny value =
    IP.parse
        (PR.teeWithFst (,)
            (PR.drainWhile (<= value))
            (PR.drainWhile (<= value))
        )

{-# INLINE shortestAllAny #-}
shortestAllAny :: MonadCatch m
    => Int -> SerialT m Int -> m ()
shortestAllAny value =
    IP.parse
        (PR.shortest
            (PR.drainWhile (<= value))
            (PR.drainWhile (<= value))
        )

{-# INLINE longestAllAny #-}
longestAllAny :: MonadCatch m
    => Int -> SerialT m Int -> m ()
longestAllAny value =
    IP.parse
        (PR.longest
            (PR.drainWhile (<= value))
            (PR.drainWhile (<= value))
        )

parseManyChunksOfSum :: Int -> Handle -> IO Int
parseManyChunksOfSum n inh =
    S.length
        $ IP.parseMany
              (PR.fromFold $ FL.take n FL.sum)
              (S.unfold Handle.read inh)

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
            Producer.simplify (Source.parseMany parser readSrc)
    S.drain $ S.unfold streamParser src

-------------------------------------------------------------------------------
-- Parsers in which -fspec-constr-recursive=16 is problematic
-------------------------------------------------------------------------------

-- XXX -fspec-constr-recursive=16 makes GHC go beserk when compiling these.
-- We need to fix GHC so that we can have better control over that option or do
-- not have to rely on it.
--
{-# INLINE lookAhead #-}
lookAhead :: MonadCatch m => Int -> SerialT m Int -> m ()
lookAhead value =
    IP.parse (PR.lookAhead (PR.takeWhile (<= value) FL.drain) $> ())

{-# INLINE sequenceA #-}
sequenceA :: MonadCatch m => Int -> SerialT m Int -> m Int
sequenceA value xs = do
    x <- IP.parse (TR.sequenceA (replicate value (PR.satisfy (> 0)))) xs
    return $ length x

{-# INLINE sequenceA_ #-}
sequenceA_ :: MonadCatch m => Int -> SerialT m Int -> m ()
sequenceA_ value =
    IP.parse (F.sequenceA_ $ replicate value (PR.satisfy (> 0)))

{-# INLINE sequence #-}
sequence :: MonadCatch m => Int -> SerialT m Int -> m Int
sequence value xs = do
    x <- IP.parse (TR.sequence (replicate value (PR.satisfy (> 0)))) xs
    return $ length x

{-# INLINE sequence_ #-}
sequence_ :: MonadCatch m => Int -> SerialT m Int -> m ()
sequence_ value =
    IP.parse (F.sequence_ $ replicate value (PR.satisfy (> 0)))

{-# INLINE choice #-}
choice :: MonadCatch m => Int -> SerialT m Int -> m Int
choice value =
    IP.parse (asum (replicate value (PR.satisfy (< 0)))
        AP.<|> PR.satisfy (> 0))

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# INLINE parseMany #-}
parseMany :: MonadCatch m => Int -> SerialT m Int -> m ()
parseMany n =
      S.drain
    . S.map getSum
    . IP.parseMany (PR.fromFold $ FL.take n FL.mconcat)
    . S.map Sum

{-# INLINE parseIterate #-}
parseIterate :: MonadCatch m => Int -> SerialT m Int -> m ()
parseIterate n =
      S.drain
    . S.map getSum
    . IP.parseIterate (PR.fromFold . FL.take n . FL.sconcat) (Sum 0)
    . S.map Sum

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Parser"

o_1_space_serial :: Int -> [Benchmark]
o_1_space_serial value =
    [ benchIOSink value "takeBetween" $ takeBetween value
    , benchIOSink value "takeEQ" $ takeEQ value
    , benchIOSink value "takeWhile" $ takeWhile value
    , benchIOSink value "drainWhile" $ drainWhile value
    , benchIOSink value "sliceBeginWith" $ sliceBeginWith value
    , benchIOSink value "groupBy" $ groupBy
    , benchIOSink value "groupByRolling" $ groupByRolling
    , benchIOSink value "wordBy" $ wordBy value
    , benchIOSink value "splitAp" $ splitAp value
    , benchIOSink value "splitApBefore" $ splitApBefore value
    , benchIOSink value "splitApAfter" $ splitApAfter value
    , benchIOSink value "serialWith" $ serialWith value
    , benchIOSink value "sliceSepByP" $ sliceSepByP value
    , benchIOSink value "many" many
    , benchIOSink value "many (wordBy even)" $ manyWordByEven
    , benchIOSink value "some" some
    , benchIOSink value "manyTill" $ manyTill value
    , benchIOSink value "tee" $ teeAllAny value
    , benchIOSink value "teeFst" $ teeFstAllAny value
    , benchIOSink value "shortest" $ shortestAllAny value
    , benchIOSink value "longest" $ longestAllAny value
    , benchIOSink value "parseMany (take 1)" (parseMany 1)
    , benchIOSink value "parseMany (take all)" (parseMany value)
    , benchIOSink value "parseIterate (take 1)" (parseIterate 1)
    , benchIOSink value "parseIterate (take all)" (parseIterate value)
    ]

o_1_space_filesystem :: BenchEnv -> [Benchmark]
o_1_space_filesystem env =
    [ mkBench ("S.parseMany (FL.take " ++ show (bigSize env) ++ " FL.sum)") env
          $ \inh _ -> noinline parseManyChunksOfSum (bigSize env) inh
    , mkBench "S.parseMany (FL.take 1 FL.sum)" env
          $ \inh _ -> inline parseManyChunksOfSum 1 inh
    ]

o_1_space_serial_unfold :: Int -> [Array.Array Int] -> [Benchmark]
o_1_space_serial_unfold bound arrays =
    [ bench "parseMany/Unfold/1000 arrays/take all"
        $ nfIO $ parseManyUnfoldArrays bound arrays
    , bench "parseMany/Unfold/1000 arrays/take 1"
        $ nfIO $ parseManyUnfoldArrays 1 arrays
    ]

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [
    -- lookahead benchmark holds the entire input till end
      benchIOSink value "lookAhead" $ lookAhead value

    -- accumulates the results in a list
    , benchIOSink value "sequence" $ sequence value
    , benchIOSink value "sequenceA" $ sequenceA value

    -- XXX why should this take O(n) heap, it discards the results?
    , benchIOSink value "sequence_" $ sequence_ value
    , benchIOSink value "sequenceA_" $ sequenceA_ value
    -- non-linear time complexity (parserD)
    , benchIOSink value "split_" $ split_ value
    -- XXX why O(n) heap?
    , benchIOSink value "choice" $ choice value

    -- These show non-linear time complexity.
    -- They accumulate the results in a list.
    , benchIOSink value "manyAlt" manyAlt
    , benchIOSink value "someAlt" someAlt
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    env <- mkHandleBenchEnv
    arrays <- IP.toList $ IP.arraysOf 100 $ sourceUnfoldrM value 0
    value `seq` runMode (mode cfg) cfg benches (allBenchmarks env value arrays)

    where

    allBenchmarks env value arrays =
        [ bgroup (o_1_space_prefix moduleName) (o_1_space_serial value)
        , bgroup
              (o_1_space_prefix moduleName ++ "/filesystem")
              (o_1_space_filesystem env)
        , bgroup (o_1_space_prefix moduleName)
            (o_1_space_serial_unfold value arrays)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        ]
