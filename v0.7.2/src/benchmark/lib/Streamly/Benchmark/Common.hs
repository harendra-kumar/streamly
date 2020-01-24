{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

-- |
-- Module      : Streamly.Benchmark.Common
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

module Streamly.Benchmark.Common
    ( parseCLIOpts

    , benchIOSink1
    , benchPure
    , benchPureSink1
    , benchFold

    , benchIOSrc1
    , benchPureSrc

    , mkString
    , mkList
    , mkListString

    , defaultStreamSize
    , limitStreamSize
    )
where

import Control.DeepSeq (NFData(..))
import Control.Exception (evaluate)
import Control.Monad (when)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (scanl')
import Data.Maybe (catMaybes)
import System.Console.GetOpt
       (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt')
import System.Environment (getArgs, lookupEnv, setEnv)
import Text.Read (readMaybe)
import System.Random (randomRIO)

import qualified Streamly.Prelude as S

import Streamly
import Gauge

-------------------------------------------------------------------------------
-- Benchmarking utilities
-------------------------------------------------------------------------------

#if !MIN_VERSION_deepseq(1,4,3)
instance NFData Ordering where rnf = (`seq` ())
#endif

-- XXX once we convert all the functions to use this we can rename this to
-- benchIOSink
{-# INLINE benchIOSink1 #-}
benchIOSink1 :: NFData b => String -> (Int -> IO b) -> Benchmark
benchIOSink1 name f = bench name $ nfIO $ randomRIO (1,1) >>= f

{-# INLINE benchIOSrc1 #-}
benchIOSrc1 :: String -> (Int -> IO ()) -> Benchmark
benchIOSrc1 name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.
{-# INLINE benchFold #-}
benchFold :: NFData b
    => String -> (t IO Int -> IO b) -> (Int -> t IO Int) -> Benchmark
benchFold name f src = bench name $ nfIO $ randomRIO (1,1) >>= f . src

{-# INLINE benchPure #-}
benchPure :: NFData b => String -> (Int -> a) -> (a -> b) -> Benchmark
benchPure name src f = bench name $ nfIO $ randomRIO (1,1) >>= return . f . src

-- XXX once we convert all the functions to use this we can rename this to
-- benchPureSink
{-# INLINE benchPureSink1 #-}
benchPureSink1 :: NFData b => String -> (Int -> Identity b) -> Benchmark
benchPureSink1 name f =
    bench name $ nfIO $ randomRIO (1,1) >>= return . runIdentity . f

{-# INLINE benchPureSrc #-}
benchPureSrc :: String -> (Int -> SerialT Identity a) -> Benchmark
benchPureSrc name src = benchPure name src (runIdentity . S.drain)

-------------------------------------------------------------------------------
-- String/List generation for read instances
-------------------------------------------------------------------------------

{-# INLINABLE mkString #-}
mkString :: Int -> String
mkString value = "fromList [1" ++ concat (replicate value ",1") ++ "]"

{-# INLINABLE mkListString #-}
mkListString :: Int -> String
mkListString value = "[1" ++ concat (replicate value ",1") ++ "]"

{-# INLINABLE mkList #-}
mkList :: Int -> [Int]
mkList value = [1..value]

-------------------------------------------------------------------------------
-- Stream size
-------------------------------------------------------------------------------

defaultStreamSize :: Int
defaultStreamSize = 100000

limitStreamSize :: Int -> IO Int
limitStreamSize value = do
    let val = min value defaultStreamSize
    when (val /= value) $
        putStrLn $ "Limiting stream size to "
                   ++ show defaultStreamSize
                   ++ " for non O(1) space operations"
    return val

-------------------------------------------------------------------------------
-- Parse custom CLI options
-------------------------------------------------------------------------------

data BenchOpts = StreamSize Int deriving Show

getStreamSize :: String -> Int
getStreamSize size =
    case (readMaybe size :: Maybe Int) of
        Just x -> x
        Nothing -> error "Stream size must be numeric"

options :: [OptDescr BenchOpts]
options =
    [
      Option [] ["stream-size"] (ReqArg getSize "COUNT") "Stream element count"
    ]

    where

    getSize = StreamSize . getStreamSize

deleteOptArgs
    :: (Maybe String, Maybe String) -- (prev, yielded)
    -> String
    -> (Maybe String, Maybe String)
deleteOptArgs (Nothing, _) opt =
    if opt == "--stream-size"
    then (Just opt, Nothing)
    else (Just opt, Just opt)

deleteOptArgs (Just prev, _) opt =
    if opt == "--stream-size" || prev == "--stream-size"
    then (Just opt, Nothing)
    else (Just opt, Just opt)

parseCLIOpts :: Int -> IO (Int, Config, [String])
parseCLIOpts defStreamSize = do
    args <- getArgs

    -- Parse custom options
    let (opts, _, _, errs) = getOpt' Permute options args
    when (not $ null errs) $ error $ concat errs
    (streamSize, args') <-
        case opts of
            StreamSize x : _ -> do
                -- When using the gauge "--measure-with" option we need to make
                -- sure that we pass the stream size to child process forked by
                -- gauge. So we use this env var for that purpose.
                setEnv "STREAM_SIZE" (show x)
                -- Hack! remove the option and its argument from args
                -- getOpt should have a way to return the unconsumed args in
                -- correct order.
                newArgs <-
                          evaluate
                        $ catMaybes
                        $ map snd
                        $ scanl' deleteOptArgs (Nothing, Nothing) args
                return (x, newArgs)
            _ -> do
                r <- lookupEnv "STREAM_SIZE"
                case r of
                    Just x -> do
                        s <- evaluate $ getStreamSize x
                        return (s, args)
                    Nothing -> return (defStreamSize, args)

    -- Parse gauge options
    let config = defaultConfig
                { timeLimit = Just 1
                , minDuration = 0
                , includeFirstIter = streamSize > defStreamSize
                }
    let (cfg, benches) = parseWith config args'
    streamSize `seq` return (streamSize, cfg, benches)
