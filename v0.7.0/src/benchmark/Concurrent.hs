{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Control.Concurrent
import Control.Monad (when)
-- import Data.IORef

import Gauge
import Streamly
import qualified Streamly.Prelude as S

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

-- Single work item yielded per thread
{-# INLINE append #-}
append :: IsStream t
    => Int -> Int -> Int -> (t IO Int -> SerialT IO Int) -> IO ()
append buflen tcount d t =
    let work = (\i -> when (d /= 0) (threadDelay d) >> return i)
    in S.drain
        $ t
        $ maxBuffer buflen
        $ maxThreads (-1)
        $ S.fromFoldableM $ map work [1..tcount]

-- Big stream of items yielded per thread
{-# INLINE concated #-}
concated :: Int -> Int -> Int -> Int -> (forall a. SerialT IO a -> SerialT IO a -> SerialT IO a) -> IO ()
concated buflen threads d elems t =
    let work = (\i -> (S.replicateM i ((when (d /= 0) (threadDelay d)) >> return i)))
    in S.drain
        $ adapt
        $ maxThreads (-1)
        $ maxBuffer buflen
        $ S.concatMapWith t work
        $ S.replicate threads elems

appendGroup :: Int -> Int -> Int -> [Benchmark]
appendGroup buflen threads delay =
    [ -- bench "serial"   $ nfIO $ append buflen threads delay serially
      bench "ahead"    $ nfIO $ append buflen threads delay aheadly
    , bench "async"    $ nfIO $ append buflen threads delay asyncly
    , bench "wAsync"   $ nfIO $ append buflen threads delay wAsyncly
    , bench "parallel" $ nfIO $ append buflen threads delay parallely
    ]

concatGroup :: Int -> Int -> Int -> Int -> [Benchmark]
concatGroup buflen threads delay n =
    [ bench "serial"   $ nfIO $ concated buflen threads delay n serial
    , bench "ahead"    $ nfIO $ concated buflen threads delay n ahead
    , bench "async"    $ nfIO $ concated buflen threads delay n async
    , bench "wAsync"   $ nfIO $ concated buflen threads delay n wAsync
    , bench "parallel" $ nfIO $ concated buflen threads delay n parallel
    ]

main :: IO ()
main = do
  defaultMainWith (defaultConfig
    { timeLimit = Just 0
    , minSamples = Just 1
    , minDuration = 0
    , includeFirstIter = True
    , quickMode = True
    })

    [ -- bgroup "append/buf-1-threads-10k-0sec"  (appendGroup 1 10000 0)
    -- , bgroup "append/buf-100-threads-100k-0sec"  (appendGroup 100 100000 0)
      bgroup "append/buf-10k-threads-10k-5sec"  (appendGroup 10000 10000 5000000)
    --  bgroup "concat/buf-1-threads-100k-count-1" (concatGroup 1 100000 0 1)
    --  bgroup "concat/buf-1-threads-1-count-10m" (concatGroup 1 1 0 10000000)
     , bgroup "concat/buf-100-threads-100-count-500k"  (concatGroup 100 100 0 500000)
    {-
    , bgroup "forkIO-5000ms-10k" $
    let delay = threadDelay 5000000
        count = 10000 :: Int
        list = [1..count]
        work i = delay >> return i
    in
    [ bench "discard" $ nfIO $ do
        mapM_ (\i -> forkIO $ work i >> return ()) list
        threadDelay 6000000
    , bench "collect" $ nfIO $ do
        ref <- newIORef []
        mapM_ (\i -> forkIO $ work i >>=
               \j -> atomicModifyIORef ref $ \xs -> (j : xs, ())) list
        threadDelay 6000000
    ]
    -}
   ]
