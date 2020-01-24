-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Gauge
import Streamly
import Streamly.Prelude as S
import System.Random (randomRIO)

-- Note that we should also compare the cpuTime especially when threaded
-- runtime is used with this benchmark because thread scheduling is not
-- predictable and can add non-deterministic delay to the total time measured.
--
-- Also, the worker dispatch depends on the worker dispatch latency which is
-- set to fixed 200 us. We need to keep that in mind when designing tests.

value :: Int
value = 1000

{-# INLINE source #-}
source :: IsStream t => (Int, Int) -> t IO Int
source range = S.replicateM value $ do
    r <- randomRIO range
    when (r /= 0) $ liftIO $ threadDelay r
    return r

{-# INLINE run #-}
run :: IsStream t => (Int, Int) -> (Int, Int) -> (t IO Int -> SerialT IO Int) -> IO ()
run srange crange t = S.drain $ do
    n <- t $ source srange
    d <- liftIO (randomRIO crange)
    when (d /= 0) $ liftIO $ threadDelay d
    return n

low, medium, high :: Int
low = 10
medium = 20
high = 30

{-# INLINE noDelay #-}
noDelay :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
noDelay = run (0,0) (0,0)

{-# INLINE alwaysConstSlowSerial #-}
alwaysConstSlowSerial :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
alwaysConstSlowSerial = run (0,0) (medium,medium)

{-# INLINE alwaysConstSlow #-}
alwaysConstSlow :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
alwaysConstSlow = run (low,low) (medium,medium)

{-# INLINE alwaysConstFast #-}
alwaysConstFast :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
alwaysConstFast = run (high,high) (medium,medium)

{-# INLINE alwaysVarSlow #-}
alwaysVarSlow :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
alwaysVarSlow = run (low,low) (low,high)

{-# INLINE alwaysVarFast #-}
alwaysVarFast :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
alwaysVarFast = run (high,high) (low,high)

-- XXX add variable producer tests as well

{-# INLINE runVarSometimesFast #-}
runVarSometimesFast :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
runVarSometimesFast = run (medium,medium) (low,high)

{-# INLINE randomVar #-}
randomVar :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
randomVar = run (low,high) (low,high)

main :: IO ()
main =
  defaultMain
    [
      bgroup "serialConstantSlowConsumer"
      [ bench "serially"    $ nfIO $ alwaysConstSlowSerial serially
      , bench "wSerially"   $ nfIO $ alwaysConstSlowSerial wSerially
      ]
    , bgroup "default"
      [ bench "serially"   $ nfIO $ noDelay serially
      , bench "wSerially"  $ nfIO $ noDelay wSerially
      , bench "aheadly"    $ nfIO $ noDelay aheadly
      , bench "asyncly"    $ nfIO $ noDelay asyncly
      , bench "wAsyncly"   $ nfIO $ noDelay wAsyncly
      , bench "parallely"  $ nfIO $ noDelay parallely
      ]
    , bgroup "constantSlowConsumer"
      [ bench "aheadly"    $ nfIO $ alwaysConstSlow aheadly
      , bench "asyncly"    $ nfIO $ alwaysConstSlow asyncly
      , bench "wAsyncly"   $ nfIO $ alwaysConstSlow wAsyncly
      , bench "parallely"  $ nfIO $ alwaysConstSlow parallely
      ]
   ,  bgroup "constantFastConsumer"
      [ bench "aheadly"    $ nfIO $ alwaysConstFast aheadly
      , bench "asyncly"    $ nfIO $ alwaysConstFast asyncly
      , bench "wAsyncly"   $ nfIO $ alwaysConstFast wAsyncly
      , bench "parallely"  $ nfIO $ alwaysConstFast parallely
      ]
   ,  bgroup "variableSlowConsumer"
      [ bench "aheadly"    $ nfIO $ alwaysVarSlow aheadly
      , bench "asyncly"    $ nfIO $ alwaysVarSlow asyncly
      , bench "wAsyncly"   $ nfIO $ alwaysVarSlow wAsyncly
      , bench "parallely"  $ nfIO $ alwaysVarSlow parallely
      ]
   ,  bgroup "variableFastConsumer"
      [ bench "aheadly"    $ nfIO $ alwaysVarFast aheadly
      , bench "asyncly"    $ nfIO $ alwaysVarFast asyncly
      , bench "wAsyncly"   $ nfIO $ alwaysVarFast wAsyncly
      , bench "parallely"  $ nfIO $ alwaysVarFast parallely
      ]
   ,  bgroup "variableSometimesFastConsumer"
      [ bench "aheadly"    $ nfIO $ runVarSometimesFast aheadly
      , bench "asyncly"    $ nfIO $ runVarSometimesFast asyncly
      , bench "wAsyncly"   $ nfIO $ runVarSometimesFast wAsyncly
      , bench "parallely"  $ nfIO $ runVarSometimesFast parallely
      ]
   ,  bgroup "variableFullOverlap"
      [ bench "aheadly"    $ nfIO $ randomVar aheadly
      , bench "asyncly"    $ nfIO $ randomVar asyncly
      , bench "wAsyncly"   $ nfIO $ randomVar wAsyncly
      , bench "parallely"  $ nfIO $ randomVar parallely
      ]
   ]
