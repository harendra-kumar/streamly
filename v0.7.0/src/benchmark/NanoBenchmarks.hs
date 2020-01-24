-------------------------------------------------------------------------------
-- Investigate specific benchmarks more closely in isolation, possibly looking
-- at GHC generated code for optimizing specific problematic cases.
-------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Streamly (SerialT)
import Streamly.Internal.Data.SVar (MonadAsync)

import qualified Streamly.Memory.Array as A
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Prelude as Internal
import qualified Streamly.Prelude as S
import qualified Streamly.Streams.StreamK as K

import Data.Char (ord)
import Gauge
import System.IO (hSeek, SeekMode(..), openFile, IOMode(..))
import System.Random

maxValue :: Int
maxValue = 100000

-- type Stream = K.Stream
type Stream = SerialT

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: MonadAsync m => Stream m Int
sourceUnfoldrM = S.unfoldrM step 0
    where
    step cnt =
        if cnt > maxValue
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldrMN #-}
sourceUnfoldrMN :: MonadAsync m => Int -> Stream m Int
sourceUnfoldrMN n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: Monad m => Int -> Stream m Int
sourceUnfoldr n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + maxValue
        then Nothing
        else Just (cnt, cnt + 1)

-------------------------------------------------------------------------------
-- take-drop composition
-------------------------------------------------------------------------------

takeAllDropOne :: Monad m => Stream m Int -> Stream m Int
takeAllDropOne = S.drop 1 . S.take maxValue

-- Requires -fspec-constr-recursive=5 for better fused code
-- The number depends on how many times we compose it

{-# INLINE takeDrop #-}
takeDrop :: Monad m => Stream m Int -> m ()
takeDrop = S.drain .
    takeAllDropOne . takeAllDropOne . takeAllDropOne . takeAllDropOne

-------------------------------------------------------------------------------
-- dropWhileFalse composition
-------------------------------------------------------------------------------

dropWhileFalse :: Monad m => Stream m Int -> Stream m Int
dropWhileFalse = S.dropWhile (> maxValue)

-- Requires -fspec-constr-recursive=5 for better fused code
-- The number depends on how many times we compose it

{-# INLINE dropWhileFalseX4 #-}
dropWhileFalseX4 :: Monad m => Stream m Int -> m ()
dropWhileFalseX4 = S.drain
    . dropWhileFalse . dropWhileFalse . dropWhileFalse .  dropWhileFalse

-------------------------------------------------------------------------------
-- iteration
-------------------------------------------------------------------------------

{-# INLINE iterateSource #-}
iterateSource
    :: MonadAsync m
    => (Stream m Int -> Stream m Int) -> Int -> Int -> Stream m Int
iterateSource g i n = f i (sourceUnfoldrMN n)
    where
        f (0 :: Int) m = g m
        f x m = g (f (x - 1) m)

-- Keep only the benchmark that is to be investiagted and comment out the rest.
-- We keep all of them enabled by default for testing the build.
main :: IO ()
main = do
    defaultMain [bench "unfoldr" $ nfIO $
        randomRIO (1,1) >>= \n -> S.drain (sourceUnfoldr n)]
    defaultMain [bench "take-drop" $ nfIO $ takeDrop sourceUnfoldrM]
    defaultMain [bench "dropWhileFalseX4" $
        nfIO $ dropWhileFalseX4 sourceUnfoldrM]
    defaultMain [bench "iterate-mapM" $
        nfIO $ S.drain $ iterateSource (S.mapM return) 100000 10]

    inText <- openFile "benchmark/text-processing/gutenberg-500.txt" ReadMode
    defaultMain [mkBenchText "splitOn abc...xyz" inText $ do
                (S.length $ Internal.splitOnSeq (A.fromList $ map (fromIntegral . ord)
                    "abcdefghijklmnopqrstuvwxyz") FL.drain
                        $ IFH.toStream inText) >>= print
                ]
    where

    mkBenchText name h action =
        bench name $ perRunEnv (hSeek h AbsoluteSeek 0) (\_ -> action)
