-- |
-- Module      : Streamly.Test.Prelude
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude (main) where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Exception (Exception, try)
import Control.Monad.Catch (throwM)
import Control.Monad.Error.Class (throwError, MonadError)
import Control.Monad.Trans.Except (runExceptT, ExceptT)
import Data.List (sort)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.Random (randomIO)
import Test.Hspec as H

import Streamly.Prelude (SerialT, IsStream)

import qualified Streamly.Prelude as S

toListSerial :: SerialT IO a -> IO [a]
toListSerial = S.toList . S.serially

-- XXX need to test that we have promptly cleaned up everything after the error
-- XXX We can also check the output that we are expected to get before the
-- error occurs.

newtype ExampleException = ExampleException String deriving (Eq, Show)

instance Exception ExampleException

simpleMonadError :: Spec
simpleMonadError = do
{-
    it "simple runExceptT" $ do
        (runExceptT $ S.drain $ return ())
        `shouldReturn` (Right () :: Either String ())
    it "simple runExceptT with error" $ do
        (runExceptT $ S.drain $ throwError "E") `shouldReturn` Left "E"
        -}
    it "simple try" $
        try (S.drain $ return ())
        `shouldReturn` (Right () :: Either ExampleException ())
    it "simple try with throw error" $
        try (S.drain $ throwM $ ExampleException "E")
        `shouldReturn` (Left (ExampleException "E") :: Either ExampleException ())

_composeWithMonadError
    :: ( IsStream t
       , Semigroup (t (ExceptT String IO) Int)
       , MonadError String (t (ExceptT String IO))
       )
    => (t (ExceptT String IO) Int -> SerialT (ExceptT String IO) Int) -> Spec
_composeWithMonadError t = do
    let tl = S.toList . t
    it "Compose throwError, nil" $
        runExceptT (tl (throwError "E" <> S.nil)) `shouldReturn` Left "E"
    it "Compose nil, error" $
        runExceptT (tl (S.nil <> throwError "E")) `shouldReturn` Left "E"

mixedOps :: Spec
mixedOps =
    it "Compose many ops" $
        (sort <$> toListSerial composeMixed)
            `shouldReturn` ([8,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,11,11
                            ,11,11,11,11,11,11,11,11,12,12,12,12,12,13
                            ] :: [Int])
    where

    composeMixed :: SerialT IO Int
    composeMixed = do
        S.valueM $ return ()
        S.valueM $ putStr ""
        let x = 1
        let y = 2
        z <- do
                x1 <- S.wAsyncly $ return 1 <> return 2
                S.valueM $ return ()
                S.valueM $ putStr ""
                y1 <- S.asyncly $ return 1 <> return 2
                z1 <- do
                    x11 <- return 1 <> return 2
                    y11 <- S.asyncly $ return 1 <> return 2
                    z11 <- S.wSerially $ return 1 <> return 2
                    S.valueM $ return ()
                    S.valueM $ putStr ""
                    return (x11 + y11 + z11)
                return (x1 + y1 + z1)
        return (x + y + z)

mixedOpsAheadly :: Spec
mixedOpsAheadly =
    it "Compose many ops" $
        (sort <$> toListSerial composeMixed)
            `shouldReturn` ([8,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,11,11
                            ,11,11,11,11,11,11,11,11,12,12,12,12,12,13
                            ] :: [Int])
    where

    composeMixed :: SerialT IO Int
    composeMixed = do
        S.valueM $ return ()
        S.valueM $ putStr ""
        let x = 1
        let y = 2
        z <- do
                x1 <- S.wAsyncly $ return 1 <> return 2
                S.valueM $ return ()
                S.valueM $ putStr ""
                y1 <- S.aheadly $ return 1 <> return 2
                z1 <- do
                    x11 <- return 1 <> return 2
                    y11 <- S.aheadly $ return 1 <> return 2
                    z11 <- S.parallely $ return 1 <> return 2
                    S.valueM $ return ()
                    S.valueM $ putStr ""
                    return (x11 + y11 + z11)
                return (x1 + y1 + z1)
        return (x + y + z)

-- XXX Merge both the loops.
nestedLoops :: IO ()
nestedLoops = S.drain $ do
    S.valueM $ hSetBuffering stdout LineBuffering
    x <- loop "A " 2
    y <- loop "B " 2
    S.valueM $ myThreadId >>= putStr . show
             >> putStr " "
             >> print (x, y)

    where

    -- we can just use
    -- parallely $ mconcat $ replicate n $ valueM (...)
    loop :: String -> Int -> SerialT IO String
    loop name n = do
        rnd <- S.valueM (randomIO :: IO Int)
        let result = name <> show rnd
            repeatIt = if n > 1 then loop name (n - 1) else S.nil
         in return result `S.wAsync` repeatIt

parallelLoops :: IO ()
parallelLoops = do
    hSetBuffering stdout LineBuffering
    S.drain $ do
        x <- S.take 10 $ loop "A" `S.parallel` loop "B"
        S.valueM $ myThreadId >>= putStr . show
               >> putStr " got "
               >> print x

    where

    -- we can just use
    -- parallely $ cycle1 $ valueM (...)
    loop :: String -> SerialT IO (String, Int)
    loop name = do
        S.valueM $ threadDelay 1000000
        rnd <- S.valueM (randomIO :: IO Int)
        S.valueM $ myThreadId >>= putStr . show
               >> putStr " yielding "
               >> print rnd
        return (name, rnd) `S.parallel` loop name


main :: IO ()
main = hspec $ H.parallel $ do
    describe "Miscellaneous combined examples" mixedOps
    describe "Miscellaneous combined examples aheadly" mixedOpsAheadly
    describe "Simple MonadError and MonadThrow" simpleMonadError

    it "Nested loops" nestedLoops
    it "Parallel loops" parallelLoops
    {-
    describe "Composed MonadError serially" $ composeWithMonadError serially
    describe "Composed MonadError wSerially" $ composeWithMonadError wSerially
    describe "Composed MonadError asyncly" $ composeWithMonadError asyncly
    describe "Composed MonadError wAsyncly" $ composeWithMonadError wAsyncly
    -}
