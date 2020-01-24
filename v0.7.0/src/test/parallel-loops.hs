import Control.Concurrent (myThreadId, threadDelay)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.Random (randomIO)
import Streamly
import qualified Streamly.Prelude as S

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    S.drain $ do
        x <- S.take 10 $ loop "A" `parallel` loop "B"
        S.yieldM $ myThreadId >>= putStr . show
               >> putStr " got "
               >> print x

    where

    -- we can just use
    -- parallely $ cycle1 $ yieldM (...)
    loop :: String -> Serial (String, Int)
    loop name = do
        S.yieldM $ threadDelay 1000000
        rnd <- S.yieldM (randomIO :: IO Int)
        S.yieldM $ myThreadId >>= putStr . show
               >> putStr " yielding "
               >> print rnd
        return (name, rnd) `parallel` loop name
