import Control.Concurrent (myThreadId)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.Random (randomIO)
import Streamly
import Streamly.Prelude (drain, nil, yieldM)

main :: IO ()
main = drain $ do
    yieldM $ hSetBuffering stdout LineBuffering
    x <- loop "A " 2
    y <- loop "B " 2
    yieldM $ myThreadId >>= putStr . show
             >> putStr " "
             >> print (x, y)

    where

    -- we can just use
    -- parallely $ mconcat $ replicate n $ yieldM (...)
    loop :: String -> Int -> SerialT IO String
    loop name n = do
        rnd <- yieldM (randomIO :: IO Int)
        let result = name <> show rnd
            repeatIt = if n > 1 then loop name (n - 1) else nil
         in return result `wAsync` repeatIt
