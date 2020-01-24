import Streamly
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import Streamly.Prelude (nil, yieldM, drain)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    putStrLn "\nloopTail:\n"
    drain $ do
        x <- loopTail 0
        yieldM $ print (x :: Int)

    putStrLn "\nloopHead:\n"
    drain $ do
        x <- loopHead 0
        yieldM $ print (x :: Int)

    putStrLn "\nloopTailA:\n"
    drain $ do
        x <- loopTailA 0
        yieldM $ print (x :: Int)

    putStrLn "\nloopHeadA:\n"
    drain $ do
        x <- loopHeadA 0
        yieldM $ print (x :: Int)

    putStrLn "\nwSerial:\n"
    drain $ do
        x <- (return 0 <> return 1) `wSerial` (return 100 <> return 101)
        yieldM $ print (x :: Int)

    putStrLn "\nParallel interleave:\n"
    drain $ do
        x <- (return 0 <> return 1) `wAsync` (return 100 <> return 101)
        yieldM $ print (x :: Int)

    where

-------------------------------------------------------------------------------
-- Serial (single-threaded) stream generator loops
-------------------------------------------------------------------------------

    -- Generates a value and then loops. Can be used to generate an infinite
    -- stream. Interleaves the generator and the consumer.
    loopTail :: Int -> Serial Int
    loopTail x = do
        yieldM $ putStrLn "LoopTail..."
        return x <> (if x < 3 then loopTail (x + 1) else nil)

    -- Loops and then generates a value. The consumer can run only after the
    -- loop has finished.  An infinite generator will not let the consumer run
    -- at all.
    loopHead :: Int -> Serial Int
    loopHead x = do
        yieldM $ putStrLn "LoopHead..."
        (if x < 3 then loopHead (x + 1) else nil) <> return x

-------------------------------------------------------------------------------
-- Concurrent (multi-threaded) adaptive demand-based stream generator loops
-------------------------------------------------------------------------------

    loopTailA :: Int -> Serial Int
    loopTailA x = do
        yieldM $ putStrLn "LoopTailA..."
        return x `async` (if x < 3 then loopTailA (x + 1) else nil)

    loopHeadA :: Int -> Serial Int
    loopHeadA x = do
        yieldM $ putStrLn "LoopHeadA..."
        (if x < 3 then loopHeadA (x + 1) else nil) `async` return x

-------------------------------------------------------------------------------
-- Parallel (fairly scheduled, multi-threaded) stream generator loops
-------------------------------------------------------------------------------
