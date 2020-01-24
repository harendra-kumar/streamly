{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-------------------------------------------------------------------------------
-- Combining control flow manipulating monad transformers (MaybeT, exceptT,
-- ContT) with Streamly
-------------------------------------------------------------------------------
--
-- Streamly streams are non-determinism (nested looping) monads. We can use a
-- control flow monad on top or streamly on top depending on whether we want to
-- superimpose control flow manipulation on top of non-deterministic
-- composition or vice-versa.
--
-- This file provides an example where we enter a sequence of characters "x",
-- and "y" on separate lines, on the command line. When any other sequence is
-- entered the control flow short circuits at the first non-matching char and
-- exits.

import Control.Concurrent (threadDelay)
import Control.Exception (catch, SomeException)
import Control.Monad
import Control.Monad.Catch (MonadThrow, throwM, Exception)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Cont
import Streamly
import qualified Streamly.Prelude as S

-------------------------------------------------------------------------------
-- Using MaybeT below streamly
-------------------------------------------------------------------------------
--
-- When streamly is on top MaybeT would terminate all iterations of
-- non-determinism.
--
getSequenceMaybeBelow
    :: ( IsStream t
       , Monad m
       , MonadTrans t
       , MonadIO (t (MaybeT m))
       )
    => t (MaybeT m) ()
getSequenceMaybeBelow = do
    liftIO $ putStrLn "MaybeT below streamly: Enter one char per line: "

    i <- S.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    r1 <- liftIO getLine
    when (r1 /= "x") $ lift mzero

    r2 <- liftIO getLine
    when (r2 /= "y") $ lift mzero

mainMaybeBelow :: IO ()
mainMaybeBelow = do
    r <- runMaybeT (S.drain getSequenceMaybeBelow)
    case r of
        Just _ -> putStrLn "Bingo"
        Nothing -> putStrLn "Wrong"

-------------------------------------------------------------------------------
-- Using MaybeT above streamly
-------------------------------------------------------------------------------
--
-- When MaybeT is on top a Nothing would terminate only the current iteration
-- of non-determinism below.
--
-- Note that this is redundant configuration as the same behavior can be
-- achieved with just streamly, using mzero.
--
getSequenceMaybeAbove :: (IsStream t, MonadIO (t m)) => MaybeT (t m) ()
getSequenceMaybeAbove = do
    liftIO $ putStrLn "MaybeT above streamly: Enter one char per line: "

    i <- lift $ S.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    r1 <- liftIO getLine
    when (r1 /= "x") mzero

    r2 <- liftIO getLine
    when (r2 /= "y") mzero

mainMaybeAbove :: (IsStream t, MonadIO (t m)) => MaybeT (t m) ()
mainMaybeAbove = do
    getSequenceMaybeAbove
    liftIO $ putStrLn "Bingo"

-------------------------------------------------------------------------------
-- Using ExceptT below streamly
-------------------------------------------------------------------------------
--
-- XXX need to have a specialized liftCatch to lift catchE
--
-- Note that throwE would terminate all iterations of non-determinism
-- altogether.
getSequenceEitherBelow
    :: ( IsStream t
       , MonadTrans t
       , Monad m
       , MonadIO (t (ExceptT String m))
       )
    => t (ExceptT String m) ()
getSequenceEitherBelow = do
    liftIO $ putStrLn "ExceptT below streamly: Enter one char per line: "

    i <- S.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    r1 <- liftIO getLine
    when (r1 /= "x") $ lift $ throwE $ "Expecting x got: " <> r1

    r2 <- liftIO getLine
    when (r2 /= "y") $ lift $ throwE $ "Expecting y got: " <> r2

mainEitherBelow :: IO ()
mainEitherBelow = do
    -- XXX Cannot lift catchE
    r <- runExceptT (S.drain getSequenceEitherBelow)
    case r of
        Right _ -> liftIO $ putStrLn "Bingo"
        Left s  -> liftIO $ putStrLn s

-------------------------------------------------------------------------------
-- Using ExceptT below concurrent streamly
-------------------------------------------------------------------------------
--
-- XXX does not work correctly yet
--
getSequenceEitherAsyncBelow
    :: ( IsStream t
       , MonadTrans t
       , MonadIO m
       , MonadIO (t (ExceptT String m))
       , Semigroup (t (ExceptT String m) Integer)
       )
    => t (ExceptT String m) ()
getSequenceEitherAsyncBelow = do
    liftIO $ putStrLn "ExceptT below concurrent streamly: "

    i <- (liftIO (threadDelay 1000)
            >> lift (throwE "First task")
            >> return 1)
            <> (lift (throwE "Second task") >> return 2)
            <> S.yield (3 :: Integer)
    liftIO $ putStrLn $ "iteration = " <> show i

mainEitherAsyncBelow :: IO ()
mainEitherAsyncBelow = do
    r <- runExceptT (S.drain $ asyncly getSequenceEitherAsyncBelow)
    case r of
        Right _ -> liftIO $ putStrLn "Bingo"
        Left s  -> liftIO $ putStrLn s

-------------------------------------------------------------------------------
-- Using ExceptT above streamly
-------------------------------------------------------------------------------
--
-- When ExceptT is on top, we can lift the non-determinism of stream from
-- below.
--
-- Note that throwE would terminate/break only current iteration of
-- non-determinism and not all of them altogether.
--
-- Here we can use catchE directly but will have to use monad-control to lift
-- stream operations with stream arguments.
getSequenceEitherAbove :: (IsStream t, MonadIO (t m))
    => ExceptT String (t m) ()
getSequenceEitherAbove = do
    liftIO $ putStrLn "ExceptT above streamly: Enter one char per line: "

    i <- lift $ S.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    r1 <- liftIO getLine
    when (r1 /= "x") $ throwE $ "Expecting x got: " <> r1

    r2 <- liftIO getLine
    when (r2 /= "y") $ throwE $ "Expecting y got: " <> r2

mainEitherAbove :: (IsStream t, MonadIO (t m)) => ExceptT String (t m) ()
mainEitherAbove =
    catchE (getSequenceEitherAbove >> liftIO (putStrLn "Bingo"))
           (liftIO . putStrLn)

-------------------------------------------------------------------------------
-- Using MonadThrow to throw exceptions in streamly
-------------------------------------------------------------------------------
--
newtype Unexpected = Unexpected String deriving Show

instance Exception Unexpected

-- Note that unlike when ExceptT is used on top, MonadThrow terminates all
-- iterations of non-determinism rather then just the current iteration.
--
getSequenceMonadThrow :: (IsStream t, MonadIO (t m), MonadThrow (t m))
    => t m ()
getSequenceMonadThrow = do
    liftIO $ putStrLn "MonadThrow in streamly: Enter one char per line: "

    i <- S.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    r1 <- liftIO getLine
    when (r1 /= "x") $ throwM $ Unexpected $ "Expecting x got: " <> r1

    r2 <- liftIO getLine
    when (r2 /= "y") $ throwM $ Unexpected $ "Expecting y got: " <> r2

mainMonadThrow :: IO ()
mainMonadThrow =
    catch (S.drain getSequenceMonadThrow >> liftIO (putStrLn "Bingo"))
          (\(e :: SomeException) -> liftIO $ print e)

-------------------------------------------------------------------------------
-- Using ContT below streamly
-------------------------------------------------------------------------------
--
-- CallCC is the goto/setjmp/longjmp equivalent
-- Allows us to manipulate the control flow in arbitrary ways
--
-- XXX need to have a specialized liftCallCC to actually lift callCC
--
getSequenceContBelow
    :: (IsStream t, MonadTrans t, MonadIO m, MonadIO (t (ContT r m)))
    => t (ContT r m) (Either String ())
getSequenceContBelow = do
    liftIO $ putStrLn "ContT below streamly: Enter one char per line: "

    i <- S.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    r <- lift $ callCC $ \exit -> do
        r1 <- liftIO getLine
        _ <- if r1 /= "x"
             then exit $ Left $ "Expecting x got: " <> r1
             else return $ Right ()

        r2 <- liftIO getLine
        if r2 /= "y"
        then exit $ Left $ "Expecting y got: " <> r2
        else return $ Right ()
    liftIO $ putStrLn $ "done iteration = " <> show i
    return r

mainContBelow
    :: (IsStream t, MonadIO m, MonadTrans t, MonadIO (t (ContT r m)))
    => t (ContT r m) ()
mainContBelow = do
    r <- getSequenceContBelow
    case r of
        Right _ -> liftIO $ putStrLn "Bingo"
        Left s  -> liftIO $ putStrLn s

-------------------------------------------------------------------------------
-- Using ContT above streamly
-------------------------------------------------------------------------------
--
getSequenceContAbove :: (IsStream t, MonadIO (t m))
    => ContT r (t m) (Either String ())
getSequenceContAbove = do
    liftIO $ putStrLn "ContT above streamly: Enter one char per line: "

    i <- lift $ S.fromFoldable [1..2 :: Int]
    liftIO $ putStrLn $ "iteration = " <> show i

    callCC $ \exit -> do
        r1 <- liftIO getLine
        _ <- if r1 /= "x"
             then exit $ Left $ "Expecting x got: " <> r1
             else return $ Right ()

        r2 <- liftIO getLine
        if r2 /= "y"
        then exit $ Left $ "Expecting y got: " <> r2
        else return $ Right ()

mainContAbove :: (IsStream t, MonadIO (t m)) => ContT r (t m) ()
mainContAbove = do
    r <- getSequenceContAbove
    case r of
        Right _ -> liftIO $ putStrLn "Bingo"
        Left s  -> liftIO $ putStrLn s

-------------------------------------------------------------------------------
-- Combining control flow manipulating monad transformers (MaybeT, exceptT,
-- ContT) with Streamly
-------------------------------------------------------------------------------

main :: IO ()
main = do
    mainMaybeBelow
    S.drain $ runMaybeT mainMaybeAbove
    runContT (S.drain mainContBelow) return
    S.drain (runContT mainContAbove return)
    mainEitherBelow
    S.drain (runExceptT mainEitherAbove)
    mainMonadThrow
    mainEitherAsyncBelow
