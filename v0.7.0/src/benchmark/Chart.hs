{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (handle, catch, SomeException, ErrorCall(..))
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.Function (on, (&))
import Data.List
import Data.List.Split
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)

import BenchShow

------------------------------------------------------------------------------
-- Command line parsing
------------------------------------------------------------------------------

data BenchType
    = Linear
    | LinearAsync
    | LinearRate
    | Nested
    | Base
    | FileIO
    | Array
    | Concurrent
    deriving Show

data Options = Options
    { genGraphs :: Bool
    , groupDiff :: Bool
    , benchType :: BenchType
    } deriving Show

defaultOptions = Options False False Linear

setGenGraphs val = do
    (args, opts) <- get
    put (args, opts { genGraphs = val })

setGroupDiff val = do
    (args, opts) <- get
    put (args, opts { groupDiff = val })

setBenchType val = do
    (args, opts) <- get
    put (args, opts { benchType = val })

-- Like the shell "shift" to shift the command line arguments
shift :: StateT ([String], Options) (MaybeT IO) (Maybe String)
shift = do
    s <- get
    case s of
        ([], _) -> return Nothing
        (x : xs, opts) -> put (xs, opts) >> return (Just x)

parseBench :: StateT ([String], Options) (MaybeT IO) ()
parseBench = do
    x <- shift
    case x of
        Just "linear" -> setBenchType Linear
        Just "linear-async" -> setBenchType LinearAsync
        Just "linear-rate" -> setBenchType LinearRate
        Just "nested" -> setBenchType Nested
        Just "base" -> setBenchType Base
        Just "fileio" -> setBenchType FileIO
        Just "array" -> setBenchType Array
        Just "concurrent" -> setBenchType Concurrent
        Just str -> do
                liftIO $ putStrLn $ "unrecognized benchmark type " <> str
                mzero
        Nothing -> do
                liftIO $ putStrLn "please provide a benchmark type "
                mzero

-- totally imperative style option parsing
parseOptions :: IO (Maybe Options)
parseOptions = do
    args <- getArgs
    runMaybeT $ flip evalStateT (args, defaultOptions) $ do
        parseLoop
        fmap snd get

    where

    parseOpt opt =
        case opt of
            "--graphs"     -> setGenGraphs True
            "--group-diff" -> setGroupDiff True
            "--benchmark"  -> parseBench
            str -> do
                liftIO $ putStrLn $ "Unrecognized option " <> str
                mzero

    parseLoop = do
        next <- shift
        case next of
            Just opt -> parseOpt opt >> parseLoop
            Nothing -> return ()

ignoringErr a = catch a (\(ErrorCall err :: ErrorCall) ->
    putStrLn $ "Failed with error:\n" <> err <> "\nSkipping.")

------------------------------------------------------------------------------
-- Linear composition charts
------------------------------------------------------------------------------

makeLinearGraphs :: Config -> String -> IO ()
makeLinearGraphs cfg@Config{..} inputFile = do
    ignoringErr $ graph inputFile "generation" $ cfg
        { title = (++) <$> title <*> Just " generation"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/generation/"
        }

    ignoringErr $ graph inputFile "elimination" $ cfg
        { title = (++) <$> title <*> Just " Elimination"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/elimination/"
        }

    ignoringErr $ graph inputFile "transformation-zip" $ cfg
        { title = (++) <$> title <*> Just " Transformation & Zip"
        , classifyBenchmark = \b ->
                if    "serially/transformation/" `isPrefixOf` b
                   || "serially/zipping" `isPrefixOf` b
                then Just ("Streamly", last $ splitOn "/" b)
                else Nothing
        }

    ignoringErr $ graph inputFile "filtering" $ cfg
        { title = (++) <$> title <*> Just " Filtering"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/filtering/"
        }

    ignoringErr $ graph inputFile "transformationX4" $ cfg
        { title = (++) <$> title <*> Just " Transformation x 4"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/transformationX4/"
        }

    ignoringErr $ graph inputFile "filteringX4"
        $ cfg
        { title = (++) <$> title <*> Just " Filtering x 4"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/filteringX4/"
        }

    ignoringErr $ graph inputFile "mixedX4"
        $ cfg
        { title = (++) <$> title <*> Just " Mixed x 4"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/mixedX4/"
        }

    ignoringErr $ graph inputFile "iterated"
        $ cfg
        { title = Just "iterate 10,000 times over 10 elems"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/iterated/"
        }

------------------------------------------------------------------------------
-- Nested composition charts
------------------------------------------------------------------------------

makeNestedGraphs :: Config -> String -> IO ()
makeNestedGraphs cfg inputFile =
    ignoringErr $ graph inputFile "nested-all" $ cfg
        { presentation = Groups Absolute
        , classifyBenchmark = classifyNested
        , selectGroups = \gs ->
            groupBy ((==) `on` snd) gs
            & fmap (\xs -> mapMaybe (\x -> (x,) <$> lookup x xs) order)
            & concat
        }

    where

    order = ["serially", "asyncly", "wAsyncly", "aheadly", "parallely"]

    classifyNested b
        | "serially/" `isPrefixOf` b =
            ("serially",) <$> stripPrefix "serially/" b
        | "asyncly/" `isPrefixOf` b =
            ("asyncly",) <$> stripPrefix "asyncly/" b
        | "wAsyncly/" `isPrefixOf` b =
            ("wAsyncly",) <$> stripPrefix "wAsyncly/" b
        | "aheadly/" `isPrefixOf` b =
            ("aheadly",) <$> stripPrefix "aheadly/" b
        | "parallely/" `isPrefixOf` b =
            ("parallely",) <$> stripPrefix "parallely/" b
        | otherwise = Nothing

------------------------------------------------------------------------------
-- Charts for parallel streams
------------------------------------------------------------------------------

makeLinearAsyncGraphs :: Config -> String -> IO ()
makeLinearAsyncGraphs cfg inputFile =
    ignoringErr $ graph inputFile "linear-async" cfg
        { presentation = Groups Absolute
        , classifyBenchmark = classifyAsync
        , selectGroups = \gs ->
            groupBy ((==) `on` snd) gs
            & fmap (\xs -> mapMaybe (\x -> (x,) <$> lookup x xs) order)
            & concat
        }

    where

    order = ["asyncly", "wAsyncly", "aheadly", "parallely"]

    classifyAsync b
        | "asyncly/" `isPrefixOf` b =
            ("asyncly",) <$> stripPrefix "asyncly/" b
        | "wAsyncly/" `isPrefixOf` b =
            ("wAsyncly",) <$> stripPrefix "wAsyncly/" b
        | "aheadly/" `isPrefixOf` b =
            ("aheadly",) <$> stripPrefix "aheadly/" b
        | "parallely/" `isPrefixOf` b =
            ("parallely",) <$> stripPrefix "parallely/" b
        | otherwise = Nothing

makeLinearRateGraphs :: Config -> String -> IO ()
makeLinearRateGraphs cfg inputFile = do
    putStrLn "Not implemented"
    return ()

------------------------------------------------------------------------------
-- FileIO
------------------------------------------------------------------------------

makeFileIOGraphs :: Config -> String -> IO ()
makeFileIOGraphs cfg@Config{..} inputFile =
    ignoringErr $ graph inputFile "fileIO" cfg

makeArrayGraphs :: Config -> String -> IO ()
makeArrayGraphs cfg@Config{..} inputFile =
    ignoringErr $ graph inputFile "array" cfg

makeConcurrentGraphs :: Config -> String -> IO ()
makeConcurrentGraphs cfg@Config{..} inputFile =
    ignoringErr $ graph inputFile "concurrent" cfg

------------------------------------------------------------------------------
-- Reports/Charts for base streams
------------------------------------------------------------------------------

showStreamDVsK Options{..} cfg inp out =
    let cfg' = cfg { classifyBenchmark = classifyBase }
    in if genGraphs
       then ignoringErr $ graph inp "streamD-vs-streamK"
                cfg' { outputDir = Just out
                     , presentation = Groups Absolute
                     }
       else ignoringErr $ report inp Nothing cfg'

    where

    classifyBase b
        | "streamD/" `isPrefixOf` b = ("streamD",) <$> stripPrefix "streamD/" b
        | "streamK/" `isPrefixOf` b = ("streamK",) <$> stripPrefix "streamK/" b
        | otherwise = Nothing

showStreamD Options{..} cfg inp out =
    let cfg' = cfg { classifyBenchmark = classifyStreamD }
    in if genGraphs
       then ignoringErr $ graph inp "streamD"
                cfg' {outputDir = Just out}
       else ignoringErr $ report inp Nothing cfg'

    where

    classifyStreamD b
        | "streamD/" `isPrefixOf` b = ("streamD",) <$> stripPrefix "streamD/" b
        | otherwise = Nothing

showStreamK Options{..} cfg inp out =
    let cfg' = cfg { classifyBenchmark = classifyStreamK }
    in if genGraphs
       then ignoringErr $ graph inp "streamK"
                cfg' {outputDir = Just out}
       else ignoringErr $ report inp Nothing cfg'

    where

    classifyStreamK b
        | "streamK/" `isPrefixOf` b = ("streamK",) <$> stripPrefix "streamK/" b
        | otherwise = Nothing

------------------------------------------------------------------------------
-- text reports
------------------------------------------------------------------------------

selectBench
    :: (SortColumn -> Maybe GroupStyle -> Either String [(String, Double)])
    -> [String]
selectBench f =
    reverse
    $ fmap fst
    $ either
      (const $ either error (sortOn snd) $ f (ColumnIndex 0) (Just PercentDiff))
      (sortOn snd)
      $ f (ColumnIndex 1) (Just PercentDiff)

benchShow Options{..} cfg func inp out =
    if genGraphs
    then func cfg {outputDir = Just out} inp
    else ignoringErr $ report inp Nothing cfg

main :: IO ()
main = do
    let cfg = defaultConfig
            { presentation = Groups PercentDiff
            , selectBenchmarks = selectBench
            }
    res <- parseOptions

    case res of
        Nothing -> do
            putStrLn "cannot parse options"
            return ()
        Just opts@Options{..} ->
            case benchType of
                Linear -> benchShow opts cfg
                            { title = Just "100,000 elems" }
                            makeLinearGraphs
                            "charts/linear/results.csv"
                            "charts/linear"
                LinearAsync -> benchShow opts cfg
                            { title = Just "Async 10,000 elems" }
                            makeLinearAsyncGraphs
                            "charts/linear-async/results.csv"
                            "charts/linear-async"
                LinearRate -> benchShow opts cfg makeLinearRateGraphs
                            "charts/linear-rate/results.csv"
                            "charts/linear-rate"
                Nested -> benchShow opts cfg
                            { title = Just "Nested loops 100 x 100 elems" }
                            makeNestedGraphs
                            "charts/nested/results.csv"
                            "charts/nested"
                FileIO -> benchShow opts cfg
                            { title = Just "File IO" }
                            makeFileIOGraphs
                            "charts/fileio/results.csv"
                            "charts/fileio"
                Array -> benchShow opts cfg
                            { title = Just "Array" }
                            makeArrayGraphs
                            "charts/array/results.csv"
                            "charts/array"
                Concurrent -> benchShow opts cfg
                            { title = Just "Concurrent Ops" }
                            makeConcurrentGraphs
                            "charts/concurrent/results.csv"
                            "charts/concurrent"
                Base -> do
                    let cfg' = cfg { title = Just "100,000 elems" }
                    if groupDiff
                    then showStreamDVsK opts cfg'
                                "charts/base/results.csv"
                                "charts/base"
                    else do
                        showStreamD opts cfg'
                                "charts/base/results.csv"
                                "charts/base"
                        showStreamK opts cfg'
                                "charts/base/results.csv"
                                "charts/base"
