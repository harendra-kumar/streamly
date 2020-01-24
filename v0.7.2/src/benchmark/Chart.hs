{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (handle, catch, SomeException, ErrorCall(..))
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.Char (toLower)
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
    | NestedConcurrent
    | Parser
    | Base
    | FileIO
    | Array
    | ArrayCmp
    | UnpinnedArray
    | SmallArray
    | PrimArray
    | Concurrent
    | Parallel
    | Adaptive
    | FoldO1Space
    | FoldOnHeap
    | UnfoldO1Space
    | UnfoldOnSpace
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
        Just "nested-concurrent" -> setBenchType NestedConcurrent
        Just "parser" -> setBenchType Parser
        Just "base" -> setBenchType Base
        Just "fileio" -> setBenchType FileIO
        Just "array-cmp" -> setBenchType ArrayCmp
        Just "array" -> setBenchType Array
        Just "unpinned-array" -> setBenchType UnpinnedArray
        Just "small-array" -> setBenchType SmallArray
        Just "prim-array" -> setBenchType PrimArray
        Just "concurrent" -> setBenchType Concurrent
        Just "parallel" -> setBenchType Parallel
        Just "adaptive" -> setBenchType Adaptive
        Just "fold-o-1-space" -> setBenchType FoldO1Space
        Just "fold-o-n-heap" -> setBenchType FoldOnHeap
        Just "unfold-o-1-space" -> setBenchType UnfoldO1Space
        Just "unfold-o-n-space" -> setBenchType UnfoldOnSpace
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
-- Stream type based comparison charts
------------------------------------------------------------------------------

makeStreamComparisonGraphs :: String -> [String] -> Config -> String -> IO ()
makeStreamComparisonGraphs outputPrefix benchPrefixes cfg inputFile =
    ignoringErr $ graph inputFile outputPrefix $ cfg
        { presentation = Groups Absolute
        , classifyBenchmark = classifyNested
        , selectGroups = \gs ->
            groupBy ((==) `on` snd) gs
            & fmap (\xs -> mapMaybe (\x -> (x,) <$> lookup x xs) benchPrefixes)
            & concat
        }

    where

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

linearAsyncPrefixes = ["asyncly", "wAsyncly", "aheadly", "parallely"]
nestedBenchPrefixes = ["serially"] ++ linearAsyncPrefixes

------------------------------------------------------------------------------
-- Generic
------------------------------------------------------------------------------

makeGraphs :: String -> Config -> String -> IO ()
makeGraphs name cfg@Config{..} inputFile =
    ignoringErr $ graph inputFile name cfg

------------------------------------------------------------------------------
-- Arrays
------------------------------------------------------------------------------

showArrayComparisons Options{..} cfg inp out =
    let cfg' = cfg { classifyBenchmark = classifyArray }
    in if genGraphs
       then ignoringErr $ graph inp "Arrays Comparison"
                cfg' { outputDir = Just out
                     , presentation = Groups Absolute
                     }
       else ignoringErr $ report inp Nothing cfg'

    where

    classifyArray b
        -- SmallArray uses a small number of elements therefore cannot be
        -- compared
        -- | "SmallArray/" `isPrefixOf` b = ("SmallArray",) <$> stripPrefix "SmallArray/" b
        | "Data.Prim.Array/" `isPrefixOf` b = ("Data.Prim.Array",) <$> stripPrefix "Data.Prim.Array/" b
        | "Data.Array/" `isPrefixOf` b = ("Data.Array",) <$> stripPrefix "Data.Array/" b
        | "array/" `isPrefixOf` b = ("array",) <$> stripPrefix "array/" b
        | otherwise = Nothing

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
            , selectFields = filter
                ( flip elem ["time" , "mean"
                            , "maxrss", "cputime"
                            ]
                . map toLower
                )
            }
    res <- parseOptions

    case res of
        Nothing -> do
            putStrLn "cannot parse options"
            return ()
        Just opts@Options{..} ->
            case benchType of
                Linear -> benchShow opts cfg
                            { title = Just "Linear" }
                            makeLinearGraphs
                            "charts/linear/results.csv"
                            "charts/linear"
                LinearRate -> benchShow opts cfg
                            { title = Just "Linear Rate" }
                            (makeGraphs "linear-rate")
                            "charts/linear-rate/results.csv"
                            "charts/linear-rate"
                LinearAsync -> benchShow opts cfg
                            { title = Just "Linear Async" }
                            (makeStreamComparisonGraphs "linear-async" linearAsyncPrefixes)
                            "charts/linear-async/results.csv"
                            "charts/linear-async"
                NestedConcurrent -> benchShow opts cfg
                            { title = Just "Nested concurrent loops" }
                            (makeStreamComparisonGraphs "nested-concurrent" nestedBenchPrefixes)
                            "charts/nested-concurrent/results.csv"
                            "charts/nested-concurrent"
                Parser -> benchShow opts cfg
                            { title = Just "Parsers" }
                            (makeGraphs "parser")
                            "charts/parser/results.csv"
                            "charts/parser"
                FileIO -> benchShow opts cfg
                            { title = Just "File IO" }
                            (makeGraphs "fileIO")
                            "charts/fileio/results.csv"
                            "charts/fileio"
                Array -> benchShow opts cfg
                            { title = Just "Array" }
                            (makeGraphs "array")
                            "charts/array/results.csv"
                            "charts/array"
                UnpinnedArray -> benchShow opts cfg
                            { title = Just "Unpinned Array" }
                            (makeGraphs "unpinned-array")
                            "charts/unpinned-array/results.csv"
                            "charts/unpinned-array"
                SmallArray -> benchShow opts cfg
                            { title = Just "Small Array" }
                            (makeGraphs "small-array")
                            "charts/small-array/results.csv"
                            "charts/small-array"
                PrimArray -> benchShow opts cfg
                            { title = Just "Prim Array" }
                            (makeGraphs "prim-array")
                            "charts/prim-array/results.csv"
                            "charts/prim-array"
                ArrayCmp -> showArrayComparisons opts cfg
                            { title = Just "Arrays Comparison" }
                            "charts/array-cmp/results.csv"
                            "charts/array-cmp"
                Concurrent -> benchShow opts cfg
                            { title = Just "Concurrent Ops" }
                            (makeGraphs "Concurrent")
                            "charts/concurrent/results.csv"
                            "charts/concurrent"
                Parallel -> benchShow opts cfg
                            { title = Just "Parallel" }
                            (makeGraphs "parallel")
                            "charts/parallel/results.csv"
                            "charts/parallel"
                Adaptive -> benchShow opts cfg
                            { title = Just "Adaptive" }
                            (makeGraphs "adaptive")
                            "charts/adaptive/results.csv"
                            "charts/adaptive"
                Base -> do
                    let cfg' = cfg { title = Just "Base stream" }
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
                FoldO1Space -> benchShow opts cfg
                            { title = Just "Fold O(1) Space" }
                            (makeGraphs "fold-o-1-space")
                            "charts/fold-o-1-space/results.csv"
                            "charts/fold-o-1-space"
                FoldOnHeap -> benchShow opts cfg
                            { title = Just "Fold O(n) Heap" }
                            (makeGraphs "fold-o-n-heap")
                            "charts/fold-o-n-heap/results.csv"
                            "charts/fold-o-n-heap"
                UnfoldO1Space -> benchShow opts cfg
                            { title = Just "Unfold O(1) Space" }
                            (makeGraphs "unfold-o-1-space")
                            "charts/unfold-o-1-space/results.csv"
                            "charts/unfold-o-1-space"
                UnfoldOnSpace -> benchShow opts cfg
                            { title = Just "Unfold O(n) Space" }
                            (makeGraphs "unfold-o-n-space")
                            "charts/unfold-o-n-space/results.csv"
                            "charts/unfold-o-n-space"
