module Main (main) where

import Data.Semigroup (Sum(..), getSum)
import Streamly.Test.Common (checkListEqual)
import Test.QuickCheck
    ( Gen
    , Property
    , arbitrary
    , choose
    , forAll
    , listOf
    , listOf1
    , property
    , vectorOf
    , withMaxSuccess
    )
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import qualified Prelude
import qualified Streamly.Internal.Data.Fold as F
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Data.Fold as FL

import Prelude hiding
    (maximum, minimum, elem, notElem, null, product, sum, head, last, take)
import Test.Hspec as H
import Test.Hspec.QuickCheck

maxStreamLen :: Int
maxStreamLen = 1000

intMin :: Int
intMin = minBound

intMax :: Int
intMax = maxBound

min_value :: Int
min_value = 0

max_value :: Int
max_value = 10000

chooseInt :: (Int, Int) -> Gen Int
chooseInt = choose

{-# INLINE maxStreamLen #-}
{-# INLINE intMin #-}
{-# INLINE intMax #-}

rollingHashFirstN :: Property
rollingHashFirstN =
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (choose (0, len)) $ \n ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec -> monadicIO $ do
                a <- run $ S.fold F.rollingHash $ S.take n $ S.fromList vec
                b <- run $ S.fold (F.rollingHashFirstN n) $ S.fromList vec
                assert $ a == b

head :: [Int] -> Expectation
head ls = S.fold FL.head (S.fromList ls) `shouldReturn` headl ls

    where

    headl [] = Nothing
    headl (x:_) = Just x

length :: [Int] -> Expectation
length ls = S.fold FL.length (S.fromList ls) `shouldReturn` Prelude.length ls

sum :: [Int] -> Expectation
sum ls = S.fold FL.sum (S.fromList ls) `shouldReturn` Prelude.sum ls

product :: [Int] -> Expectation
product ls =
    S.fold FL.product (S.fromList ls) `shouldReturn` Prelude.product ls

lesser :: (a -> a -> Ordering) -> a -> a -> a
lesser f x y = if f x y == LT then x else y

greater :: (a -> a -> Ordering) -> a -> a -> a
greater f x y = if f x y == GT then x else y

foldMaybe :: (b -> a -> b) -> b -> [a] -> Maybe b
foldMaybe f acc ls =
    case ls of
        [] -> Nothing
        _ -> Just (foldl f acc ls)

maximumBy :: (Ord a, Show a) => a -> (a -> a -> Ordering) -> [a] -> Expectation
maximumBy genmin f ls =
    S.fold (FL.maximumBy f) (S.fromList ls)
        `shouldReturn` foldMaybe (greater f) genmin ls

maximum :: (Show a, Ord a) => a -> [a] -> Expectation
maximum genmin ls =
    S.fold FL.maximum (S.fromList ls)
        `shouldReturn` foldMaybe (greater compare) genmin ls

minimumBy :: (Ord a, Show a) => a -> (a -> a -> Ordering) -> [a] -> Expectation
minimumBy genmax f ls =
    S.fold (FL.minimumBy f) (S.fromList ls)
        `shouldReturn` foldMaybe (lesser f) genmax ls

minimum :: (Show a, Ord a) => a -> [a] -> Expectation
minimum genmax ls =
    S.fold FL.minimum (S.fromList ls)
        `shouldReturn` foldMaybe (lesser compare) genmax ls

toList :: [Int] -> Expectation
toList ls = S.fold FL.toList (S.fromList ls) `shouldReturn` ls

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs

last :: [String] -> Expectation
last ls = S.fold FL.last (S.fromList ls) `shouldReturn` safeLast ls

mapMaybe :: [Int] -> Expectation
mapMaybe ls =
    let maybeEven x =
            if even x
            then Just x
            else Nothing
        f = F.mapMaybe maybeEven FL.toList
    in S.fold f (S.fromList ls) `shouldReturn` filter even ls

nth :: Int -> [a] -> Maybe a
nth idx (x : xs)
    | idx == 0 = Just x
    | idx < 0 = Nothing
    | otherwise = nth (idx - 1) xs
nth _ [] = Nothing

index :: Int -> [String] -> Expectation
index idx ls =
    let x = S.fold (FL.index idx) (S.fromList ls)
    in x `shouldReturn` nth idx ls

find :: (Show a, Eq a) => (a -> Bool) -> [a] -> Expectation
find f ls = do
    y <- S.fold (FL.findIndex f) (S.fromList ls)
    case y of
        Nothing ->
            let fld = S.fold (FL.find f) (S.fromList ls)
            in fld `shouldReturn` Nothing
        Just idx ->
            let fld = S.fold (FL.any f) (S.fromList $ Prelude.take idx ls)
            in fld `shouldReturn` False

neg :: (a -> Bool) -> a -> Bool
neg f x = not (f x)

findIndex :: (a -> Bool) -> [a] -> Expectation
findIndex f ls = do
    y <- S.fold (FL.findIndex f) (S.fromList ls)
    case y of
        Nothing  ->
            let fld = S.fold (FL.all $ neg f) (S.fromList ls)
            in fld `shouldReturn` True
        Just idx ->
            if idx == 0
            then
                S.fold (FL.all f) (S.fromList []) `shouldReturn` True
            else
                S.fold (FL.all f) (S.fromList $ Prelude.take idx ls)
                    `shouldReturn` False

predicate :: Int -> Bool
predicate x = x * x < 100

elemIndex :: Int -> [Int] -> Expectation
elemIndex elm ls = do
    y <- S.fold (FL.elemIndex elm) (S.fromList ls)
    case y of
        Nothing ->
            let fld = S.fold (FL.any (== elm)) (S.fromList ls)
            in fld `shouldReturn` False
        Just idx ->
            let fld = S.fold (FL.any (== elm)) (S.fromList $ Prelude.take idx ls)
            in fld `shouldReturn` False

null :: [Int] -> Expectation
null ls =
    S.fold FL.null (S.fromList ls)
        `shouldReturn`
            case ls of
                [] -> True
                _ -> False

elem :: Int -> [Int] -> Expectation
elem elm ls = do
    y <- S.fold (FL.elem elm) (S.fromList ls)
    let fld = S.fold (FL.any (== elm)) (S.fromList ls)
    fld `shouldReturn` y

notElem :: Int -> [Int] -> Expectation
notElem elm ls = do
    y <- S.fold (FL.notElem elm) (S.fromList ls)
    let fld = S.fold (FL.any (== elm)) (S.fromList ls)
    fld `shouldReturn` not y

all :: (a -> Bool) -> [a] -> Expectation
all f ls =
    S.fold (FL.all f) (S.fromList ls) `shouldReturn` Prelude.all f ls

any :: (a -> Bool) -> [a] -> Expectation
any f ls = S.fold (FL.any f) (S.fromList ls) `shouldReturn` Prelude.any f ls

and :: [Bool] -> Expectation
and ls = S.fold FL.and (S.fromList ls) `shouldReturn` Prelude.and ls

or :: [Bool] -> Expectation
or ls = S.fold FL.or (S.fromList ls) `shouldReturn` Prelude.or ls

take :: [Int] -> Property
take ls =
    forAll (chooseInt (-1, Prelude.length ls + 2)) $ \n ->
            S.fold (F.take n FL.toList) (S.fromList ls)
                `shouldReturn` Prelude.take n ls

takeEndBy_ :: Property
takeEndBy_ =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        let p = (== 1)
            f = F.takeEndBy_ p FL.toList
            ys = Prelude.takeWhile (not . p) ls
         in case S.fold f (S.fromList ls) of
            Right xs -> checkListEqual xs ys
            Left _ -> property False

takeEndByOrMax :: Property
takeEndByOrMax =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (0, 1))) $ \ls ->
            let p = (== 1)
                f = F.takeEndBy_ p (F.take n FL.toList)
                ys = Prelude.take n (Prelude.takeWhile (not . p) ls)
             in case S.fold f (S.fromList ls) of
                    Right xs -> checkListEqual xs ys
                    Left _ -> property False

chooseFloat :: (Float, Float) -> Gen Float
chooseFloat = choose

drain :: [Int] -> Expectation
drain ls = S.fold FL.drain (S.fromList ls) `shouldReturn` ()

drainBy :: [Int] -> Expectation
drainBy ls = S.fold (FL.drainBy return) (S.fromList ls) `shouldReturn` ()

mean :: Property
mean =
    forAll (listOf1 (chooseFloat (-100.0, 100.0)))
        $ \ls0 -> withMaxSuccess 1000 $ monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ S.fold FL.mean (S.fromList ls)
        let v2 = Prelude.sum ls / fromIntegral (Prelude.length ls)
        assert (abs (v1 - v2) < 0.0001)

stdDev :: Property
stdDev =
    forAll (listOf1 (chooseFloat (-100.0, 100.0)))
        $ \ls0 -> withMaxSuccess 1000 $ monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ S.fold FL.stdDev (S.fromList ls)
        let avg = Prelude.sum ls / fromIntegral (Prelude.length ls)
            se = Prelude.sum (fmap (\x -> (x - avg) * (x - avg)) ls)
            sd = sqrt $ se / fromIntegral (Prelude.length ls)
        assert (abs (v1 - sd) < 0.0001 )

variance :: Property
variance =
    forAll (listOf1 (chooseFloat (-100.0, 100.0)))
        $ \ls0 -> withMaxSuccess 1000 $ monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ S.fold FL.variance (S.fromList ls)
        let avg = Prelude.sum ls / fromIntegral (Prelude.length ls)
            se = Prelude.sum (fmap (\x -> (x - avg) * (x - avg)) ls)
            vr = se / fromIntegral (Prelude.length ls)
        assert (abs (v1 - vr) < 0.01 )

mconcat :: Property
mconcat =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ S.fold FL.mconcat (S.map Sum $ S.fromList ls)
        let v2 = Prelude.sum ls
        assert (getSum v1 == v2)

foldMap :: Property
foldMap =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ S.fold (FL.foldMap Sum) $ S.fromList ls
        let v2 = Prelude.sum ls
        assert (getSum v1 == v2)

foldMapM :: Property
foldMapM =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ S.fold (FL.foldMapM (return . Sum)) $ S.fromList ls
        let v2 = Prelude.sum ls
        assert (getSum v1 == v2)

lookup :: Property
lookup =
    forAll (chooseInt (1, 15))
        $ \key0 ->monadicIO $ action key0

    where

    action key = do
        let ls = [(1, "first"), (2, "second"), (3, "third"), (4, "fourth")
                , (5, "fifth"), (6, "fifth+first"), (7, "fifth+second")
                , (8, "fifth+third"), (9, "fifth+fourth")
                , (10, "fifth+fifth")]
        v1 <- run $ S.fold (FL.lookup key) $ S.fromList ls
        let v2 = Prelude.lookup key ls
        assert (v1 == v2)

rmapM :: Property
rmapM =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        let addLen x = return $ x + Prelude.length ls
            fld = FL.rmapM addLen FL.sum
            v2 = foldl (+) (Prelude.length ls) ls
        v1 <- run $ S.fold fld $ S.fromList ls
        assert (v1 == v2)

teeWithLength :: Property
teeWithLength =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ S.fold (FL.tee FL.sum FL.length) $ S.fromList ls
        let v2 = Prelude.sum ls
            v3 = Prelude.length ls
        assert (v1 == (v2, v3))

teeWithMax :: Property
teeWithMax =
    forAll (listOf1 (chooseInt (intMin, intMax)))
       $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ S.fold (FL.tee FL.sum FL.maximum) $ S.fromList ls
        let v2 = Prelude.sum ls
            v3 = foldMaybe (greater compare) intMin ls
        assert (v1 == (v2, v3))

distribute :: Property
distribute =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ S.fold (FL.distribute [FL.sum, FL.length]) $ S.fromList ls
        let v2 = Prelude.sum ls
            v3 = Prelude.length ls
        assert (v1 == [v2, v3])

partition :: Property
partition =
    monadicIO $ do
        v1 :: (Int, [String]) <-
            run
                $ S.fold (FL.partition FL.sum FL.toList)
                $ S.fromList
                    [Left 1, Right "abc", Left 3, Right "xy", Right "pp2"]
        let v2 = (4,["abc","xy","pp2"])
        assert (v1 == v2)

unzip :: Property
unzip =
    monadicIO $ do
    v1 :: (Int, [String]) <-
        run
            $ S.fold (FL.unzip FL.sum FL.toList)
            $ S.fromList [(1, "aa"), (2, "bb"), (3, "cc")]
    let v2 = (6, ["aa", "bb", "cc"])
    assert (v1 == v2)

many :: Property
many =
    forAll (listOf (chooseInt (0, 100))) $ \lst ->
    forAll (chooseInt (1, 100)) $ \i ->
        monadicIO $ do
            let strm = S.fromList lst
            r1 <- S.fold (F.many (split i) F.toList) strm
            r2 <- S.toList $ S.foldMany (split i) strm
            assert $ r1 == r2

    where

    split i = F.take i F.toList

headAndRest :: [Int] -> Property
headAndRest ls = monadicIO $ do
    (mbh, rest) <- run $ Stream.fold_ FL.head (S.fromList ls)
    rests <- run $ S.toList rest
    assert (mbh == headl ls)
    listEquals (==) rests (taill ls)

    where

    taill :: [a] -> [a]
    taill [] = []
    taill (_:xs) = xs

main :: IO ()
main = hspec $
    describe "Fold" $ do
        -- Folds
        -- Accumulators
        prop "mconcat" Main.mconcat
        prop "foldMap" Main.foldMap
        prop "foldMapM" Main.foldMapM

        prop "drain" Main.drain
        prop "drainBy" Main.drainBy
        prop "last" last
        prop "length" Main.length
        prop "sum" sum
        prop "product" product
        prop "maximumBy" $ maximumBy intMin compare
        prop "maximum" $ maximum intMin
        prop "minimumBy" $ minimumBy intMax compare
        prop "minimum" $ minimum intMax
        prop "mean" Main.mean
        prop "stdDev" Main.stdDev
        prop "variance" Main.variance
        prop "rollingHashFirstN" rollingHashFirstN

        prop "toList" toList

        -- Terminating folds
        prop "index" index
        prop "head" head
        prop "find" $ find predicate
        prop "lookup" Main.lookup
        prop "findIndex" $ findIndex predicate
        prop "elemIndex" $ elemIndex 10
        prop "null" null
        prop "elem" $ elem 10
        prop "notElem" $ notElem 10
        prop "all" $ Main.all predicate
        prop "any" $ Main.any predicate
        prop "and" Main.and
        prop "or" Main.or

        -- Combinators

        -- Transformation
        -- rsequence
        -- Functor instance
        prop "rmapM" Main.rmapM
        -- lmap/lmapM

        -- Filtering
        -- filter/filterM
        -- catMaybes
        prop "mapMaybe" mapMaybe

        -- Trimming
        prop "take" take
        -- takeEndBy
        prop "takeEndBy_" takeEndBy_
        prop "takeEndByOrMax" takeEndByOrMax

        -- Appending
        -- serialWith

        -- Distributing
        -- tee
        prop "teeWithLength" Main.teeWithLength
        prop "teeWithMax" Main.teeWithMax
        prop "distribute" Main.distribute

        -- Partitioning
        prop "partition" Main.partition

        -- Unzipping
        prop "unzip" Main.unzip

        -- Nesting
        prop "many" Main.many
        -- concatMap
        -- chunksOf
    describe "FoldAndReturn s" $ do
        prop "head from fold_" headAndRest
