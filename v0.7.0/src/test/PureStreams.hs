{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Hspec
import qualified GHC.Exts as GHC

import Streamly

#ifdef USE_STREAMLY_LIST
import Data.Functor.Identity
import Streamly.Internal.Data.List (List(..), pattern Cons, pattern Nil, ZipList(..),
                     fromZipList, toZipList)
import qualified Streamly.Prelude as S
#else
import Prelude -- to suppress compiler warning

type List = []

pattern Nil :: [a]
pattern Nil <- [] where Nil = []

pattern Cons :: a -> [a] -> [a]
pattern Cons x xs = x : xs
infixr 5 `Cons`

{-# COMPLETE Nil, Cons #-}
#endif

main :: IO ()
main = hspec $ do
#ifdef USE_STREAMLY_LIST
    describe "OverloadedLists for 'SerialT Identity' type" $ do
        it "Overloaded lists" $ do
            ([1..3] :: SerialT Identity Int) `shouldBe` S.fromList [1..3]
            GHC.toList ([1..3] :: SerialT Identity Int) `shouldBe` [1..3]

        it "Show instance" $ do
            show (S.fromList [1..3] :: SerialT Identity Int)
                `shouldBe` "fromList [1,2,3]"
        it "Read instance" $ do
            (read "fromList [1,2,3]" :: SerialT Identity Int) `shouldBe` [1..3]

        it "Eq instance" $ do
            ([1,2,3] :: SerialT Identity Int) == [1,2,3] `shouldBe` True

        it "Ord instance" $ do
            ([1,2,3] :: SerialT Identity Int) > [1,2,1] `shouldBe` True

        it "Monad comprehension" $ do
            [(x,y) | x <- [1..2], y <- [1..2]] `shouldBe`
                ([(1,1), (1,2), (2,1), (2,2)] :: SerialT Identity (Int, Int))

        it "Foldable (sum)" $ sum ([1..3] :: SerialT Identity Int)
            `shouldBe` 6

        it "Traversable (mapM)" $
            mapM return ([1..10] :: SerialT Identity Int)
                `shouldReturn` [1..10]

    describe "OverloadedStrings for 'SerialT Identity' type" $ do
        it "overloaded strings" $ do
            ("hello" :: SerialT Identity Char) `shouldBe` S.fromList "hello"
#endif

    describe "OverloadedLists for List type" $ do
        it "overloaded lists" $ do
            ([1..3] :: List Int) `shouldBe` GHC.fromList [1..3]
            GHC.toList ([1..3] :: List Int) `shouldBe` [1..3]

        it "Construct empty list" $ (Nil :: List Int) `shouldBe` []

        it "Construct a list" $ do
            (Nil :: List Int) `shouldBe` []
            ('x' `Cons` Nil) `shouldBe` ['x']
            (1 `Cons` [2 :: Int]) `shouldBe` [1,2]
            (1 `Cons` 2 `Cons` 3 `Cons` Nil :: List Int) `shouldBe` [1,2,3]

        it "pattern matches" $ do
            case [] of
                Nil -> return ()
                _ -> expectationFailure "not reached"

            case ['x'] of
                Cons 'x' Nil -> return ()
                _ -> expectationFailure "not reached"

            case [1..10 :: Int] of
                Cons x xs -> do
                    x `shouldBe` 1
                    xs `shouldBe` [2..10]
                _ -> expectationFailure "not reached"

            case [1..10 :: Int] of
                x `Cons` y `Cons` xs -> do
                    x `shouldBe` 1
                    y `shouldBe` 2
                    xs `shouldBe` [3..10]
                _ -> expectationFailure "not reached"

        it "Show instance" $ do
            show ([1..3] :: List Int) `shouldBe`
#ifdef USE_STREAMLY_LIST
                "List {toSerial = fromList [1,2,3]}"
#else
                "[1,2,3]"
#endif

        it "Read instance" $ do
            (read
#ifdef USE_STREAMLY_LIST
                "List {toSerial = fromList [1,2,3]}"
#else
                "[1,2,3]"
#endif
                :: List Int) `shouldBe` [1..3]

        it "Eq instance" $ do
            ([1,2,3] :: List Int) == [1,2,3] `shouldBe` True

        it "Ord instance" $ do
            ([1,2,3] :: List Int) > [1,2,1] `shouldBe` True

        it "Monad comprehension" $ do
            [(x,y) | x <- [1..2], y <- [1..2]] `shouldBe`
                ([(1,1), (1,2), (2,1), (2,2)] :: List (Int, Int))

        it "Foldable (sum)" $ sum ([1..3] :: List Int) `shouldBe` 6

        it "Traversable (mapM)" $
            mapM return ([1..10] :: List Int)
                `shouldReturn` [1..10]

    describe "OverloadedStrings for List type" $ do
        it "overloaded strings" $ do
            ("hello" :: List Char) `shouldBe` GHC.fromList "hello"

        it "pattern matches" $ do
#if __GLASGOW_HASKELL__ >= 802
            case "" of
#else
            case "" :: List Char of
#endif
                Nil -> return ()
                _ -> expectationFailure "not reached"

            case "a" of
                Cons x Nil -> x `shouldBe` 'a'
                _ -> expectationFailure "not reached"

            case "hello" <> "world" of
                Cons x1 (Cons x2 xs) -> do
                    x1 `shouldBe` 'h'
                    x2 `shouldBe` 'e'
                    xs `shouldBe` "lloworld"
                _ -> expectationFailure "not reached"

#ifdef USE_STREAMLY_LIST
    describe "OverloadedLists for ZipList type" $ do
        it "overloaded lists" $ do
            ([1..3] :: ZipList Int) `shouldBe` GHC.fromList [1..3]
            GHC.toList ([1..3] :: ZipList Int) `shouldBe` [1..3]

        it "toZipList" $ do
            toZipList (Nil :: List Int) `shouldBe` []
            toZipList ('x' `Cons` Nil) `shouldBe` ['x']
            toZipList (1 `Cons` [2 :: Int]) `shouldBe` [1,2]
            toZipList (1 `Cons` 2 `Cons` 3 `Cons` Nil :: List Int) `shouldBe` [1,2,3]

        it "fromZipList" $ do
            case fromZipList [] of
                Nil -> return ()
                _ -> expectationFailure "not reached"

            case fromZipList ['x'] of
                Cons 'x' Nil -> return ()
                _ -> expectationFailure "not reached"

            case fromZipList [1..10 :: Int] of
                Cons x xs -> do
                    x `shouldBe` 1
                    xs `shouldBe` [2..10]
                _ -> expectationFailure "not reached"

            case fromZipList [1..10 :: Int] of
                x `Cons` y `Cons` xs -> do
                    x `shouldBe` 1
                    y `shouldBe` 2
                    xs `shouldBe` [3..10]
                _ -> expectationFailure "not reached"

        it "Show instance" $ do
            show ([1..3] :: ZipList Int) `shouldBe`
                "ZipList {toZipSerial = fromList [1,2,3]}"

        it "Read instance" $ do
            (read "ZipList {toZipSerial = fromList [1,2,3]}" :: ZipList Int)
                `shouldBe` [1..3]

        it "Eq instance" $ do
            ([1,2,3] :: ZipList Int) == [1,2,3] `shouldBe` True

        it "Ord instance" $ do
            ([1,2,3] :: ZipList Int) > [1,2,1] `shouldBe` True

        it "Foldable (sum)" $ sum ([1..3] :: ZipList Int) `shouldBe` 6

        it "Traversable (mapM)" $
            mapM return ([1..10] :: ZipList Int)
                `shouldReturn` [1..10]

        it "Applicative Zip" $ do
            (,) <$> "abc" <*> [1..3] `shouldBe`
                ([('a',1),('b',2),('c',3)] :: ZipList (Char, Int))
#endif
