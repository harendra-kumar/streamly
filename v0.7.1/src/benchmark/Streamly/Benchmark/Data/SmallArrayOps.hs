-- |
-- Module      : Streamly.Benchmark.Data.SmallArrayOps
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.Benchmark.Data.SmallArrayOps where

import Control.Monad.IO.Class (MonadIO)
import Prelude (Int, Bool, (+), ($), (==), (>), (.), Maybe(..), undefined)
import qualified Prelude as P
#ifdef DEVBUILD
import qualified Data.Foldable as F
#endif

import qualified Streamly           as S hiding (foldMapWith, runStream)
import qualified Streamly.Internal.Data.SmallArray as A
import qualified Streamly.Prelude   as S

value :: Int
value = 128

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream = A.SmallArray

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: MonadIO m => Int -> m (Stream Int)
sourceUnfoldr n = S.fold (A.writeN value) $ S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else (Just (cnt, cnt + 1))

{-# INLINE sourceIntFromTo #-}
sourceIntFromTo :: MonadIO m => Int -> m (Stream Int)
sourceIntFromTo n = S.fold (A.writeN value) $ S.enumerateFromTo n (n + value)

{-# INLINE sourceIntFromToFromList #-}
sourceIntFromToFromList :: MonadIO m => Int -> m (Stream Int)
sourceIntFromToFromList n = P.return $ (A.fromListN value) $ [n..n + value]

{-# INLINE sourceFromList #-}
sourceFromList :: MonadIO m => Int -> m (Stream Int)
sourceFromList n = S.fold (A.writeN value) $ S.fromList [n..n+value]

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE composeN #-}
composeN :: P.Monad m
    => Int -> (Stream Int -> m (Stream Int)) -> Stream Int -> m (Stream Int)
composeN n f x =
    case n of
        1 -> f x
        2 -> f x P.>>= f
        3 -> f x P.>>= f P.>>= f
        4 -> f x P.>>= f P.>>= f P.>>= f
        _ -> undefined

{-# INLINE scanl' #-}
{-# INLINE scanl1' #-}
{-# INLINE map #-}

scanl', scanl1', map
    :: MonadIO m => Int -> Stream Int -> m (Stream Int)

{-# INLINE onArray #-}
onArray
    :: MonadIO m => (S.SerialT m Int -> S.SerialT m Int)
    -> Stream Int
    -> m (Stream Int)
onArray f arr = S.fold (A.writeN value) $ f $ (S.unfold A.read arr)

scanl'        n = composeN n $ onArray $ S.scanl' (+) 0
scanl1'       n = composeN n $ onArray $ S.scanl1' (+)
map           n = composeN n $ onArray $ S.map (+1)

{-# INLINE eqInstance #-}
eqInstance :: Stream Int -> Bool
eqInstance src = src == src

{-# INLINE eqInstanceNotEq #-}
eqInstanceNotEq :: Stream Int -> Bool
eqInstanceNotEq src = src P./= src

{-# INLINE ordInstance #-}
ordInstance :: Stream Int -> Bool
ordInstance src = src P.< src

{-# INLINE ordInstanceMin #-}
ordInstanceMin :: Stream Int -> Stream Int
ordInstanceMin src = P.min src src

{-# INLINE showInstance #-}
showInstance :: Stream Int -> P.String
showInstance src = P.show src

{-# INLINE readInstance #-}
readInstance :: P.String -> Stream Int
readInstance str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "readInstance: no parse"

{-# INLINE pureFoldl' #-}
pureFoldl' :: MonadIO m => Stream Int -> m Int
pureFoldl' = S.foldl' (+) 0 . S.unfold A.read

#ifdef DEVBUILD
{-# INLINE foldableFoldl' #-}
foldableFoldl' :: Stream Int -> Int
foldableFoldl' = F.foldl' (+) 0

{-# INLINE foldableSum #-}
foldableSum :: Stream Int -> Int
foldableSum = P.sum
#endif
