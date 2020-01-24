-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}

import qualified Streamly.Benchmark.Data.Stream.StreamK as K

#if !defined(O_N_HEAP)
import qualified Streamly.Benchmark.Data.Stream.StreamD as D
#endif

#ifdef O_1_SPACE
import qualified Streamly.Benchmark.Data.Stream.StreamDK as DK
#endif

import Gauge

main :: IO ()
main =
  defaultMain $
#ifdef O_1_SPACE
       D.o_1_space
    ++ K.o_1_space_list
    ++ K.o_1_space
    ++ DK.o_1_space
#elif defined(O_N_HEAP)
       K.o_n_heap
#elif defined(O_N_STACK)
       D.o_n_stack
    ++ K.o_n_stack
#elif defined(O_N_SPACE)
       D.o_n_space
    ++ K.o_n_space
#else
#error "One of O_1_SPACE/O_N_HEAP/O_N_STACK/O_N_SPACE must be defined"
#endif
