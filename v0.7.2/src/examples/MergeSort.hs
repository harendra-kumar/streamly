{-# LANGUAGE FlexibleContexts    #-}

-- | This example generates two streams sorted in ascending order and merges
-- them in ascending order, concurrently.
--
-- Compile with '-threaded -with-rtsopts "-N"' GHC options to use the
-- parallelism.

import Data.Word
import System.Random (getStdGen, randoms)
import Data.List (sort)
import Data.Ord (compare)

import Streamly
import qualified Streamly.Prelude as S

getSorted :: Serial Word16
getSorted = do
    g <- S.yieldM getStdGen
    let ls = take 100000 (randoms g) :: [Word16]
    foldMap return (sort ls)

main :: IO ()
main = S.last (S.mergeAsyncBy compare getSorted getSorted) >>= print
