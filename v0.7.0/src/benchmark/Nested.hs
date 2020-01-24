-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Control.DeepSeq (NFData)
import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)
import qualified NestedOps as Ops
import Streamly
import Gauge

benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

_benchId :: (NFData b) => String -> (Int -> Identity b) -> Benchmark
_benchId name f = bench name $ nf (\g -> runIdentity (g 1))  f

main :: IO ()
main =
  -- TBD Study scaling with 10, 100, 1000 loop iterations
  defaultMain
    [ bgroup "serially"
      [ benchIO "toNullAp"       $ Ops.toNullAp       serially
      , benchIO "toNull"         $ Ops.toNull         serially
      , benchIO "toNull3"        $ Ops.toNull3        serially
      , benchIO "toList"         $ Ops.toList         serially
   --   , benchIO "toListSome"     $ Ops.toListSome     serially
      , benchIO "filterAllOut"   $ Ops.filterAllOut   serially
      , benchIO "filterAllIn"    $ Ops.filterAllIn    serially
      , benchIO "filterSome"     $ Ops.filterSome     serially
      , benchIO "breakAfterSome" $ Ops.breakAfterSome serially
      ]

    , bgroup "wSerially"
      [ benchIO "toNullAp"       $ Ops.toNullAp       wSerially
      , benchIO "toNull"         $ Ops.toNull         wSerially
      , benchIO "toNull3"        $ Ops.toNull3        wSerially
      , benchIO "toList"         $ Ops.toList         wSerially
    --  , benchIO "toListSome"     $ Ops.toListSome     wSerially
      , benchIO "filterAllOut"   $ Ops.filterAllOut   wSerially
      , benchIO "filterAllIn"    $ Ops.filterAllIn    wSerially
      , benchIO "filterSome"     $ Ops.filterSome     wSerially
      , benchIO "breakAfterSome" $ Ops.breakAfterSome wSerially
      ]

    , bgroup "aheadly"
      [ benchIO "toNullAp"       $ Ops.toNullAp       aheadly
      , benchIO "toNull"         $ Ops.toNull         aheadly
      , benchIO "toNull3"        $ Ops.toNull3        aheadly
      , benchIO "toList"         $ Ops.toList         aheadly
     -- , benchIO "toListSome"     $ Ops.toListSome     aheadly
      , benchIO "filterAllOut"   $ Ops.filterAllOut   aheadly
      , benchIO "filterAllIn"    $ Ops.filterAllIn    aheadly
      , benchIO "filterSome"     $ Ops.filterSome     aheadly
      , benchIO "breakAfterSome" $ Ops.breakAfterSome aheadly
      ]

    , bgroup "asyncly"
      [ benchIO "toNullAp"       $ Ops.toNullAp       asyncly
      , benchIO "toNull"         $ Ops.toNull         asyncly
      , benchIO "toNull3"        $ Ops.toNull3        asyncly
      , benchIO "toList"         $ Ops.toList         asyncly
    --  , benchIO "toListSome"     $ Ops.toListSome     asyncly
      , benchIO "filterAllOut"   $ Ops.filterAllOut   asyncly
      , benchIO "filterAllIn"    $ Ops.filterAllIn    asyncly
      , benchIO "filterSome"     $ Ops.filterSome     asyncly
      , benchIO "breakAfterSome" $ Ops.breakAfterSome asyncly
      ]

    , bgroup "wAsyncly"
      [ benchIO "toNullAp"       $ Ops.toNullAp       wAsyncly
      , benchIO "toNull"         $ Ops.toNull         wAsyncly
      , benchIO "toNull3"        $ Ops.toNull3        wAsyncly
      , benchIO "toList"         $ Ops.toList         wAsyncly
     -- , benchIO "toListSome"     $ Ops.toListSome     wAsyncly
      , benchIO "filterAllOut"   $ Ops.filterAllOut   wAsyncly
      , benchIO "filterAllIn"    $ Ops.filterAllIn    wAsyncly
      , benchIO "filterSome"     $ Ops.filterSome     wAsyncly
      , benchIO "breakAfterSome" $ Ops.breakAfterSome wAsyncly
      ]

    , bgroup "parallely"
      [ benchIO "toNullAp"       $ Ops.toNullAp       parallely
      , benchIO "toNull"         $ Ops.toNull         parallely
      , benchIO "toNull3"        $ Ops.toNull3        parallely
      , benchIO "toList"         $ Ops.toList         parallely
      --, benchIO "toListSome"     $ Ops.toListSome     parallely
      , benchIO "filterAllOut"   $ Ops.filterAllOut   parallely
      , benchIO "filterAllIn"    $ Ops.filterAllIn    parallely
      , benchIO "filterSome"     $ Ops.filterSome     parallely
      , benchIO "breakAfterSome" $ Ops.breakAfterSome parallely
      ]
    ]
