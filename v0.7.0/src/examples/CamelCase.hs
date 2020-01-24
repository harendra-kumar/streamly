-- ghc -O2  -fspec-constr-recursive=10 -fmax-worker-args=16
-- Convert the input file to camel case and write to stdout

import Data.Maybe (fromJust, isJust)
import System.Environment (getArgs)
import System.IO (Handle, IOMode(..), openFile, stdout)

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.FileSystem.Handle as FH

camelCase :: Handle -> Handle -> IO ()
camelCase src dst =
      FH.fromBytes dst
    $ S.map fromJust
    $ S.filter isJust
    $ S.map snd
    $ S.scanl' step (True, Nothing)
    $ FH.toBytes src

    where

    step (wasSpace, _) x =
        if x == 0x0a || x >= 0x41 && x <= 0x5a
        then (False, Just x)
        else if x >= 0x61 && x <= 0x7a
             then (False, Just $ if wasSpace then x - 32 else x)
             else (True, Nothing)

main :: IO ()
main = do
    name <- fmap head getArgs
    src <- openFile name ReadMode
    camelCase src stdout
