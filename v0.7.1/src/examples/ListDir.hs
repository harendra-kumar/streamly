module Main (main) where

import Data.Bifunctor (bimap)
import Data.Function ((&))
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import Streamly (ahead)

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.FileSystem.Dir as Dir

-- | List the current directory recursively using concurrent processing
--
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    S.mapM_ print $ S.concatMapTreeWith ahead listDir
        (S.yieldM $ return (Left "."))

    where

    listDir dir =
          Dir.toEither dir            -- SerialT IO (Either String String)
        & S.map (bimap prefix prefix) -- SerialT IO (Either String String)
        where prefix x = dir ++ "/" ++ x
