import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import Streamly (aheadly, ahead, AheadT)
import Data.Function ((&))

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.FileSystem.Dir as Dir

-- | List the current directory recursively using concurrent processing
--
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    S.mapM_ print $ aheadly $ recursePath (Left ".")

    where

    -- XXX Fix bug in enqueueAhead when mixing serial with ahead:
    -- (\x -> S.yield dir <> S.concatMapWith ahead recursePath x)
    --  :: SerialT IO String
    -- or S.cons dir . S.concatMapWith ahead recursePath
    recursePath :: Either String String -> AheadT IO String
    recursePath (Left dir) =
          Dir.toEither dir                  -- SerialT IO (Either String String)
        & S.map (prefixDir dir)             -- SerialT IO (Either String String)
        & S.consM (return dir)
        . S.concatMapWith ahead recursePath -- SerialT IO String
    recursePath (Right file) = S.yield file -- SerialT IO String

    prefixDir :: String -> Either String String -> Either String String
    prefixDir dir (Right x) = Right $ dir ++ "/" ++ x
    prefixDir dir (Left x)  = Left  $ dir ++ "/" ++ x
