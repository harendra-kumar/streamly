-- A concurrent TCP server that:
--
-- * receives connections from clients
-- * splits the incoming data into lines
-- * lines from concurrent connections are merged into a single srteam
-- * writes the line stream to an output file

import Control.Monad.IO.Class (liftIO)
import Network.Socket (close)
import System.Environment (getArgs)

import Streamly
import Streamly.Data.Unicode.Stream
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Memory.Array as A
import qualified Streamly.Network.Socket as NS
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Prelude as S

import System.IO (withFile, IOMode(..))

main :: IO ()
main = do
    file <- fmap head getArgs
    withFile file AppendMode
        (\src -> S.fold (FH.write src)
        $ encodeLatin1Lax
        $ S.concatUnfold A.read
        $ S.concatMapWith parallel use
        $ S.unfold TCP.acceptOnPort 8090)

    where

    use sk = S.finally (liftIO $ close sk) (recv sk)
    recv =
          S.splitWithSuffix (== '\n') A.write
        . decodeLatin1
        . S.unfold NS.read
