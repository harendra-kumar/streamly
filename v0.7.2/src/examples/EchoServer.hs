-- A concurrent TCP server that echoes everything that it receives.

import Data.Function ((&))

import Streamly
import Streamly.Internal.Network.Socket (handleWithM)
import Streamly.Network.Socket

import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Prelude as S

main :: IO ()
main =
      serially (S.unfold TCP.acceptOnPort 8091)
    & parallely . S.mapM (handleWithM echo)
    & S.drain

    where

    echo sk =
          S.unfold readChunksWithBufferOf (32768, sk) -- SerialT IO Socket
        & S.fold (writeChunks sk)                     -- IO ()
