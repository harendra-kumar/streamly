#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Network.Inet.TCP
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators to build Inet/TCP clients and servers.

module Streamly.Internal.Network.Inet.TCP
    (
    -- * TCP Servers
    -- ** Unfolds
      acceptOnAddr
    , acceptOnAddrWith
    , acceptOnPort
    , acceptOnPortWith
    , acceptOnPortLocal

    -- ** Streams
    , connectionsOnAddr
    , connectionsOnAddrWith
    , connectionsOnPort
    , connectionsOnLocalHost

    -- * TCP clients
    -- | IP Address based operations.
    , connect
    , withConnectionM

    -- ** Unfolds
    , usingConnection
    , read

    -- ** Streams
    , withConnection
    -- *** Source
    , toBytes
    -- , readUtf8
    -- , readLines
    -- , readFrames
    -- , readByChunks

    -- -- * Array Read
    -- , readArrayUpto
    -- , readArrayOf

    -- , readChunksUpto
    -- , readChunksOf
    -- , readChunks

    -- *** Sink
    , write
    -- , writeUtf8
    -- , writeUtf8ByLines
    -- , writeByFrames
    , writeWithBufferOf
    , fromBytes
    , fromBytesWithBufferOf

    -- -- * Array Write
    -- , writeArray
    , writeChunks
    , fromChunks

    -- ** Transformation
    , transformBytesWith
    {-
    -- ** Sink Servers

    -- These abstractions can be applied to any setting where we need to do a
    -- sink processing of multiple streams e.g. output from multiple processes
    -- or data coming from multiple files.

    -- handle connections concurrently using a specified fold
    -- , handleConnections

    -- handle frames concurrently using a specified fold
    , handleFrames

    -- merge frames from all connection into a single stream. Frames can be
    -- created by a specified fold.
    , mergeFrames

    -- * UDP Servers
    , datagrams
    , datagramsOn
    -}
    )
where

import Control.Exception (onException)
import Control.Monad.Catch (MonadCatch, MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Network.Socket
       (Socket, PortNumber, SocketOption(..), Family(..), SockAddr(..),
        SocketType(..), defaultProtocol, maxListenQueue, tupleToHostAddress,
        socket)
import Prelude hiding (read)

import Streamly.Internal.Data.Array.Foreign.Types (Array(..), defaultChunkSize, writeNUnsafe)
import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.SVar (fork)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Streamly.Internal.Data.Unfold.Types (Unfold(..))
import Streamly.Internal.Network.Socket (SockSpec(..), accept, connections)
import Streamly.Prelude (MonadAsync)

import qualified Control.Monad.Catch as MC
import qualified Network.Socket as Net

import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Array.Stream.Foreign as AS
import qualified Streamly.Internal.Data.Fold.Types as FL
import qualified Streamly.Prelude as S
import qualified Streamly.Network.Socket as SK
import qualified Streamly.Internal.Network.Socket as ISK

-------------------------------------------------------------------------------
-- Accept (unfolds)
-------------------------------------------------------------------------------

{-# INLINE acceptOnAddrWith #-}
acceptOnAddrWith
    :: MonadIO m
    => [(SocketOption, Int)]
    -> Unfold m ((Word8, Word8, Word8, Word8), PortNumber) Socket
acceptOnAddrWith opts = UF.lmap f accept
    where
    f (addr, port) =
        (maxListenQueue
        , SockSpec
            { sockFamily = AF_INET
            , sockType = Stream
            , sockProto = defaultProtocol -- TCP
            , sockOpts = opts
            }
        , SockAddrInet port (tupleToHostAddress addr)
        )

-- | Unfold a tuple @(ipAddr, port)@ into a stream of connected TCP sockets.
-- @ipAddr@ is the local IP address and @port@ is the local port on which
-- connections are accepted.
--
-- @since 0.7.0
{-# INLINE acceptOnAddr #-}
acceptOnAddr
    :: MonadIO m
    => Unfold m ((Word8, Word8, Word8, Word8), PortNumber) Socket
acceptOnAddr = acceptOnAddrWith []

{-# INLINE acceptOnPortWith #-}
acceptOnPortWith :: MonadIO m
    => [(SocketOption, Int)]
    -> Unfold m PortNumber Socket
acceptOnPortWith opts = UF.supplyFirst (acceptOnAddrWith opts) (0,0,0,0)

-- | Like 'acceptOnAddr' but binds on the IPv4 address @0.0.0.0@ i.e.  on all
-- IPv4 addresses/interfaces of the machine and listens for TCP connections on
-- the specified port.
--
-- > acceptOnPort = UF.supplyFirst acceptOnAddr (0,0,0,0)
--
-- @since 0.7.0
{-# INLINE acceptOnPort #-}
acceptOnPort :: MonadIO m => Unfold m PortNumber Socket
acceptOnPort = UF.supplyFirst acceptOnAddr (0,0,0,0)

-- | Like 'acceptOnAddr' but binds on the localhost IPv4 address @127.0.0.1@.
-- The server can only be accessed from the local host, it cannot be accessed
-- from other hosts on the network.
--
-- > acceptOnPortLocal = UF.supplyFirst acceptOnAddr (127,0,0,1)
--
-- @since 0.7.0
{-# INLINE acceptOnPortLocal #-}
acceptOnPortLocal :: MonadIO m => Unfold m PortNumber Socket
acceptOnPortLocal = UF.supplyFirst acceptOnAddr (127,0,0,1)

-------------------------------------------------------------------------------
-- Accept (streams)
-------------------------------------------------------------------------------

{-# INLINE connectionsOnAddrWith #-}
connectionsOnAddrWith
    :: MonadAsync m
    => [(SocketOption, Int)]
    -> (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> SerialT m Socket
connectionsOnAddrWith opts addr port =
    connections maxListenQueue SockSpec
        { sockFamily = AF_INET
        , sockType = Stream
        , sockProto = defaultProtocol
        , sockOpts = opts
        }
        (SockAddrInet port (tupleToHostAddress addr))

-- | Like 'connections' but binds on the specified IPv4 address of the machine
-- and listens for TCP connections on the specified port.
--
-- /Pre-release/
{-# INLINE connectionsOnAddr #-}
connectionsOnAddr
    :: MonadAsync m
    => (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> SerialT m Socket
connectionsOnAddr = connectionsOnAddrWith []

-- | Like 'connections' but binds on the IPv4 address @0.0.0.0@ i.e.  on all
-- IPv4 addresses/interfaces of the machine and listens for TCP connections on
-- the specified port.
--
-- > connectionsOnPort = connectionsOnAddr (0,0,0,0)
--
-- /Pre-release/
{-# INLINE connectionsOnPort #-}
connectionsOnPort :: MonadAsync m => PortNumber -> SerialT m Socket
connectionsOnPort = connectionsOnAddr (0,0,0,0)

-- | Like 'connections' but binds on the localhost IPv4 address @127.0.0.1@.
-- The server can only be accessed from the local host, it cannot be accessed
-- from other hosts on the network.
--
-- > connectionsOnLocalHost = connectionsOnAddr (127,0,0,1)
--
-- /Pre-release/
{-# INLINE connectionsOnLocalHost #-}
connectionsOnLocalHost :: MonadAsync m => PortNumber -> SerialT m Socket
connectionsOnLocalHost = connectionsOnAddr (127,0,0,1)

-------------------------------------------------------------------------------
-- TCP Clients
-------------------------------------------------------------------------------

-- | Connect to the specified IP address and port number. Returns a connected
-- socket or throws an exception.
--
-- @since 0.7.0
connect :: (Word8, Word8, Word8, Word8) -> PortNumber -> IO Socket
connect addr port = do
    sock <- socket AF_INET Stream defaultProtocol
    Net.connect sock (SockAddrInet port (Net.tupleToHostAddress addr))
        `onException` Net.close sock
    return sock

-- | Connect to a remote host using IP address and port and run the supplied
-- action on the resulting socket.  'withConnectionM' makes sure that the
-- socket is closed on normal termination or in case of an exception.  If
-- closing the socket raises an exception, then this exception will be raised
-- by 'withConnectionM'.
--
-- /Pre-release/
{-# INLINABLE withConnectionM #-}
withConnectionM :: (MonadMask m, MonadIO m)
    => (Word8, Word8, Word8, Word8) -> PortNumber -> (Socket -> m ()) -> m ()
withConnectionM addr port =
    bracket (liftIO $ connect addr port) (liftIO . Net.close)

-------------------------------------------------------------------------------
-- Connect (unfolds)
-------------------------------------------------------------------------------

-- | Transform an 'Unfold' from a 'Socket' to an unfold from a remote IP
-- address and port. The resulting unfold opens a socket, uses it using the
-- supplied unfold and then makes sure that the socket is closed on normal
-- termination or in case of an exception.  If closing the socket raises an
-- exception, then this exception will be raised by 'usingConnection'.
--
-- /Pre-release/
{-# INLINABLE usingConnection #-}
usingConnection :: (MonadCatch m, MonadAsync m)
    => Unfold m Socket a
    -> Unfold m ((Word8, Word8, Word8, Word8), PortNumber) a
usingConnection =
    UF.bracket (\(addr, port) -> liftIO $ connect addr port)
               (liftIO . Net.close)

-------------------------------------------------------------------------------
-- Connect (streams)
-------------------------------------------------------------------------------

-- | @'withConnection' addr port act@ opens a connection to the specified IPv4
-- host address and port and passes the resulting socket handle to the
-- computation @act@.  The handle will be closed on exit from 'withConnection',
-- whether by normal termination or by raising an exception.  If closing the
-- handle raises an exception, then this exception will be raised by
-- 'withConnection' rather than any exception raised by 'act'.
--
-- /Pre-release/
{-# INLINABLE withConnection #-}
withConnection :: (IsStream t, MonadCatch m, MonadAsync m)
    => (Word8, Word8, Word8, Word8) -> PortNumber -> (Socket -> t m a) -> t m a
withConnection addr port =
    S.bracket (liftIO $ connect addr port) (liftIO . Net.close)

-------------------------------------------------------------------------------
-- Read Addr to Stream
-------------------------------------------------------------------------------

-- | Read a stream from the supplied IPv4 host address and port number.
--
-- @since 0.7.0
{-# INLINE read #-}
read :: (MonadCatch m, MonadAsync m)
    => Unfold m ((Word8, Word8, Word8, Word8), PortNumber) Word8
read = UF.concat (usingConnection ISK.readChunks) A.read

-- | Read a stream from the supplied IPv4 host address and port number.
--
-- @since 0.7.0
{-# INLINE toBytes #-}
toBytes :: (IsStream t, MonadCatch m, MonadAsync m)
    => (Word8, Word8, Word8, Word8) -> PortNumber -> t m Word8
toBytes addr port = AS.concat $ withConnection addr port ISK.toChunks

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

-- | Write a stream of arrays to the supplied IPv4 host address and port
-- number.
--
-- @since 0.7.0
{-# INLINE fromChunks #-}
fromChunks
    :: (MonadCatch m, MonadAsync m)
    => (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> SerialT m (Array Word8)
    -> m ()
fromChunks addr port xs =
    S.drain $ withConnection addr port (\sk -> S.valueM $ ISK.fromChunks sk xs)

-- | Write a stream of arrays to the supplied IPv4 host address and port
-- number.
--
-- @since 0.7.0
{-# INLINE writeChunks #-}
writeChunks
    :: (MonadAsync m, MonadCatch m)
    => (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> Fold m (Array Word8) ()
writeChunks addr port = Fold step initial extract
    where
    initial = do
        skt <- liftIO (connect addr port)
        fld <- FL.initialize (SK.writeChunks skt) `MC.onException` liftIO (Net.close skt)
        return $ FL.Partial (Tuple' fld skt)
    step (Tuple' fld skt) x = do
        r <- FL.runStep fld x `MC.onException` liftIO (Net.close skt)
        return $ FL.Partial (Tuple' r skt)
    extract (Tuple' (Fold _ initial1 extract1) skt) = do
        liftIO $ Net.close skt
        res <- initial1
        case res of
            FL.Partial fs -> extract1 fs
            FL.Done fb -> return fb

-- | Like 'write' but provides control over the write buffer. Output will
-- be written to the IO device as soon as we collect the specified number of
-- input elements.
--
-- @since 0.7.0
{-# INLINE fromBytesWithBufferOf #-}
fromBytesWithBufferOf
    :: (MonadCatch m, MonadAsync m)
    => Int
    -> (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> SerialT m Word8
    -> m ()
fromBytesWithBufferOf n addr port m = fromChunks addr port $ AS.arraysOf n m

-- | Like 'write' but provides control over the write buffer. Output will
-- be written to the IO device as soon as we collect the specified number of
-- input elements.
--
-- @since 0.7.0
{-# INLINE writeWithBufferOf #-}
writeWithBufferOf
    :: (MonadAsync m, MonadCatch m)
    => Int
    -> (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> Fold m Word8 ()
writeWithBufferOf n addr port =
    FL.chunksOf n (writeNUnsafe n) (writeChunks addr port)

-- | Write a stream to the supplied IPv4 host address and port number.
--
-- @since 0.7.0
{-# INLINE fromBytes #-}
fromBytes :: (MonadCatch m, MonadAsync m)
    => (Word8, Word8, Word8, Word8) -> PortNumber -> SerialT m Word8 -> m ()
fromBytes = fromBytesWithBufferOf defaultChunkSize

-- | Write a stream to the supplied IPv4 host address and port number.
--
-- @since 0.7.0
{-# INLINE write #-}
write :: (MonadAsync m, MonadCatch m)
    => (Word8, Word8, Word8, Word8) -> PortNumber -> Fold m Word8 ()
write = writeWithBufferOf defaultChunkSize

-------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------

{-# INLINABLE withInputConnect #-}
withInputConnect
    :: (IsStream t, MonadCatch m, MonadAsync m)
    => (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> SerialT m Word8
    -> (Socket -> t m a)
    -> t m a
withInputConnect addr port input f = S.bracket pre post handler

    where

    pre = do
        sk <- liftIO $ connect addr port
        tid <- fork (ISK.fromBytes sk input)
        return (sk, tid)

    handler (sk, _) = f sk

    -- XXX kill the thread immediately?
    post (sk, _) = liftIO $ Net.close sk

-- | Send an input stream to a remote host and produce the output stream from
-- the host. The server host just acts as a transformation function on the
-- input stream.  Both sending and receiving happen asynchronously.
--
-- /Pre-release/
--
{-# INLINABLE transformBytesWith #-}
transformBytesWith
    :: (IsStream t, MonadAsync m, MonadCatch m)
    => (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> SerialT m Word8
    -> t m Word8
transformBytesWith addr port input =
    withInputConnect addr port input ISK.toBytes
