--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.App.Server.WebSocket
    ( -- * Constructor
      newWebSocketApp

      -- * Logging
    , TraceWebSocket (..)
    ) where

import Relude hiding
    ( atomically )

import Ogmios.App.Metrics
    ( Sensors, recordSession )
import Ogmios.App.Options
    ( NetworkParameters (..), Options (..) )
import Ogmios.App.Protocol.ChainSync
    ( mkChainSyncClient )
import Ogmios.Control.Exception
    ( Exception (..)
    , IOException
    , MonadCatch (..)
    , MonadThrow (..)
    , SomeException
    )
import Ogmios.Control.MonadAsync
    ( ExceptionInLinkedThread (..), MonadAsync (..), MonadLink, link )
import Ogmios.Control.MonadClock
    ( MonadClock (..), idle )
import Ogmios.Control.MonadLog
    ( HasSeverityAnnotation (..)
    , Logger
    , MonadLog (..)
    , Severity (..)
    , natTracer
    , nullTracer
    )
import Ogmios.Control.MonadMetrics
    ( MonadMetrics (..) )
import Ogmios.Control.MonadOuroboros
    ( MonadOuroboros )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..), TQueue, newTQueue, writeTQueue )
import Ogmios.Control.MonadWebSocket
    ( Connection
    , ConnectionException (..)
    , MonadWebSocket (..)
    , PendingConnection
    , WebSocketApp
    , headers
    )
import Ogmios.Data.Json
    ()

import Cardano.Network.Protocol.NodeToClient
    ( ApplyErr, Block, Clients (..), connectClient, mkClient )
import Cardano.Network.Protocol.NodeToClient.Trace
    ( TraceClient )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Jsonifier
    ( Json )
import Network.HTTP.Types.Header
    ( hUserAgent )
import Ouroboros.Consensus.Byron.Ledger
    ( GenTx )
import Ouroboros.Network.NodeToClient.Version
    ( NodeToClientVersionData (NodeToClientVersionData) )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( LocalStateQueryClient (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxSubmissionClient (..) )

import qualified Codec.Json.Wsp.Handler as Wsp
import qualified Data.Aeson as Json

--
-- WebSocketApp
--

newWebSocketApp
    :: forall m env.
        ( MonadIO m -- Needed by 'connectClient'
        , MonadClock m
        , MonadLink m
        , MonadMetrics m
        , MonadOuroboros m
        , MonadWebSocket m
        , MonadReader env m
        , MonadLog m
        , HasType NetworkParameters env
        , HasType Options env
        , HasType (Sensors m) env
        )
    => Logger TraceWebSocket
    -> (forall a. m a -> IO a)
    -> m WebSocketApp
newWebSocketApp tr embed = do
    NetworkParameters{slotsPerEpoch,networkMagic} <- asks (view typed)
    Options{nodeSocket} <- asks (view typed)
    sensors <- asks (view typed)
    return $ \pending -> embed $ do
        logWith tr (WebSocketConnectionAccepted $ userAgent pending)
        acceptRequest pending $ \conn -> do
            let trClient = natTracer liftIO $ contramap WebSocketClient tr
            client <- mkClient embed trClient slotsPerEpoch <$> newOuroborosClients conn
            connectClient nullTracer client (NodeToClientVersionData networkMagic) nodeSocket
                & onExceptions conn
                & recordSession sensors
        logWith tr (WebSocketConnectionEnded $ userAgent pending)
  where
    userAgent :: PendingConnection -> ByteString
    userAgent pending = maybe "User-Agent unknown" snd
        $ find ((== hUserAgent) . fst)
        $ headers pending

    onExceptions :: Connection -> m () -> m ()
    onExceptions conn
        = handle onUnknownException
        . handle onConnectionClosed
        . handle onLinkedException
        . handle (onIOException conn)

    onUnknownException :: SomeException -> m ()
    onUnknownException e = do
        logWith tr $ WebSocketUnknownException e

    onConnectionClosed :: ConnectionException -> m ()
    onConnectionClosed = \case
        CloseRequest{} ->
            pure ()
        ConnectionClosed{} ->
            pure ()
        e ->
            throwIO e

    onLinkedException :: ExceptionInLinkedThread -> m ()
    onLinkedException = \case
        ExceptionInLinkedThread _ e -> case fromException e of
            Just e' -> onConnectionClosed e'
            Nothing -> throwIO e

    onIOException :: Connection -> IOException -> m ()
    onIOException conn e = do
        logWith tr $ WebSocketFailedToConnect e
        let msg = "Connection with the node lost or failed."
        close conn $ toStrict $ Json.encode $ Wsp.serverFault msg

-- | FIXME: instantiate the right client based on the client's request
newOuroborosClients
    :: forall m.
        ( MonadAsync m
        , MonadClock m
        , MonadLink m
        , MonadWebSocket m
        )
    => Connection
    -> m (Clients m Block (GenTx Block) ApplyErr)
newOuroborosClients conn = do
    let yield = send conn . BL.toStrict . Json.encodingToLazyByteString
    queue <- atomically newTQueue
    link =<< async (queue `feedWith` conn)
    return Clients
        { chainSyncClient = mkChainSyncClient queue yield
        , txSubmissionClient = LocalTxSubmissionClient idle
        , stateQueryClient = LocalStateQueryClient idle
        }
  where
    feedWith :: TQueue m ByteString -> Connection -> m ()
    feedWith queue source =
        forever $ receive source >>= atomically . writeTQueue queue

--
-- Logging
--

data TraceWebSocket where
    WebSocketClient
        :: TraceClient (GenTx Block) ApplyErr
        -> TraceWebSocket

    WebSocketConnectionAccepted
        :: { userAgent :: ByteString }
        -> TraceWebSocket

    WebSocketConnectionEnded
        :: { userAgent :: ByteString }
        -> TraceWebSocket

    WebSocketFailedToConnect
        :: { ioException :: IOException }
        -> TraceWebSocket

    WebSocketUnknownException
        :: { exception :: SomeException }
        -> TraceWebSocket

deriving instance Show TraceWebSocket

instance HasSeverityAnnotation TraceWebSocket where
    getSeverityAnnotation = \case
        WebSocketClient msg -> getSeverityAnnotation msg

        WebSocketConnectionAccepted{} -> Info
        WebSocketConnectionEnded{}    -> Info
        WebSocketFailedToConnect{}    -> Error
        WebSocketUnknownException{}   -> Error
