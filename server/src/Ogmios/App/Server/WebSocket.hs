--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.App.Server.WebSocket
    ( -- * Constructor
      newWebSocketApp

      -- * Logging
    , TraceWebSocket (..)
    ) where

import Ogmios.Prelude

import Ogmios.App.Metrics
    ( Sensors (..), recordSession )
import Ogmios.App.Options
    ( NetworkParameters (..), Options (..) )
import Ogmios.App.Protocol
    ( onUnmatchedMessage )
import Ogmios.App.Protocol.ChainSync
    ( MaxInFlight, mkChainSyncClient )
import Ogmios.App.Protocol.StateQuery
    ( TraceStateQuery, mkStateQueryClient )
import Ogmios.App.Protocol.TxSubmission
    ( mkTxSubmissionClient )
import Ogmios.Control.Exception
    ( IOException, MonadCatch (..), MonadThrow (..) )
import Ogmios.Control.MonadAsync
    ( ExceptionInLinkedThread (..), MonadAsync (..), MonadLink, link )
import Ogmios.Control.MonadClock
    ( MonadClock (..) )
import Ogmios.Control.MonadLog
    ( HasSeverityAnnotation (..)
    , Logger
    , MonadLog (..)
    , Severity (..)
    , natTracer
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
    , SubProtocol
    , WebSocketApp
    , headers
    , subProtocols
    )
import Ogmios.Data.Json
    ( Json
    , SerializationMode (..)
    , ToJSON
    , encodeAcquireFailure
    , encodeBlock
    , encodePoint
    , encodeSubmitTxError
    , encodeTip
    , jsonToByteString
    )
import Ogmios.Data.Json.Orphans
    ()
import Ogmios.Data.Protocol.ChainSync
    ( ChainSyncCodecs (..), ChainSyncMessage (..), mkChainSyncCodecs )
import Ogmios.Data.Protocol.StateQuery
    ( StateQueryCodecs (..), StateQueryMessage (..), mkStateQueryCodecs )
import Ogmios.Data.Protocol.TxSubmission
    ( TxSubmissionCodecs (..), TxSubmissionMessage (..), mkTxSubmissionCodecs )

import Cardano.Network.Protocol.NodeToClient
    ( Block
    , Clients (..)
    , SubmitTxError
    , SubmitTxPayload
    , connectClient
    , mkClient
    )
import Cardano.Network.Protocol.NodeToClient.Trace
    ( TraceClient )
import Network.HTTP.Types.Header
    ( hUserAgent )
import Ouroboros.Network.NodeToClient.Version
    ( NodeToClientVersionData (NodeToClientVersionData) )
import System.TimeManager
    ( TimeoutThread (..) )

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
newWebSocketApp tr unliftIO = do
    NetworkParameters{slotsPerEpoch,networkMagic} <- asks (view typed)
    Options{nodeSocket,maxInFlight} <- asks (view typed)
    sensors <- asks (view typed)
    return $ \pending -> unliftIO $ do
        let (mode, sub) = choseSerializationMode pending
        logWith tr $ WebSocketConnectionAccepted (userAgent pending) mode
        recordSession sensors $ onExceptions $ acceptRequest pending sub $ \conn -> do
            let trClient = contramap WebSocketClient tr
            withOuroborosClients tr mode maxInFlight sensors conn $ \clients -> do
                let client = mkClient unliftIO (natTracer liftIO trClient) slotsPerEpoch clients
                let vData  = NodeToClientVersionData networkMagic
                connectClient trClient client vData nodeSocket
                    & handle (onIOException conn)
        logWith tr (WebSocketConnectionEnded $ userAgent pending)
  where
    userAgent :: PendingConnection -> Text
    userAgent pending = maybe "User-Agent unknown" (decodeUtf8 . snd)
        $ find ((== hUserAgent) . fst)
        $ headers pending

    onIOException :: Connection -> IOException -> m ()
    onIOException conn e = do
        logWith tr $ WebSocketFailedToConnect $ show e
        let msg = "Connection with the node lost or failed."
        close conn $ toStrict $ Json.encode $ Wsp.serverFault Nothing msg

    onExceptions :: m () -> m ()
    onExceptions
        = handle onUnknownException
        . handle onTimeoutThread
        . handle onConnectionClosed

    onUnknownException :: SomeException -> m ()
    onUnknownException e0 = case fromException e0 of
        Nothing -> do
            logWith tr $ WebSocketUnknownException $ show e0
        Just (ExceptionInLinkedThread _ e1) -> do
            case fromException e1 of
                Nothing ->
                    logWith tr $ WebSocketWorkerExited $ show e1
                Just e2 ->
                    onConnectionClosed e2

    onTimeoutThread :: TimeoutThread -> m ()
    onTimeoutThread = \case
        TimeoutThread -> pure ()

    onConnectionClosed :: ConnectionException -> m ()
    onConnectionClosed = \case
        CloseRequest{} ->
            pure ()
        ConnectionClosed{} ->
            pure ()
        e ->
            throwIO e

-- | Chose a serialization mode based on the sub-protocols given by the client
-- websocket. If the client specifies "compact" as a sub-protocol, then Ogmios
-- will not return proofs, signatures and other voluminous data from the
-- chain-sync protocol.
choseSerializationMode
    :: PendingConnection
    -> (SerializationMode, Maybe SubProtocol)
choseSerializationMode conn =
    case subProtocols conn of
        sub:_ | sub == compact -> (CompactSerialization, Just compact)
        sub:_ | sub == full -> (FullSerialization, Nothing)
        _ -> (FullSerialization, Nothing)
  where
    full = "ogmios.v1"
    compact = full<>":compact"

withOuroborosClients
    :: forall m a.
        ( MonadAsync m
        , MonadLink m
        , MonadLog m
        , MonadMetrics m
        , MonadOuroboros m
        , MonadWebSocket m
        )
    => Logger TraceWebSocket
    -> SerializationMode
    -> MaxInFlight
    -> Sensors m
    -> Connection
    -> (Clients m Block -> m a)
    -> m a
withOuroborosClients tr mode maxInFlight sensors conn action = do
    (chainSyncQ, txSubmissionQ, stateQueryQ) <-
        atomically $ (,,) <$> newTQueue <*> newTQueue <*> newTQueue

    withAsync (routeMessage Nothing chainSyncQ stateQueryQ txSubmissionQ) $ \worker -> do
        link worker
        action $ Clients
             { chainSyncClient =
                 mkChainSyncClient maxInFlight chainSyncCodecs chainSyncQ yield
             , stateQueryClient =
                 mkStateQueryClient (contramap WebSocketStateQuery tr) stateQueryCodecs stateQueryQ yield
             , txSubmissionClient =
                 mkTxSubmissionClient txSubmissionCodecs txSubmissionQ yield
             }
  where
    yield :: Json -> m ()
    yield = send conn . jsonToByteString

    push :: TQueue m any -> any -> m ()
    push queue = atomically . writeTQueue queue

    defaultHandler :: ByteString -> m ()
    defaultHandler = yield . onUnmatchedMessage @Block

    routeMessage
        :: Maybe (ByteString, m ())
        -> TQueue m (ChainSyncMessage Block)
        -> TQueue m (StateQueryMessage Block)
        -> TQueue m (TxSubmissionMessage Block)
        -> m ()
    routeMessage cache chainSyncQ stateQueryQ txSubmissionQ = do
        count (totalMessagesCounter sensors)
        bytes <- receive conn
        case cache of
            Just (prev, again) | prev == bytes ->
                again *> routeMessage cache chainSyncQ stateQueryQ txSubmissionQ

            _ -> do
                matched <- Wsp.match bytes
                    (count (totalUnroutedCounter sensors) *> defaultHandler bytes)
                    [ Wsp.Handler decodeRequestNext
                        (\r t -> push chainSyncQ . MsgRequestNext r t)
                    , Wsp.Handler decodeFindIntersect
                        (\r t -> push chainSyncQ . MsgFindIntersect r t)

                    , Wsp.Handler decodeAcquire
                        (\r t -> push stateQueryQ . MsgAcquire r t)
                    , Wsp.Handler decodeRelease
                        (\r t -> push stateQueryQ . MsgRelease r t)
                    , Wsp.Handler decodeQuery
                        (\r t -> push stateQueryQ . MsgQuery r t)

                    , Wsp.Handler decodeSubmitTx
                        (\r t -> push txSubmissionQ .  MsgSubmitTx r t)
                    ]
                routeMessage matched chainSyncQ stateQueryQ txSubmissionQ

    chainSyncCodecs@ChainSyncCodecs
        { decodeFindIntersect
        , decodeRequestNext
        } = mkChainSyncCodecs (encodeBlock mode) encodePoint encodeTip

    stateQueryCodecs@StateQueryCodecs
        { decodeAcquire
        , decodeRelease
        , decodeQuery
        } = mkStateQueryCodecs encodePoint encodeAcquireFailure

    txSubmissionCodecs@TxSubmissionCodecs
        { decodeSubmitTx
        } = mkTxSubmissionCodecs encodeSubmitTxError

--
-- Logging
--

data TraceWebSocket where
    WebSocketClient
        :: TraceClient (SubmitTxPayload Block) (SubmitTxError Block)
        -> TraceWebSocket

    WebSocketStateQuery
        :: TraceStateQuery Block
        -> TraceWebSocket

    WebSocketWorkerExited
        :: { exception :: Text }
        -> TraceWebSocket

    WebSocketConnectionAccepted
        :: { userAgent :: Text, mode :: SerializationMode }
        -> TraceWebSocket

    WebSocketConnectionEnded
        :: { userAgent :: Text }
        -> TraceWebSocket

    WebSocketUnknownException
        :: { exception :: Text }
        -> TraceWebSocket

    WebSocketFailedToConnect
        :: { ioException :: Text }
        -> TraceWebSocket
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

instance HasSeverityAnnotation TraceWebSocket where
    getSeverityAnnotation = \case
        WebSocketClient msg           -> getSeverityAnnotation msg
        WebSocketStateQuery msg       -> getSeverityAnnotation msg
        WebSocketWorkerExited{}       -> Debug
        WebSocketConnectionAccepted{} -> Info
        WebSocketConnectionEnded{}    -> Info
        WebSocketUnknownException{}   -> Error
        WebSocketFailedToConnect{}    -> Error
