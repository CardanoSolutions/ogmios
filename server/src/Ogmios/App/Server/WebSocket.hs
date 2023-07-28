--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.App.Server.WebSocket
    ( -- * Constructor
      newWebSocketApp

      -- * Logging
    , TraceWebSocket (..)
    ) where

import Ogmios.Prelude

import Cardano.Network.Protocol.NodeToClient
    ( Block
    , Clients (..)
    , SerializedTransaction
    , SubmitTransactionError
    , connectClient
    , mkClient
    )
import Cardano.Network.Protocol.NodeToClient.Trace
    ( TraceClient
    )
import Network.HTTP.Types.Header
    ( hUserAgent
    )
import Ogmios.App.Configuration
    ( Configuration (..)
    , NetworkParameters (..)
    , readAlonzoGenesis
    , readByronGenesis
    , readConwayGenesis
    , readShelleyGenesis
    )
import Ogmios.App.Metrics
    ( Sensors (..)
    , recordSession
    )
import Ogmios.App.Protocol
    ( onUnmatchedMessage
    )
import Ogmios.App.Protocol.ChainSync
    ( MaxInFlight
    , mkChainSyncClient
    )
import Ogmios.App.Protocol.StateQuery
    ( TraceStateQuery
    , mkStateQueryClient
    )
import Ogmios.App.Protocol.TxMonitor
    ( mkTxMonitorClient
    )
import Ogmios.App.Protocol.TxSubmission
    ( ExecutionUnitsEvaluator
    , mkTxSubmissionClient
    , newExecutionUnitsEvaluator
    )
import Ogmios.Control.Exception
    ( IOException
    , MonadCatch (..)
    , MonadThrow (..)
    )
import Ogmios.Control.MonadAsync
    ( ExceptionInLinkedThread (..)
    , MonadAsync (..)
    , MonadLink
    , link
    )
import Ogmios.Control.MonadClock
    ( MonadClock (..)
    , idle
    )
import Ogmios.Control.MonadLog
    ( HasSeverityAnnotation (..)
    , Logger
    , MonadLog (..)
    , Severity (..)
    , getSeverityAnnotation'
    , natTracer
    )
import Ogmios.Control.MonadMetrics
    ( MonadMetrics (..)
    )
import Ogmios.Control.MonadOuroboros
    ( MonadOuroboros
    )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..)
    , TQueue
    , newTQueue
    , writeTQueue
    )
import Ogmios.Control.MonadWebSocket
    ( Connection
    , ConnectionException (..)
    , MonadWebSocket (..)
    , PendingConnection
    , WebSocketApp
    , headers
    )
import Ogmios.Data.Json
    ( Json
    , ToJSON
    , encodeAcquireExpired
    , encodeAcquireFailure
    , encodeBlock
    , encodeDeserialisationFailure
    , encodeExUnits
    , encodePoint
    , encodeRdmrPtr
    , encodeScriptFailure
    , encodeSubmitTransactionError
    , encodeTip
    , encodeTranslationError
    , encodeTx
    , encodeTxId
    , encodeTxIn
    , jsonToByteString
    )
import Ogmios.Data.Json.Orphans
    ()
import Ogmios.Data.Protocol.ChainSync
    ( ChainSyncCodecs (..)
    , ChainSyncMessage (..)
    , mkChainSyncCodecs
    )
import Ogmios.Data.Protocol.StateQuery
    ( GetGenesisConfig (..)
    , StateQueryCodecs (..)
    , StateQueryMessage (..)
    , mkStateQueryCodecs
    )
import Ogmios.Data.Protocol.TxMonitor
    ( TxMonitorCodecs (..)
    , TxMonitorMessage (..)
    , mkTxMonitorCodecs
    )
import Ogmios.Data.Protocol.TxSubmission
    ( TxSubmissionCodecs (..)
    , TxSubmissionMessage (..)
    , mkTxSubmissionCodecs
    )
import Ouroboros.Network.NodeToClient.Version
    ( NodeToClientVersionData (NodeToClientVersionData)
    )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..)
    )
import Ouroboros.Network.Protocol.LocalTxMonitor.Client
    ( LocalTxMonitorClient (..)
    )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxSubmissionClient (..)
    )
import System.TimeManager
    ( TimeoutThread (..)
    )

import qualified Codec.Json.Rpc.Handler as Rpc
import qualified Data.Aeson as Json

--
-- WebSocketApp
--

newWebSocketApp
    :: forall m env.
        ( MonadIO m -- Needed by 'connectClient' & for reading genesis configurations
        , MonadClock m
        , MonadLink m
        , MonadMetrics m
        , MonadOuroboros m
        , MonadWebSocket m
        , MonadReader env m
        , MonadLog m
        , HasType NetworkParameters env
        , HasType Configuration env
        , HasType (Sensors m) env
        )
    => Logger TraceWebSocket
    -> (forall a. m a -> IO a)
    -> m WebSocketApp
newWebSocketApp tr unliftIO = do
    NetworkParameters{slotsPerEpoch,networkMagic} <- asks (view typed)
    Configuration{nodeSocket,maxInFlight,nodeConfig} <- asks (view typed)
    sensors <- asks (view typed)
    let getGenesisConfig = GetGenesisConfig
            { getByronGenesis = readByronGenesis nodeConfig
            , getShelleyGenesis = readShelleyGenesis nodeConfig
            , getAlonzoGenesis = readAlonzoGenesis nodeConfig
            , getConwayGenesis = readConwayGenesis nodeConfig
            }
    return $ \pending -> unliftIO $ do
        logWith tr $ WebSocketConnectionAccepted (userAgent pending)
        recordSession sensors $ onExceptions $ acceptRequest pending $ \conn -> do
            let trClient = contramap WebSocketClient tr
            withExecutionUnitsEvaluator $ \exUnitsEvaluator exUnitsClients -> do
                withOuroborosClients tr maxInFlight sensors exUnitsEvaluator getGenesisConfig conn $ \protocolsClients -> do
                    let clientA = mkClient unliftIO (natTracer liftIO trClient) slotsPerEpoch protocolsClients
                    let clientB = mkClient unliftIO (natTracer liftIO trClient) slotsPerEpoch exUnitsClients
                    let vData  = NodeToClientVersionData networkMagic False
                    concurrently_
                        (connectClient trClient clientA vData nodeSocket)
                        (connectClient trClient clientB vData nodeSocket)
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
        close conn $ toStrict $ Json.encode $ Rpc.customError Nothing (negate 32000) msg

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

withExecutionUnitsEvaluator
    :: forall m a.
        ( MonadClock m
        , MonadSTM m
        )
    => (ExecutionUnitsEvaluator m Block -> Clients m Block -> m a)
    -> m a
withExecutionUnitsEvaluator action = do
    ( exUnitsEvaluator, stateQueryClient ) <- newExecutionUnitsEvaluator
    action exUnitsEvaluator $ Clients
         { chainSyncClient =
            ChainSyncClientPipelined idle
         , stateQueryClient =
            stateQueryClient
         , txSubmissionClient =
            LocalTxSubmissionClient idle
         , txMonitorClient =
            LocalTxMonitorClient idle
         }

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
    -> MaxInFlight
    -> Sensors m
    -> ExecutionUnitsEvaluator m Block
    -> GetGenesisConfig m
    -> Connection
    -> (Clients m Block -> m a)
    -> m a
withOuroborosClients tr maxInFlight sensors exUnitsEvaluator getGenesisConfig conn action = do
    (chainSyncQ, stateQueryQ, txSubmissionQ, txMonitorQ) <-
        atomically $ (,,,)
            <$> newTQueue
            <*> newTQueue
            <*> newTQueue
            <*> newTQueue

    withAsync (routeMessage Nothing chainSyncQ stateQueryQ txSubmissionQ txMonitorQ) $ \worker -> do
        link worker
        action $ Clients
             { chainSyncClient =
                 mkChainSyncClient maxInFlight chainSyncCodecs chainSyncQ yield
             , stateQueryClient =
                 mkStateQueryClient (contramap WebSocketStateQuery tr) stateQueryCodecs getGenesisConfig stateQueryQ yield
             , txSubmissionClient =
                 mkTxSubmissionClient txSubmissionCodecs exUnitsEvaluator txSubmissionQ yield
             , txMonitorClient =
                 mkTxMonitorClient txMonitorCodecs txMonitorQ yield
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
        -> TQueue m (TxMonitorMessage Block)
        -> m ()
    routeMessage cache chainSyncQ stateQueryQ txSubmissionQ txMonitorQ = do
        count (totalMessagesCounter sensors)
        bytes <- receive conn
        case cache of
            Just (prev, again) | prev == bytes ->
                again *> routeMessage cache chainSyncQ stateQueryQ txSubmissionQ txMonitorQ

            _ -> do
                matched <- Rpc.match bytes
                    (count (totalUnroutedCounter sensors) *> defaultHandler bytes)
                    -- ChainSync
                    [ Rpc.Handler decodeNextBlock
                        (\r t -> push chainSyncQ . MsgNextBlock r t)
                    , Rpc.Handler decodeFindIntersection
                        (\r t -> push chainSyncQ . MsgFindIntersection r t)

                    -- TxSubmission
                    , Rpc.Handler decodeSubmitTransaction
                        (\r t -> push txSubmissionQ . MsgSubmitTransaction r t)
                    , Rpc.Handler decodeEvaluateTransaction
                        (\r t -> push txSubmissionQ . MsgEvaluateTransaction r t)

                    -- StateQuery
                    , Rpc.Handler decodeAcquireLedgerState
                        (\r t -> push stateQueryQ . MsgAcquireLedgerState r t)
                    , Rpc.Handler decodeReleaseLedgerState
                        (\r t -> push stateQueryQ . MsgReleaseLedgerState r t)
                    , Rpc.Handler decodeQueryLedgerState
                        (\r t -> push stateQueryQ . MsgQueryLedgerState r t)

                    -- TxMonitor
                    , Rpc.Handler decodeAcquireMempool
                        (\r t -> push txMonitorQ . MsgAcquireMempool r t)
                    , Rpc.Handler decodeNextTransaction
                        (\r t -> push txMonitorQ . MsgNextTransaction r t)
                    , Rpc.Handler decodeHasTransaction
                        (\r t -> push txMonitorQ . MsgHasTransaction r t)
                    , Rpc.Handler decodeSizeOfMempool
                        (\r t -> push txMonitorQ . MsgSizeOfMempool r t)
                    , Rpc.Handler decodeReleaseMempool
                        (\r t -> push txMonitorQ . MsgReleaseMempool r t)
                    ]
                routeMessage matched chainSyncQ stateQueryQ txSubmissionQ txMonitorQ

    chainSyncCodecs@ChainSyncCodecs{..} =
        mkChainSyncCodecs encodeBlock encodePoint encodeTip
    stateQueryCodecs@StateQueryCodecs{..} =
        mkStateQueryCodecs encodePoint encodeAcquireFailure encodeAcquireExpired
    txMonitorCodecs@TxMonitorCodecs{..} =
        mkTxMonitorCodecs
            encodeTxId
            encodeTx
    txSubmissionCodecs@TxSubmissionCodecs{..} =
        mkTxSubmissionCodecs
            encodeTxId
            encodeRdmrPtr
            encodeExUnits
            encodeTxIn
            encodeTranslationError
            encodeSubmitTransactionError
            encodeScriptFailure
            encodeDeserialisationFailure

--
-- Logging
--

data TraceWebSocket where
    WebSocketClient
        :: TraceClient (SerializedTransaction Block) (SubmitTransactionError Block)
        -> TraceWebSocket

    WebSocketStateQuery
        :: TraceStateQuery Block
        -> TraceWebSocket

    WebSocketWorkerExited
        :: { exception :: Text }
        -> TraceWebSocket

    WebSocketConnectionAccepted
        :: { userAgent :: Text }
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
        WebSocketClient msg           -> getSeverityAnnotation' msg
        WebSocketStateQuery msg       -> getSeverityAnnotation msg
        WebSocketWorkerExited{}       -> Debug
        WebSocketConnectionAccepted{} -> Info
        WebSocketConnectionEnded{}    -> Info
        WebSocketUnknownException{}   -> Error
        WebSocketFailedToConnect{}    -> Error
