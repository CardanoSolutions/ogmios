--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
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

import Cardano.Network.Protocol.NodeToClient
    ( Block
    , Clients (..)
    , SerializedTx
    , SubmitTxError
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
    , encodeAcquireFailure
    , encodeBlock
    , encodeExUnits
    , encodePoint
    , encodeScriptFailure
    , encodeSubmitTxError
    , encodeTip
    , encodeTranslationError
    , encodeTx
    , encodeTxId
    , encodeTxIn
    , jsonToByteString
    , stringifyRdmrPtr
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

import qualified Cardano.Ledger.Alonzo.PParams
import qualified Cardano.Ledger.Babbage.PParams
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
            }
    return $ \pending -> unliftIO $ do
        logWith tr $ WebSocketConnectionAccepted (userAgent pending)
        recordSession sensors $ onExceptions $ acceptRequest pending $ \conn -> do
            let trClient = contramap WebSocketClient tr
            withExecutionUnitsEvaluator $ \exUnitsEvaluator exUnitsClients -> do
                withOuroborosClients tr maxInFlight sensors exUnitsEvaluator getGenesisConfig conn $ \protocolsClients -> do
                    let clientA = mkClient unliftIO (natTracer liftIO trClient) slotsPerEpoch protocolsClients
                    let clientB = mkClient unliftIO (natTracer liftIO trClient) slotsPerEpoch exUnitsClients
                    let vData  = NodeToClientVersionData networkMagic
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
        close conn $ toStrict $ Json.encode $ Rpc.serverError Nothing (negate 32000) msg

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
                    [ Rpc.Handler decodeRequestNext
                        (\r t -> push chainSyncQ . MsgRequestNext r t)
                    , Rpc.Handler decodeFindIntersect
                        (\r t -> push chainSyncQ . MsgFindIntersect r t)

                    -- TxSubmission
                    , Rpc.Handler decodeSubmitTx
                        (\r t -> push txSubmissionQ .  MsgSubmitTx r t)
                    , Rpc.Handler decodeEvaluateTx
                        (\r t -> push txSubmissionQ .  MsgEvaluateTx r t)

                    -- StateQuery
                    , Rpc.Handler decodeAcquire
                        (\r t -> push stateQueryQ . MsgAcquire r t)
                    , Rpc.Handler decodeRelease
                        (\r t -> push stateQueryQ . MsgRelease r t)
                    , Rpc.Handler decodeQuery
                        (\r t -> push stateQueryQ . MsgQuery r t)

                    -- TxMonitor
                    , Rpc.Handler decodeAwaitAcquire
                        (\r t -> push txMonitorQ . MsgAwaitAcquire r t)
                    , Rpc.Handler decodeNextTx
                        (\r t -> push txMonitorQ . MsgNextTx r t)
                    , Rpc.Handler decodeHasTx
                        (\r t -> push txMonitorQ . MsgHasTx r t)
                    , Rpc.Handler decodeSizeAndCapacity
                        (\r t -> push txMonitorQ . MsgSizeAndCapacity r t)
                    , Rpc.Handler decodeReleaseMempool
                        (\r t -> push txMonitorQ . MsgReleaseMempool r t)
                    ]
                routeMessage matched chainSyncQ stateQueryQ txSubmissionQ txMonitorQ

    chainSyncCodecs@ChainSyncCodecs{..} =
        mkChainSyncCodecs encodeBlock encodePoint encodeTip
    stateQueryCodecs@StateQueryCodecs{..} =
        mkStateQueryCodecs encodePoint encodeAcquireFailure
    txMonitorCodecs@TxMonitorCodecs{..} =
        mkTxMonitorCodecs
            encodeTxId
            encodeTx
    txSubmissionCodecs@TxSubmissionCodecs{..} =
        mkTxSubmissionCodecs
            encodeTxId
            encodeSubmitTxError
            stringifyRdmrPtr
            encodeExUnits
            encodeScriptFailure
            encodeTxIn
            encodeTranslationError

--
-- Logging
--

data TraceWebSocket where
    WebSocketClient
        :: TraceClient (SerializedTx Block) (SubmitTxError Block)
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
