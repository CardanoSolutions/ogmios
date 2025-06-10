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
import Data.List
    ( isInfixOf
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
    ( defaultWithInternalError
    , onUnmatchedMessage
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
    ( ExecutionUnitsEvaluator (..)
    , TraceTxSubmission
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
import Ogmios.Control.MonadDisk
    ( MonadDisk
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
    , encodeEvaluationError
    , encodeExUnits
    , encodeGenTxId
    , encodePoint
    , encodeSubmitTransactionError
    , encodeTip
    , encodeTx
    , jsonToByteString
    )
import Ogmios.Data.Json.Ledger.PredicateFailure
    ( encodeScriptPurposeIndexInAnyEra
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
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxSubmissionClient (..)
    )
import System.TimeManager
    ( TimeoutThread (..)
    )

import qualified Codec.Json.Rpc as Rpc
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
        , MonadDisk m
        , HasType NetworkParameters env
        , HasType Configuration env
        , HasType (Sensors m) env
        )
    => Logger TraceWebSocket
    -> (forall a. m a -> IO a)
    -> m WebSocketApp
newWebSocketApp tr unliftIO = do
    NetworkParameters{slotsPerEpoch,networkMagic} <- asks (view typed)
    Configuration{nodeSocket,maxInFlight,nodeConfig,includeCbor,metadataFormat,strictRpc} <- asks (view typed)
    sensors <- asks (view typed)
    let getGenesisConfig = GetGenesisConfig
            { getByronGenesis = readByronGenesis nodeConfig
            , getShelleyGenesis = readShelleyGenesis nodeConfig
            , getAlonzoGenesis = readAlonzoGenesis nodeConfig
            , getConwayGenesis = readConwayGenesis nodeConfig
            }

    let opts = Rpc.defaultOptions
            { Rpc.omitMethodInResponse = strictRpc
            }

    let codecs =
            ( mkChainSyncCodecs
                opts
                (encodeBlock (metadataFormat, includeCbor))
                encodePoint
                encodeTip

            , mkStateQueryCodecs
                opts
                encodePoint
                encodeAcquireFailure
                encodeAcquireExpired

            , mkTxMonitorCodecs
                opts
                encodeGenTxId
                (encodeTx (metadataFormat, includeCbor))

            , mkTxSubmissionCodecs
                opts
                encodeGenTxId
                encodeScriptPurposeIndexInAnyEra
                encodeExUnits
                encodeEvaluationError
                encodeSubmitTransactionError
                encodeDeserialisationFailure
            )

    return $ \pending -> unliftIO $ do
        logWith tr $ WebSocketConnectionAccepted (userAgent pending)
        recordSession sensors $ onExceptions $ acceptRequest pending $ \conn -> do
            let trClient = contramap WebSocketClient tr
            withExecutionUnitsEvaluator tr $ \exUnitsEvaluator exUnitsClients -> do
                withOuroborosClients tr opts codecs maxInFlight sensors exUnitsEvaluator getGenesisConfig conn $ \protocolsClients -> do
                    let clientA = mkClient unliftIO (natTracer liftIO trClient) slotsPerEpoch protocolsClients
                    let clientB = mkClient unliftIO (natTracer liftIO trClient) slotsPerEpoch exUnitsClients
                    let vData  = NodeToClientVersionData networkMagic False
                    concurrently_
                       (connectClient trClient clientA vData nodeSocket)
                       (connectClient trClient clientB vData nodeSocket)
                       & handle (onIOException conn)
                       & handle (onShelleyEncoderException conn)
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

    onShelleyEncoderException :: Connection -> SomeException -> m ()
    onShelleyEncoderException conn e =
        if "ShelleyEncoderUnsupportedQuery" `isInfixOf` displayException e then do
            let msg = "Unable to encode and send a query to the node because it is \
                      \likely running an old version of the client protocols. Upgrade \
                      \your node and try again."
            logWith tr $ WebSocketNodeVersionIsTooOld msg
            close conn $ toStrict $ Json.encode $ Rpc.customError Nothing (negate 32001) (toString msg)
        else
            throwIO e

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
        , MonadLog m
        )
    => Logger TraceWebSocket
    -> (ExecutionUnitsEvaluator m Block -> Clients m Block -> m a)
    -> m a
withExecutionUnitsEvaluator tr action = do
    ( exUnitsEvaluator, stateQueryClient, txMonitorClient ) <- newExecutionUnitsEvaluator (contramap WebSocketTxSubmission tr)
    action exUnitsEvaluator $ Clients
         { chainSyncClient =
            ChainSyncClientPipelined idle
         , stateQueryClient =
            stateQueryClient
         , txSubmissionClient =
            LocalTxSubmissionClient idle
         , txMonitorClient =
            const txMonitorClient
         }

withOuroborosClients
    :: forall m a.
        ( MonadAsync m
        , MonadDisk m
        , MonadLink m
        , MonadLog m
        , MonadMetrics m
        , MonadWebSocket m
        )
    => Logger TraceWebSocket
    -> Rpc.Options
    -> (ChainSyncCodecs Block, StateQueryCodecs Block, TxMonitorCodecs Block, TxSubmissionCodecs Block)
    -> MaxInFlight
    -> Sensors m
    -> ExecutionUnitsEvaluator m Block
    -> GetGenesisConfig m
    -> Connection
    -> (Clients m Block -> m a)
    -> m a
withOuroborosClients tr opts codecs maxInFlight sensors exUnitsEvaluator getGenesisConfig conn action = do
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
                 mkStateQueryClient (contramap WebSocketStateQuery tr) catchError stateQueryCodecs getGenesisConfig stateQueryQ yield
             , txSubmissionClient =
                 mkTxSubmissionClient (contramap WebSocketTxSubmission tr) catchError txSubmissionCodecs exUnitsEvaluator txSubmissionQ yield
             , txMonitorClient =
                 mkTxMonitorClient catchError txMonitorCodecs txMonitorQ yield
             }
  where
    catchError :: forall b r. m b -> (Json -> m ()) -> Rpc.ToResponse r -> m b -> m b
    catchError =
        defaultWithInternalError reportException opts
      where
        reportException =
            logWith tr . WebSocketUnknownException . toText . displayException

    yield :: Json -> m ()
    yield = send conn . jsonToByteString

    push :: TQueue m any -> any -> m ()
    push queue = atomically . writeTQueue queue

    defaultHandler :: ByteString -> m ()
    defaultHandler = yield . onUnmatchedMessage @Block opts

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
                        (\r -> push chainSyncQ . MsgNextBlock r)
                    , Rpc.Handler decodeFindIntersection
                        (\r -> push chainSyncQ . MsgFindIntersection r)

                    -- TxSubmission
                    , Rpc.Handler decodeSubmitTransaction
                        (\r -> push txSubmissionQ . MsgSubmitTransaction r)
                    , Rpc.Handler decodeEvaluateTransaction
                        (\r -> push txSubmissionQ . MsgEvaluateTransaction r)

                    -- StateQuery
                    , Rpc.Handler decodeAcquireLedgerState
                        (\r -> push stateQueryQ . MsgAcquireLedgerState r)
                    , Rpc.Handler decodeReleaseLedgerState
                        (\r -> push stateQueryQ . MsgReleaseLedgerState r)
                    , Rpc.Handler decodeQueryLedgerState
                        (\r -> push stateQueryQ . MsgQueryLedgerState r)

                    -- TxMonitor
                    , Rpc.Handler decodeAcquireMempool
                        (\r -> push txMonitorQ . MsgAcquireMempool r)
                    , Rpc.Handler decodeNextTransaction
                        (\r -> push txMonitorQ . MsgNextTransaction r)
                    , Rpc.Handler decodeHasTransaction
                        (\r -> push txMonitorQ . MsgHasTransaction r)
                    , Rpc.Handler decodeSizeOfMempool
                        (\r -> push txMonitorQ . MsgSizeOfMempool r)
                    , Rpc.Handler decodeReleaseMempool
                        (\r -> push txMonitorQ . MsgReleaseMempool r)
                    ]
                routeMessage matched chainSyncQ stateQueryQ txSubmissionQ txMonitorQ

    (   chainSyncCodecs@ChainSyncCodecs{..}
      , stateQueryCodecs@StateQueryCodecs{..}
      , txMonitorCodecs@TxMonitorCodecs{..}
      , txSubmissionCodecs@TxSubmissionCodecs{..}
      ) = codecs

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

    WebSocketTxSubmission
        :: TraceTxSubmission
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

    WebSocketNodeVersionIsTooOld
        :: { encoderException :: Text }
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
        WebSocketClient msg            -> getSeverityAnnotation' msg
        WebSocketStateQuery msg        -> getSeverityAnnotation msg
        WebSocketTxSubmission msg      -> getSeverityAnnotation msg
        WebSocketWorkerExited{}        -> Debug
        WebSocketConnectionAccepted{}  -> Info
        WebSocketConnectionEnded{}     -> Info
        WebSocketNodeVersionIsTooOld{} -> Error
        WebSocketUnknownException{}    -> Error
        WebSocketFailedToConnect{}     -> Error
