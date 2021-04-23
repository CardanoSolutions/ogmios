--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
    ( mkChainSyncClient )
import Ogmios.App.Protocol.StateQuery
    ( mkStateQueryClient )
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
    , encodeHardForkApplyTxErr
    , encodePoint
    , encodeTip
    , jsonToByteString
    )
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
import Data.Aeson.Via.Show
    ( GenericToJsonViaShow (..) )
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
    Options{nodeSocket} <- asks (view typed)
    sensors <- asks (view typed)
    return $ \pending -> unliftIO $ do
        let (mode, sub) = choseSerializationMode pending
        logWith tr $ WebSocketConnectionAccepted (userAgent pending) mode
        recordSession sensors $ onExceptions $ acceptRequest pending sub $ \conn -> do
            let trClient = natTracer liftIO $ contramap WebSocketClient tr
            withOuroborosClients mode sensors conn $ \clients -> do
                let client = mkClient unliftIO trClient slotsPerEpoch clients
                let vData  = NodeToClientVersionData networkMagic
                connectClient nullTracer client vData nodeSocket
                    & handle (onIOException conn)
        logWith tr (WebSocketConnectionEnded $ userAgent pending)
  where
    userAgent :: PendingConnection -> Text
    userAgent pending = maybe "User-Agent unknown" (decodeUtf8 . snd)
        $ find ((== hUserAgent) . fst)
        $ headers pending

    onIOException :: Connection -> IOException -> m ()
    onIOException conn e = do
        logWith tr $ WebSocketFailedToConnect e
        let msg = "Connection with the node lost or failed."
        close conn $ toStrict $ Json.encode $ Wsp.serverFault msg

    onExceptions :: m () -> m ()
    onExceptions
        = handle onUnknownException
        . handle onTimeoutThread
        . handle onConnectionClosed

    onUnknownException :: SomeException -> m ()
    onUnknownException e0 = case fromException e0 of
        Nothing -> do
            logWith tr $ WebSocketUnknownException e0
        Just (ExceptionInLinkedThread _ e1) -> do
            case fromException e1 of
                Nothing ->
                    logWith tr $ WebSocketWorkerExited e1
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
        , MonadMetrics m
        , MonadOuroboros m
        , MonadWebSocket m
        )
    => SerializationMode
    -> Sensors m
    -> Connection
    -> (Clients m Block -> m a)
    -> m a
withOuroborosClients mode sensors conn action = do
    (chainSyncQ, txSubmissionQ, stateQueryQ) <-
        atomically $ (,,) <$> newTQueue <*> newTQueue <*> newTQueue

    withAsync (routeMessage Nothing chainSyncQ stateQueryQ txSubmissionQ) $ \worker -> do
        link worker
        action $ Clients
             { chainSyncClient =
                 mkChainSyncClient chainSyncCodecs chainSyncQ yield
             , stateQueryClient =
                 mkStateQueryClient stateQueryCodecs stateQueryQ yield
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
                        (\r -> push chainSyncQ . MsgRequestNext r)
                    , Wsp.Handler decodeFindIntersect
                        (\r -> push chainSyncQ . MsgFindIntersect r)

                    , Wsp.Handler decodeAcquire
                        (\r -> push stateQueryQ . MsgAcquire r)
                    , Wsp.Handler decodeRelease
                        (\r -> push stateQueryQ . MsgRelease r)
                    , Wsp.Handler decodeQuery
                        (\r -> push stateQueryQ . MsgQuery r)

                    , Wsp.Handler decodeSubmitTx
                        (\r -> push txSubmissionQ .  MsgSubmitTx r)
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
        } = mkTxSubmissionCodecs encodeHardForkApplyTxErr

--
-- Logging
--

data TraceWebSocket where
    WebSocketClient
        :: TraceClient (SubmitTxPayload Block) (SubmitTxError Block)
        -> TraceWebSocket

    WebSocketWorkerExited
        :: { exception :: SomeException }
        -> TraceWebSocket

    WebSocketConnectionAccepted
        :: { userAgent :: Text, mode :: SerializationMode }
        -> TraceWebSocket

    WebSocketConnectionEnded
        :: { userAgent :: Text }
        -> TraceWebSocket

    WebSocketUnknownException
        :: { exception :: SomeException }
        -> TraceWebSocket

    WebSocketFailedToConnect
        :: { ioException :: IOException }
        -> TraceWebSocket
    deriving stock (Generic, Show)
    deriving ToJSON via GenericToJsonViaShow TraceWebSocket

instance HasSeverityAnnotation TraceWebSocket where
    getSeverityAnnotation = \case
        WebSocketClient msg           -> getSeverityAnnotation msg
        WebSocketWorkerExited{}       -> Debug
        WebSocketConnectionAccepted{} -> Info
        WebSocketConnectionEnded{}    -> Info
        WebSocketUnknownException{}   -> Error
        WebSocketFailedToConnect{}    -> Error
