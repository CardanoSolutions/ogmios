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
import Ogmios.App.Protocol.StateQuery
    ( mkStateQueryClient )
import Ogmios.App.Protocol.TxSubmission
    ( mkTxSubmissionClient )
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
    , WebSocketApp
    , headers
    )
import Ogmios.Data.Json
    ( Json
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
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Network.HTTP.Types.Header
    ( hUserAgent )
import Ouroboros.Network.NodeToClient.Version
    ( NodeToClientVersionData (NodeToClientVersionData) )

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
        logWith tr (WebSocketConnectionAccepted $ userAgent pending)
        acceptRequest pending $ \conn -> do
            let trClient = natTracer liftIO $ contramap WebSocketClient tr
            client <- mkClient unliftIO trClient slotsPerEpoch <$> newOuroborosClients conn
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

newOuroborosClients
    :: forall m.
        ( MonadAsync m
        , MonadLink m
        , MonadWebSocket m
        )
    => Connection
    -> m (Clients m Block)
newOuroborosClients conn = do
    (chainSyncQ, txSubmissionQ, stateQueryQ) <-
        atomically $ (,,) <$> newTQueue <*> newTQueue <*> newTQueue

    link =<< async (routeMessage Nothing chainSyncQ stateQueryQ txSubmissionQ)

    pure Clients
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

    routeMessage
        :: Maybe (ByteString, m ())
        -> TQueue m (ChainSyncMessage Block)
        -> TQueue m (StateQueryMessage Block)
        -> TQueue m (TxSubmissionMessage Block)
        -> m ()
    routeMessage cache chainSyncQ stateQueryQ txSubmissionQ = do
        bytes <- receive conn
        case cache of
            Just (prev, again) | prev == bytes ->
                again *> routeMessage cache chainSyncQ stateQueryQ txSubmissionQ

            _ -> do
                matched <- Wsp.match bytes (defaultHandler bytes)
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
        } = mkChainSyncCodecs encodeBlock encodePoint encodeTip

    stateQueryCodecs@StateQueryCodecs
        { decodeAcquire
        , decodeRelease
        , decodeQuery
        } = mkStateQueryCodecs encodePoint encodeAcquireFailure

    txSubmissionCodecs@TxSubmissionCodecs
        { decodeSubmitTx
        } = mkTxSubmissionCodecs encodeHardForkApplyTxErr

    -- FIXME: Do error handling here.
    defaultHandler :: ByteString -> m ()
    defaultHandler _ = return ()

--
-- Logging
--

data TraceWebSocket where
    WebSocketClient
        :: TraceClient (SubmitTxPayload Block) (SubmitTxError Block)
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
        WebSocketClient msg           -> getSeverityAnnotation msg
        WebSocketConnectionAccepted{} -> Info
        WebSocketConnectionEnded{}    -> Info
        WebSocketFailedToConnect{}    -> Error
        WebSocketUnknownException{}   -> Error
