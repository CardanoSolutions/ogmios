--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2020 KtorZ <matthias.benkort@gmail.com>
-- License: MPL-2.0
-- Stability: Experimental
-- Portability: Portable
module Cardano.Network.Protocol.NodeToClient
    (
    -- * Building
      Block
    , Eras
    , SubmitTxError
    , SerializedTx
    , Crypto
    , Client
    , Clients(..)
    , mkClient

    -- * Connecting
    , connectClient
    , codecs
    , cChainSyncCodec
    , cTxSubmissionCodec
    , cStateQueryCodec
    , cTxMonitorCodec
    , nodeToClientV_Latest

    -- * Running
    , runPeer
    , runPipelinedPeer
    , chainSyncClientPeerPipelined
    , localStateQueryClientPeer
    , localTxSubmissionClientPeer
    , localTxMonitorClientPeer

    -- * Helpers / Re-exports
    , MuxError (..)
    , HandshakeProtocolError (..)
    , NodeToClientVersion
    , GenTx
    , GenTxId

    -- * Boilerplate
    , localChainSync
    , localTxSubmission
    , localStateQuery
    , localTxMonitor
    ) where

import Prelude hiding
    ( read )

import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Network.Protocol.NodeToClient.Trace
    ( TraceClient (..) )
import Cardano.Slotting.Slot
    ( SlotNo )
import Control.Monad.Class.MonadAsync
    ( MonadAsync )
import Control.Monad.Class.MonadST
    ( MonadST )
import Control.Monad.Class.MonadThrow
    ( MonadThrow )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Tracer
    ( Tracer (..), contramap, nullTracer )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Kind
    ( Type )
import Data.Map.Strict
    ( (!) )
import Data.Proxy
    ( Proxy (..) )
import Data.Void
    ( Void )
import Network.Mux
    ( MuxError (..), MuxMode (..) )
import Network.TypedProtocol.Codec
    ( Codec )
import Network.TypedProtocol.Codec.CBOR
    ( DeserialiseFailure )
import Ouroboros.Consensus.Byron.Ledger.Config
    ( CodecConfig (..) )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoEras, CodecConfig (..), HardForkApplyTxErr )
import Ouroboros.Consensus.Ledger.Query
    ( Query (..) )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( GenTx, GenTxId )
import Ouroboros.Consensus.Network.NodeToClient
    ( ClientCodecs, Codecs' (..), clientCodecs )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
    ( SupportedNetworkProtocolVersion (..) )
import Ouroboros.Consensus.Protocol.Praos.Translate
    ()
import Ouroboros.Consensus.Shelley.Ledger.Config
    ( CodecConfig (..) )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol
    ()
import Ouroboros.Network.Block
    ( Point (..), Tip (..) )
import Ouroboros.Network.Channel
    ( Channel, hoistChannel )
import Ouroboros.Network.Driver.Simple
    ( TraceSendRecv, runPeer, runPipelinedPeer )
import Ouroboros.Network.Mux
    ( MuxPeer (..), OuroborosApplication (..), RunMiniProtocol (..) )
import Ouroboros.Network.NodeToClient
    ( LocalAddress
    , NetworkConnectTracers (..)
    , NodeToClientProtocols (..)
    , NodeToClientVersion (..)
    , NodeToClientVersionData (..)
    , connectTo
    , localSnocket
    , nodeToClientProtocols
    , withIOManager
    )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined, chainSyncClientPeerPipelined )
import Ouroboros.Network.Protocol.ChainSync.Type
    ( ChainSync )
import Ouroboros.Network.Protocol.Handshake.Type
    ( HandshakeProtocolError (..) )
import Ouroboros.Network.Protocol.Handshake.Version
    ( combineVersions, simpleSingletonVersions )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( LocalStateQueryClient, localStateQueryClientPeer )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( LocalStateQuery )
import Ouroboros.Network.Protocol.LocalTxMonitor.Client
    ( LocalTxMonitorClient, localTxMonitorClientPeer )
import Ouroboros.Network.Protocol.LocalTxMonitor.Type
    ( LocalTxMonitor )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxSubmissionClient, localTxSubmissionClientPeer )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( LocalTxSubmission )

-- | Concrete block type.
type Block = CardanoBlock StandardCrypto

-- | Concrete eras type.
type Eras = CardanoEras StandardCrypto

-- | A helper to help getting more uniform type signatures by making the submit
-- failure a function of a 'block' parameter
type family SubmitTxError block :: Type where
    SubmitTxError (CardanoBlock crypto) = HardForkApplyTxErr (CardanoEras crypto)

-- | Type-family helper, similar to 'SubmitTxError' but more generic.
type family Crypto block :: Type where
    Crypto (CardanoBlock crypto) = crypto

-- | A slightly more transparent type alias for 'GenTx''
type SerializedTx = GenTx

-- | Type representing a network client running two mini-protocols to sync
-- from the chain and, submit transactions.
type Client m = OuroborosApplication
    'InitiatorMode
        -- Initiator ~ Client (as opposed to Responder / Server)
    LocalAddress
        -- Address type
    ByteString
        -- Concrete representation for bytes string
    m
        -- Underlying monad we run in
    ()
        -- Clients return type
    Void

-- | A handy type to pass clients around
data Clients m block = Clients
    { chainSyncClient
        :: ChainSyncClientPipelined block (Point block) (Tip block) m ()
    , txSubmissionClient
        :: LocalTxSubmissionClient (SerializedTx block) (SubmitTxError block) m ()
    , txMonitorClient
        :: LocalTxMonitorClient (GenTxId block) (GenTx block) SlotNo m ()
    , stateQueryClient
        :: LocalStateQueryClient block (Point block) (Query block) m ()
    }

-- | Connect a client to a network, see `mkClient` to construct a network
-- client interface.
connectClient
    :: MonadIO m
    => Tracer IO (TraceClient tx err)
    -> (NodeToClientVersion -> Client IO)
    -> NodeToClientVersionData
    -> FilePath
    -> m ()
connectClient tr client vData addr = liftIO $ withIOManager $ \iocp -> do
    connectTo (localSnocket iocp) tracers versions addr
  where
    versions = combineVersions
        [ simpleSingletonVersions v vData (client v)
        | v <- [NodeToClientV_13, NodeToClientV_12, NodeToClientV_11, NodeToClientV_10]
        ]

    tracers :: NetworkConnectTracers LocalAddress NodeToClientVersion
    tracers = NetworkConnectTracers
        { nctMuxTracer = nullTracer
        , nctHandshakeTracer = contramap TrHandshake tr
        }

nodeToClientV_Latest :: NodeToClientVersion
nodeToClientV_Latest = maxBound

-- | Construct a network client
mkClient
    :: forall m.
        ( MonadAsync m
        , MonadIO m
        , MonadST m
        , MonadThrow m
        )
    => (forall a. m a -> IO a)
        -- ^ A natural transformation to unlift a particular 'm' into 'IO'.
    -> Tracer m (TraceClient (SerializedTx Block) (SubmitTxError Block))
        -- ^ Base trace for underlying protocols
    -> EpochSlots
        -- ^ Static blockchain parameters
    -> Clients m Block
        -- ^ Clients with the driving logic
    -> (NodeToClientVersion -> Client IO)
mkClient unlift tr epochSlots clients = \nodeToClientV ->
    nodeToClientProtocols (const $ pure $ NodeToClientProtocols
        { localChainSyncProtocol =
            InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
                localChainSync unlift trChainSync (codecChainSync nodeToClientV)
                (chainSyncClient clients)
                (hoistChannel liftIO channel)

        , localTxSubmissionProtocol =
            InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
                localTxSubmission unlift trTxSubmission (codecTxSubmission nodeToClientV)
                (txSubmissionClient clients)
                (hoistChannel liftIO channel)

        , localTxMonitorProtocol =
            InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
                localTxMonitor unlift trTxMonitor (codecTxMonitor nodeToClientV)
                (txMonitorClient clients)
                (hoistChannel liftIO channel)

        , localStateQueryProtocol =
            InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
                localStateQuery unlift trStateQuery (codecStateQuery nodeToClientV)
                (stateQueryClient clients)
                (hoistChannel liftIO channel)
        })
        nodeToClientV
  where
    trChainSync    = nullTracer
    codecChainSync = cChainSyncCodec . codecs epochSlots

    trTxSubmission    = contramap TrTxSubmission tr
    codecTxSubmission = cTxSubmissionCodec . codecs epochSlots

    trTxMonitor    = nullTracer
    codecTxMonitor = cTxMonitorCodec . codecs epochSlots

    trStateQuery    = nullTracer
    codecStateQuery = cStateQueryCodec . codecs epochSlots

-- | Boilerplate for lifting a 'ChainSyncClientPipelined'
localChainSync
    :: forall m protocol.
        ( protocol ~ ChainSync Block (Point Block) (Tip Block)
        , MonadThrow m
        , MonadAsync m
        )
    => (forall a. m a -> IO a)
        -- ^ A natural transformation to unlift a particular 'm' into 'IO'.
    -> Tracer m (TraceSendRecv protocol)
        -- ^ Base tracer for the mini-protocols
    -> Codec protocol DeserialiseFailure m ByteString
        -- ^ Codec for deserializing / serializing binary data
    -> ChainSyncClientPipelined Block (Point Block) (Tip Block) m ()
        -- ^ The actual chain sync client
    -> Channel m ByteString
        -- ^ A 'Channel' is a abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> IO ((), Maybe ByteString)
localChainSync unliftIO tr codec client channel =
    unliftIO $ runPipelinedPeer tr codec channel (chainSyncClientPeerPipelined client)

-- | Boilerplate for lifting a 'LocalTxSubmissionClient'
localTxSubmission
    :: forall m protocol.
        ( protocol ~ LocalTxSubmission (SerializedTx Block) (SubmitTxError Block)
        , MonadThrow m
        )
    => (forall a. m a -> IO a)
        -- ^ A natural transformation to unlift a particular 'm' into 'IO'.
    -> Tracer m (TraceSendRecv protocol)
        -- ^ Base tracer for the mini-protocols
    -> Codec protocol DeserialiseFailure m ByteString
        -- ^ Codec for deserializing / serializing binary data
    -> LocalTxSubmissionClient (SerializedTx Block) (SubmitTxError Block) m ()
        -- ^ Actual local tx submission client
    -> Channel m ByteString
        -- ^ A 'Channel' is an abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> IO ((), Maybe ByteString)
localTxSubmission unliftIO tr codec client channel =
    unliftIO $ runPeer tr codec channel (localTxSubmissionClientPeer client)

-- | Boilerplate for lifting a 'LocalTxMonitorClient'
localTxMonitor
    :: forall m protocol.
        ( protocol ~ LocalTxMonitor (GenTxId Block) (GenTx Block) SlotNo
        , MonadThrow m
        )
    => (forall a. m a -> IO a)
        -- ^ A natural transformation to unlift a particular 'm' into 'IO'.
    -> Tracer m (TraceSendRecv protocol)
        -- ^ Base tracer for the mini-protocols
    -> Codec protocol DeserialiseFailure m ByteString
        -- ^ Codec for deserializing / serializing binary data
    -> LocalTxMonitorClient (GenTxId Block) (GenTx Block) SlotNo m ()
        -- ^ Actual local tx submission client
    -> Channel m ByteString
        -- ^ A 'Channel' is an abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> IO ((), Maybe ByteString)
localTxMonitor unliftIO tr codec client channel =
    unliftIO $ runPeer tr codec channel (localTxMonitorClientPeer client)

-- | Boilerplate for lifting a 'LocalStateQueryClient'
localStateQuery
    :: forall m protocol.
        ( protocol ~ LocalStateQuery Block (Point Block) (Query Block)
        , MonadThrow m
        )
    => (forall a. m a -> IO a)
        -- ^ A natural transformation to unlift a particular 'm' into 'IO'.
    -> Tracer m (TraceSendRecv protocol)
        -- ^ Base tracer for the mini-protocols
    -> Codec protocol DeserialiseFailure m ByteString
        -- ^ Codec for deserializing / serializing binary data
    -> LocalStateQueryClient Block (Point Block) (Query Block) m ()
        -- ^ Actual local state query client.
    -> Channel m ByteString
        -- ^ A 'Channel' is an abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> IO ((), Maybe ByteString)
localStateQuery unliftIO tr codec client channel =
    unliftIO $ runPeer tr codec channel (localStateQueryClientPeer client)

-- | Client codecs for Cardano
codecs
    :: forall m. (MonadST m)
    => EpochSlots
    -> NodeToClientVersion
    -> ClientCodecs Block m
codecs epochSlots nodeToClientV =
    clientCodecs cfg (supportedVersions ! nodeToClientV) nodeToClientV
  where
    supportedVersions = supportedNodeToClientVersions (Proxy @Block)
    cfg = CardanoCodecConfig byron shelley allegra mary alonzo babbage
      where
        byron   = ByronCodecConfig epochSlots
        shelley = ShelleyCodecConfig
        allegra = ShelleyCodecConfig
        mary    = ShelleyCodecConfig
        alonzo  = ShelleyCodecConfig
        babbage = ShelleyCodecConfig
