--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Network.Protocol.NodeToClient
    (
    -- * Building
      Block
    , Client
    , mkClient

    -- * Connecting
    , connectClient
    , codecs

    -- * Boilerplate
    , localChainSync
    , localTxSubmission
    , localStateQuery
    , nullProtocol
    ) where

import Prelude hiding
    ( read )

import Cardano.Byron.Constants
    ( NodeVersionData )
import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Cardano.Network.Protocol.NodeToClient.Trace
    ( TraceClient (..) )
import Control.Monad
    ( forever )
import Control.Monad.Class.MonadAsync
    ( MonadAsync )
import Control.Monad.Class.MonadST
    ( MonadST )
import Control.Monad.Class.MonadThrow
    ( MonadThrow )
import Control.Monad.Class.MonadTimer
    ( MonadTimer, threadDelay )
import Control.Monad.IO.Class
    ( MonadIO )
import Control.Tracer
    ( Tracer (..), contramap, nullTracer )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Map.Strict
    ( (!) )
import Data.Proxy
    ( Proxy (..) )
import Data.Void
    ( Void )
import Network.Mux
    ( MuxMode (..) )
import Network.TypedProtocol.Codec
    ( Codec )
import Ouroboros.Consensus.Byron.Ledger
    ( GenTx, Query (..) )
import Ouroboros.Consensus.Byron.Ledger.Config
    ( CodecConfig (..) )
import Ouroboros.Consensus.Byron.Node
    ()
import Ouroboros.Consensus.Cardano
    ( CardanoBlock )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoApplyTxErr, CodecConfig (..) )
import Ouroboros.Consensus.Network.NodeToClient
    ( ClientCodecs, Codecs' (..), clientCodecs )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
    ( SupportedNetworkProtocolVersion (..) )
import Ouroboros.Consensus.Shelley.Ledger.Config
    ( CodecConfig (..) )
import Ouroboros.Consensus.Shelley.Protocol.Crypto
    ( StandardCrypto )
import Ouroboros.Network.Block
    ( Tip (..) )
import Ouroboros.Network.Channel
    ( Channel )
import Ouroboros.Network.Codec
    ( DeserialiseFailure )
import Ouroboros.Network.Driver.Simple
    ( TraceSendRecv, runPeer, runPipelinedPeer )
import Ouroboros.Network.Mux
    ( MuxPeer (..), OuroborosApplication (..), RunMiniProtocol (..) )
import Ouroboros.Network.NodeToClient
    ( LocalAddress
    , NetworkConnectTracers (..)
    , NodeToClientProtocols (..)
    , NodeToClientVersion (..)
    , connectTo
    , localSnocket
    , nodeToClientProtocols
    , withIOManager
    )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined, chainSyncClientPeerPipelined )
import Ouroboros.Network.Protocol.ChainSync.Type
    ( ChainSync )
import Ouroboros.Network.Protocol.Handshake.Version
    ( DictVersion (..), simpleSingletonVersions )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( LocalStateQueryClient, localStateQueryClientPeer )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( LocalStateQuery )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxSubmissionClient, localTxSubmissionClientPeer )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( LocalTxSubmission )

-- | Concrete block type.
type Block = CardanoBlock StandardCrypto

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

-- Connect a client to a network, see `mkClient` to construct a network
-- client interface.
connectClient
    :: Tracer IO (TraceClient tx err)
    -> Client IO
    -> NodeVersionData
    -> FilePath
    -> IO ()
connectClient tr client (vData, vCodec) addr = withIOManager $ \iocp -> do
    let vDict = DictVersion vCodec
    let versions = simpleSingletonVersions NodeToClientV_2 vData vDict client
    let socket = localSnocket iocp addr
    connectTo socket tracers versions addr
  where
    tracers :: NetworkConnectTracers LocalAddress NodeToClientVersion
    tracers = NetworkConnectTracers
        { nctMuxTracer = contramap TrMux tr
        , nctHandshakeTracer = contramap TrHandshake tr
        }

-- | Construct a network client
mkClient
    :: forall m err.
        ( err ~ CardanoApplyTxErr StandardCrypto
        , MonadIO m, MonadThrow m, MonadST m, MonadAsync m
        )
    => Tracer m (TraceClient (GenTx Block) err)
        -- ^ Base trace for underlying protocols
    -> EpochSlots
        -- ^ Static blockchain parameters
    -> ChainSyncClientPipelined Block (Tip Block) m ()
        -- ^ Actual ChainSync client logic
    -> LocalTxSubmissionClient (GenTx Block) err m ()
        -- ^ Actual LocalTxSubmission client logic
    -> LocalStateQueryClient Block (Query Block) m ()
        -- ^ Actual LocalStateQuery client logic
    -> Client m
mkClient tr epochSlots chainSyncClient txSubmissionClient stateQueryClient =
    nodeToClientProtocols (const $ pure $ NodeToClientProtocols
        { localChainSyncProtocol =
            InitiatorProtocolOnly $ MuxPeerRaw $
                localChainSync trChainSync codecChainSync chainSyncClient

        , localTxSubmissionProtocol =
            InitiatorProtocolOnly $ MuxPeerRaw $
                localTxSubmission trTxSubmission codecTxSubmission txSubmissionClient

        , localStateQueryProtocol =
            InitiatorProtocolOnly $ MuxPeerRaw $
                localStateQuery trStateQuery codecStateQuery stateQueryClient
        })
        NodeToClientV_2
  where
    trChainSync    = nullTracer
    codecChainSync = cChainSyncCodec $ codecs epochSlots

    trTxSubmission    = contramap TrTxSubmission tr
    codecTxSubmission = cTxSubmissionCodec $ codecs epochSlots

    trStateQuery    = nullTracer
    codecStateQuery = cStateQueryCodec $ codecs epochSlots

localChainSync
    :: forall m protocol.
        ( protocol ~ ChainSync Block (Tip Block)
        , MonadThrow m, MonadAsync m
        )
    => Tracer m (TraceSendRecv protocol)
        -- ^ Base tracer for the mini-protocols
    -> Codec protocol DeserialiseFailure m ByteString
        -- ^ Codec for deserializing / serializing binary data
    -> ChainSyncClientPipelined Block (Tip Block) m ()
        -- ^ The actual chain sync client
    -> Channel m ByteString
        -- ^ A 'Channel' is a abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m ((), Maybe ByteString)
localChainSync tr codec client channel =
    runPipelinedPeer tr codec channel (chainSyncClientPeerPipelined client)

localTxSubmission
    :: forall m err protocol.
        ( err ~ CardanoApplyTxErr StandardCrypto
        , protocol ~ LocalTxSubmission (GenTx Block) err
        , MonadThrow m
        )
    => Tracer m (TraceSendRecv protocol)
        -- ^ Base tracer for the mini-protocols
    -> Codec protocol DeserialiseFailure m ByteString
        -- ^ Codec for deserializing / serializing binary data
    -> LocalTxSubmissionClient (GenTx Block) err m ()
        -- ^ Actual local tx submission client
    -> Channel m ByteString
        -- ^ A 'Channel' is an abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m ((), Maybe ByteString)
localTxSubmission tr codec client channel =
    runPeer tr codec channel (localTxSubmissionClientPeer client)

localStateQuery
    :: forall m protocol.
        ( protocol ~ LocalStateQuery Block (Query Block)
        , MonadThrow m
        )
    => Tracer m (TraceSendRecv protocol)
        -- ^ Base tracer for the mini-protocols
    -> Codec protocol DeserialiseFailure m ByteString
        -- ^ Codec for deserializing / serializing binary data
    -> LocalStateQueryClient Block (Query Block) m ()
        -- ^ Actual local state query client.
    -> Channel m ByteString
        -- ^ A 'Channel' is an abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m ((), Maybe ByteString)
localStateQuery tr codec client channel =
    runPeer tr codec channel (localStateQueryClientPeer client)

-- | A protocol that does nothing. Useful as a placeholder for protocols of an
-- Ouroboros application.
nullProtocol
    :: forall m a. (MonadTimer m)
    => RunMiniProtocol 'InitiatorMode ByteString m a Void
nullProtocol = do
    InitiatorProtocolOnly $ MuxPeerRaw $ const $ forever $ threadDelay 43200

-- | Client codecs for Cardano
codecs
    :: forall m. (MonadST m)
    => EpochSlots
    -> ClientCodecs Block m
codecs epochSlots =
    clientCodecs (CardanoCodecConfig byron shelley) version
  where
    byron = ByronCodecConfig epochSlots
    shelley = ShelleyCodecConfig
    version = supportedNodeToClientVersions (Proxy @Block) ! NodeToClientV_3
