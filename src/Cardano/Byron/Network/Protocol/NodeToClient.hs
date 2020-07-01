--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Byron.Network.Protocol.NodeToClient
    (
    -- * Building
      Client
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
import Cardano.Chain.Byron.API
    ( ApplyMempoolPayloadErr (..) )
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
import Data.Void
    ( Void )
import Network.Mux
    ( MuxMode (..) )
import Network.TypedProtocol.Codec
    ( Codec )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock (..), ByronNodeToClientVersion (..), GenTx, Query (..) )
import Ouroboros.Consensus.Byron.Ledger.Config
    ( CodecConfig (..) )
import Ouroboros.Consensus.Byron.Node
    ()
import Ouroboros.Consensus.Config.SecurityParam
    ( SecurityParam (..) )
import Ouroboros.Consensus.Network.NodeToClient
    ( ClientCodecs, Codecs' (..), clientCodecs )
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
    :: forall m block err.
        ( block ~ ByronBlock
        , err ~ ApplyMempoolPayloadErr
        , MonadIO m, MonadThrow m, MonadST m, MonadAsync m
        )
    => Tracer m (TraceClient (GenTx block) err)
        -- ^ Base trace for underlying protocols
    -> (EpochSlots, SecurityParam)
        -- ^ Static blockchain parameters
    -> ChainSyncClientPipelined block (Tip block) m ()
        -- ^ Actual ChainSync client logic
    -> LocalTxSubmissionClient (GenTx block) err m ()
        -- ^ Actual LocalTxSubmission client logic
    -> LocalStateQueryClient block (Query block) m ()
        -- ^ Actual LocalStateQuery client logic
    -> Client m
mkClient tr (epochSlots, securityParam) chainSyncClient txSubmissionClient stateQueryClient =
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
    codecChainSync = cChainSyncCodec $ codecs epochSlots securityParam

    trTxSubmission    = contramap TrTxSubmission tr
    codecTxSubmission = cTxSubmissionCodec $ codecs epochSlots securityParam

    trStateQuery    = nullTracer
    codecStateQuery = cStateQueryCodec $ codecs epochSlots securityParam

nullProtocol
    :: MonadTimer m => RunMiniProtocol 'InitiatorMode ByteString m a Void
nullProtocol = do
    InitiatorProtocolOnly $ MuxPeerRaw $ const $ forever $ threadDelay 43200

localChainSync
    :: forall m block protocol.
        ( block ~ ByronBlock
        , protocol ~ ChainSync block (Tip block)
        , MonadThrow m, MonadAsync m
        )
    => Tracer m (TraceSendRecv protocol)
        -- ^ Base tracer for the mini-protocols
    -> Codec protocol DeserialiseFailure m ByteString
        -- ^ Codec for deserializing / serializing binary data
    -> ChainSyncClientPipelined block (Tip block) m ()
        -- ^ The actual chain sync client
    -> Channel m ByteString
        -- ^ A 'Channel' is a abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m ((), Maybe ByteString)
localChainSync tr codec client channel =
    runPipelinedPeer tr codec channel (chainSyncClientPeerPipelined client)

localTxSubmission
    :: forall m block err protocol.
        ( block ~ ByronBlock
        , err ~ ApplyMempoolPayloadErr
        , protocol ~ LocalTxSubmission (GenTx block) err
        , MonadThrow m
        )
    => Tracer m (TraceSendRecv protocol)
        -- ^ Base tracer for the mini-protocols
    -> Codec protocol DeserialiseFailure m ByteString
        -- ^ Codec for deserializing / serializing binary data
    -> LocalTxSubmissionClient (GenTx block) err m ()
        -- ^ Actual local tx submission client
    -> Channel m ByteString
        -- ^ A 'Channel' is an abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m ((), Maybe ByteString)
localTxSubmission tr codec client channel =
    runPeer tr codec channel (localTxSubmissionClientPeer client)

-- TODO
localStateQuery
    :: forall m block protocol.
        ( block ~ ByronBlock
        , protocol ~ LocalStateQuery block (Query block)
        , MonadThrow m
        )
    => Tracer m (TraceSendRecv protocol)
        -- ^ Base tracer for the mini-protocols
    -> Codec protocol DeserialiseFailure m ByteString
        -- ^ Codec for deserializing / serializing binary data
    -> LocalStateQueryClient block (Query block) m ()
        -- ^ Actual local state query client.
    -> Channel m ByteString
        -- ^ A 'Channel' is an abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m ((), Maybe ByteString)
localStateQuery tr codec client channel =
    runPeer tr codec channel (localStateQueryClientPeer client)

-- | Client codecs for Byron.
codecs
    :: MonadST m
    => EpochSlots
    -> SecurityParam
    -> ClientCodecs ByronBlock m
codecs epochSlots securityParam =
    clientCodecs config ByronNodeToClientVersion2
  where
    config = ByronCodecConfig epochSlots securityParam
