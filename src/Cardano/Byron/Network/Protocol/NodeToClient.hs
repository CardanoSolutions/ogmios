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

module Cardano.Byron.Network.Protocol.NodeToClient
    (
    -- * Building
      Client
    , mkClient

    -- * Connecting
    , connectClient
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
import Control.Monad.Class.MonadAsync
    ( MonadAsync )
import Control.Monad.Class.MonadST
    ( MonadST )
import Control.Monad.Class.MonadThrow
    ( MonadThrow )
import Control.Monad.IO.Class
    ( MonadIO )
import Control.Tracer
    ( Tracer (..), contramap, nullTracer )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Proxy
    ( Proxy (..) )
import Data.Void
    ( Void )
import Network.Mux
    ( AppType (..) )
import Network.Mux.Types
    ( MiniProtocolLimits (..), MiniProtocolNum (..) )
import Network.TypedProtocol.Codec
    ( Codec )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock (..)
    , GenTx
    , decodeByronBlock
    , decodeByronGenTx
    , decodeByronHeaderHash
    , encodeByronBlock
    , encodeByronGenTx
    , encodeByronHeaderHash
    )
import Ouroboros.Consensus.Byron.Node
    ()
import Ouroboros.Consensus.Node.Run
    ( nodeDecodeApplyTxError, nodeEncodeApplyTxError )
import Ouroboros.Network.Block
    ( Tip (..)
    , decodePoint
    , decodeTip
    , encodePoint
    , encodeTip
    , unwrapCBORinCBOR
    )
import Ouroboros.Network.Channel
    ( Channel )
import Ouroboros.Network.Codec
    ( DeserialiseFailure )
import Ouroboros.Network.Driver.Simple
    ( TraceSendRecv, runPeer, runPipelinedPeer )
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MuxPeer (..)
    , OuroborosApplication (..)
    , RunMiniProtocol (..)
    )
import Ouroboros.Network.NodeToClient
    ( LocalAddress
    , NetworkConnectTracers (..)
    , NodeToClientVersion (..)
    , connectTo
    , localSnocket
    , withIOManager
    )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined, chainSyncClientPeerPipelined )
import Ouroboros.Network.Protocol.ChainSync.Codec
    ( codecChainSync )
import Ouroboros.Network.Protocol.ChainSync.Type
    ( ChainSync )
import Ouroboros.Network.Protocol.Handshake.Version
    ( DictVersion (..), simpleSingletonVersions )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxSubmissionClient, localTxSubmissionClientPeer )
import Ouroboros.Network.Protocol.LocalTxSubmission.Codec
    ( codecLocalTxSubmission )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( LocalTxSubmission )
import Ouroboros.Network.Socket
    ( ConnectionId (..) )

-- | Type representing a network client running two mini-protocols to sync
-- from the chain and, submit transactions.
type Client m = OuroborosApplication
    'InitiatorApp
        -- Initiator ~ Client (as opposed to Responder / Server)
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
    :: Tracer IO (TraceClient block)
    -> (ConnectionId LocalAddress -> Client IO)
    -> NodeVersionData
    -> FilePath
    -> IO ()
connectClient tr client (vData, vCodec) addr = withIOManager $ \iocp -> do
    let vDict = DictVersion vCodec
    let versions = simpleSingletonVersions NodeToClientV_1 vData vDict client
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
    => Tracer m (TraceClient block)
        -- ^ Base trace for underlying protocols
    -> EpochSlots
        -- ^ Static blockchain parameters
    -> ChainSyncClientPipelined block (Tip block) m ()
        -- ^ Actual ChainSync client logic
    -> LocalTxSubmissionClient (GenTx block) err m ()
        -- ^ Actual LocalTxSubmission client logic
    -> ConnectionId LocalAddress
        -- ^ Connection identifier attributed by the protocol
    -> Client m
mkClient tr epochSlots localChainSyncClient localTxSubmissionClient _ =
    OuroborosApplication
        [ MiniProtocol
            { miniProtocolNum    = MiniProtocolNum 5
            , miniProtocolLimits = maximumMiniProtocolLimits
            , miniProtocolRun    = InitiatorProtocolOnly $
                MuxPeerRaw $ chainSyncWithBlocks trChainSync epochSlots localChainSyncClient
            }
        , MiniProtocol
            { miniProtocolNum    = MiniProtocolNum 6
            , miniProtocolLimits = maximumMiniProtocolLimits
            , miniProtocolRun    = InitiatorProtocolOnly $
                MuxPeerRaw $ localTxSubmission trTxSubmission localTxSubmissionClient
            }
        ]
  where
    trChainSync    = contramap TrChainSync tr
    trTxSubmission = nullTracer
    maximumMiniProtocolLimits =
        MiniProtocolLimits { maximumIngressQueue = 0xffffffff }

chainSyncWithBlocks
    :: forall m block protocol.
        ( block ~ ByronBlock
        , protocol ~ ChainSync block (Tip block)
        , MonadThrow m, MonadST m, MonadAsync m
        )
    => Tracer m (TraceSendRecv protocol)
        -- ^ Base tracer for the mini-protocols
    -> EpochSlots
        -- ^ Blockchain parameters
    -> ChainSyncClientPipelined block (Tip block) m ()
        -- ^ The actual chain sync client
    -> Channel m ByteString
        -- ^ A 'Channel' is a abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m ()
chainSyncWithBlocks tr epochSlots client channel =
    runPipelinedPeer tr codec channel (chainSyncClientPeerPipelined client)
  where
    codec :: Codec protocol DeserialiseFailure m ByteString
    codec =
        codecChainSync
          encodeByronBlock
          (unwrapCBORinCBOR $ decodeByronBlock epochSlots)
          (encodePoint encodeByronHeaderHash)
          (decodePoint decodeByronHeaderHash)
          (encodeTip encodeByronHeaderHash)
          (decodeTip decodeByronHeaderHash)

localTxSubmission
    :: forall m block err protocol.
        ( block ~ ByronBlock
        , err ~ ApplyMempoolPayloadErr
        , protocol ~ LocalTxSubmission (GenTx block) err
        , MonadThrow m, MonadST m
        )
    => Tracer m (TraceSendRecv protocol)
        -- ^ Base tracer for the mini-protocols
    -> LocalTxSubmissionClient (GenTx block) err m ()
        -- ^ Actual local tx submission client
    -> Channel m ByteString
        -- ^ A 'Channel' is a abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m ()
localTxSubmission tr client channel =
    runPeer tr codec channel (localTxSubmissionClientPeer client)
  where
    codec :: Codec protocol DeserialiseFailure m ByteString
    codec = codecLocalTxSubmission
        encodeByronGenTx -- Tx -> Cbor.Encoding
        decodeByronGenTx -- Cbor.Decoder s Tx
        (nodeEncodeApplyTxError (Proxy @block)) -- err -> Cbor.Encoding
        (nodeDecodeApplyTxError (Proxy @block)) -- Cbor.Decoder s err
