--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE LambdaCase #-}

module Cardano.Network.Protocol.NodeToClient.Trace
    ( TraceClient (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
import Codec.CBOR.Term
    ( Term )
import Network.Mux
    ( MuxTrace, WithMuxBearer )
import Ouroboros.Network.Block
    ( Tip )
import Ouroboros.Network.Driver.Simple
    ( TraceSendRecv )
import Ouroboros.Network.NodeToClient
    ( ConnectionId (..), Handshake, LocalAddress, NodeToClientVersion )
import Ouroboros.Network.Protocol.ChainSync.Type
    ( ChainSync )

type HandshakeTrace = TraceSendRecv (Handshake NodeToClientVersion Term)

data TraceClient block
    = TrChainSync (TraceSendRecv (ChainSync block (Tip block)))
    | TrMux (WithMuxBearer (ConnectionId LocalAddress) MuxTrace)
    | TrHandshake (WithMuxBearer (ConnectionId LocalAddress) HandshakeTrace)
    deriving (Show)

instance DefinePrivacyAnnotation (TraceClient block)
instance DefineSeverity (TraceClient block) where
    defineSeverity = \case
        TrChainSync{}    -> Debug
        TrMux{}          -> Debug
        TrHandshake{}    -> Debug
