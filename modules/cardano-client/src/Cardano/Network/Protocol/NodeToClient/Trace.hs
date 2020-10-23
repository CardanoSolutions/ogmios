--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Cardano.Network.Protocol.NodeToClient.Trace
    ( TraceClient (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Codec.CBOR.Term
    ( Term )
import Network.Mux
    ( MuxTrace, WithMuxBearer )
import Ouroboros.Network.Driver.Simple
    ( TraceSendRecv )
import Ouroboros.Network.NodeToClient
    ( ConnectionId (..), Handshake, LocalAddress, NodeToClientVersion )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( LocalTxSubmission )

type HandshakeTrace = TraceSendRecv (Handshake NodeToClientVersion Term)

data TraceClient tx err
    = TrTxSubmission (TraceSendRecv (LocalTxSubmission tx err))
    | TrMux (WithMuxBearer (ConnectionId LocalAddress) MuxTrace)
    | TrHandshake (WithMuxBearer (ConnectionId LocalAddress) HandshakeTrace)
    deriving (Show)

instance HasPrivacyAnnotation (TraceClient tx err)
instance HasSeverityAnnotation (TraceClient tx err) where
    getSeverityAnnotation = \case
        TrTxSubmission{} -> Info
        TrMux{}          -> Debug
        TrHandshake{}    -> Debug
