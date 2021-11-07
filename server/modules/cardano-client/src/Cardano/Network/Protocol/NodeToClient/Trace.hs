--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Cardano.Network.Protocol.NodeToClient.Trace
    ( TraceClient (..)
    , encodeTraceClient
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Codec.CBOR.Term
    ( Term )
import Data.Aeson
    ( (.=) )
import GHC.Generics
    ( Generic )
import Network.Mux
    ( WithMuxBearer (..) )
import Network.TypedProtocol.Codec
    ( AnyMessageAndAgency (..) )
import Ouroboros.Network.Driver.Simple
    ( TraceSendRecv (..) )
import Ouroboros.Network.NodeToClient
    ( ConnectionId (..), LocalAddress, NodeToClientVersion )
import Ouroboros.Network.Protocol.Handshake.Type
    ( Handshake, Message (..), RefuseReason (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( LocalTxSubmission, Message (..) )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Map.Strict as Map

type HandshakeTrace = TraceSendRecv (Handshake NodeToClientVersion Term)

data TraceClient tx err
    = TrTxSubmission (TraceSendRecv (LocalTxSubmission tx err))
    | TrHandshake (WithMuxBearer (ConnectionId LocalAddress) HandshakeTrace)
    deriving (Generic, Show)

encodeTraceClient
    :: forall tx err. ()
    => (tx -> Json.Value)
    -> (err -> Json.Value)
    -> TraceClient tx err
    -> Json.Value
encodeTraceClient encodeTx encodeErr = \case
    TrTxSubmission tr ->
        Json.object (("tag" .= Json.String "TxSubmission")
            : encodeTraceSendRecvTxSubmission tr
        )
    TrHandshake tr ->
        Json.object (("tag" .= Json.String "Handshake")
            : encodeTraceSendRecvHandshake tr
        )
  where
    encodeTraceSendRecvTxSubmission
        :: TraceSendRecv (LocalTxSubmission tx err)
        -> [Json.Pair]
    encodeTraceSendRecvTxSubmission = \case
        TraceSendMsg (AnyMessageAndAgency agency msg) ->
            [ "event" .= ("send" :: String)
            , "agency" .= show agency
            ] ++ encodeMsg msg
        TraceRecvMsg (AnyMessageAndAgency agency msg) ->
            [ "event" .= ("receive" :: String)
            , "agency" .= show agency
            ] ++ encodeMsg msg
      where
        encodeMsg
            :: Message (LocalTxSubmission tx err) from to
            -> [Json.Pair]
        encodeMsg = \case
            MsgSubmitTx tx ->
                [ "tag" .= ("SubmitTx" :: String)
                , "tx" .= encodeTx tx
                ]
            MsgAcceptTx ->
                [ "tag" .= ("AcceptTx" :: String)
                ]
            MsgRejectTx err ->
                [ "tag" .= ("RejectTx" :: String)
                , "reasons" .= encodeErr err
                ]
            MsgDone ->
                [ "tag" .= ("Done" :: String)
                ]

    encodeTraceSendRecvHandshake
        :: WithMuxBearer (ConnectionId LocalAddress) HandshakeTrace
        -> [Json.Pair]
    encodeTraceSendRecvHandshake = \case
        WithMuxBearer _peerId (TraceSendMsg (AnyMessageAndAgency agency msg)) ->
            [ "event" .= ("send" :: String)
            , "agency" .= show agency
            ] ++ encodeMsg msg
        WithMuxBearer _peerId (TraceRecvMsg (AnyMessageAndAgency agency msg)) ->
            [ "event" .= ("receive" :: String)
            , "agency" .= show agency
            ] ++ encodeMsg msg
      where
        encodeMsg
            :: Message (Handshake NodeToClientVersion Term) from to
            -> [Json.Pair]
        encodeMsg = \case
            MsgProposeVersions versions ->
                [ "tag" .= ("ProposeVersions" :: String)
                , "versions" .= (show <$> Map.keys versions)
                ]
            MsgReplyVersions versions ->
                [ "tag" .= ("ReplyVersions" :: String)
                , "versions" .= (show <$> Map.keys versions)
                ]
            MsgAcceptVersion v _ ->
                [ "tag" .= ("AcceptVersion" :: String)
                , "version" .= show (show v)
                ]
            MsgRefuse reason ->
                [ "tag" .= ("RefuseVersions" :: String)
                , "reason" .= encodeRefuseReason reason
                ]

        encodeRefuseReason
            :: RefuseReason vNumber
            -> Json.Value
        encodeRefuseReason = \case
            VersionMismatch{} -> Json.String "VersionMismatchOrUnknown"
            HandshakeDecodeError{} -> Json.String "HandshakeDecodeError"
            Refused{} -> Json.String "ServerRejected"

instance HasPrivacyAnnotation (TraceClient tx err)
instance HasSeverityAnnotation (TraceClient tx err) where
    getSeverityAnnotation = \case
        TrTxSubmission{} -> Info
        TrHandshake{}    -> Info
