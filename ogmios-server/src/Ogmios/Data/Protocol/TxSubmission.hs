--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- NOTE:
-- This module uses partial record field accessor to automatically derive
-- JSON instances from the generic data-type structure. The partial fields are
-- otherwise unused.
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

-- NOTE:
-- Needed to derive 'ToJSON' and 'Show' instances for 'SubmitResult'.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.Data.Protocol.TxSubmission
    ( -- * Codecs
      TxSubmissionCodecs (..)
    , mkTxSubmissionCodecs

      -- * Messages
    , TxSubmissionMessage (..)

      -- ** SubmitTx
    , SubmitTx (..)
    , SubmitTxResponse
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Network.Protocol.NodeToClient
    ( SubmitTxError, SubmitTxPayload )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..) )

import qualified Codec.Json.Wsp as Wsp
import qualified Codec.Json.Wsp.Handler as Wsp

--
-- Codecs
--

data TxSubmissionCodecs block = TxSubmissionCodecs
    { decodeSubmitTx
        :: ByteString
        -> Maybe (Wsp.Request (SubmitTx block))
    , encodeSubmitTxResponse
        :: Wsp.Response (SubmitTxResponse block)
        -> Json
    }

mkTxSubmissionCodecs
    :: forall block. (FromJSON (SubmitTxPayload block))
    => (SubmitTxError block -> Json)
    -> TxSubmissionCodecs block
mkTxSubmissionCodecs encodeSubmitTxError =
    TxSubmissionCodecs
        { decodeSubmitTx =
            _decodeSubmitTx
        , encodeSubmitTxResponse =
            _encodeSubmitTxResponse (Proxy @block) encodeSubmitTxError
        }

--
-- Messages
--

data TxSubmissionMessage block
    = MsgSubmitTx
        (SubmitTx block)
        (Wsp.ToResponse (SubmitTxResponse block))

--
-- SubmitTx
--

data SubmitTx block
    = SubmitTx { bytes :: SubmitTxPayload block }
    deriving (Generic)
deriving instance Show (SubmitTxPayload block) => Show (SubmitTx block)

_decodeSubmitTx
    :: FromJSON (SubmitTxPayload block)
    => ByteString
    -> Maybe (Wsp.Request (SubmitTx block))
_decodeSubmitTx =
    decodeWith (Wsp.genericFromJSON Wsp.defaultOptions)

type SubmitTxResponse block = SubmitResult (SubmitTxError block)

_encodeSubmitTxResponse
    :: Proxy block
    -> (SubmitTxError block -> Json)
    -> Wsp.Response (SubmitTxResponse block)
    -> Json
_encodeSubmitTxResponse _proxy encodeSubmitTxError = \case
    Wsp.Response _ SubmitSuccess ->
        encodeText "SubmitSuccess"
    Wsp.Response _ (SubmitFail e) ->
        encodeObject [ ( "SubmitFail", encodeSubmitTxError e ) ]
