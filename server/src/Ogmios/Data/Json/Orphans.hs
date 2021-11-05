--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.Data.Json.Orphans () where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Crypto
    ( Crypto )
import Cardano.Network.Protocol.NodeToClient.Trace
    ( TraceClient, encodeTraceClient )
import Ogmios.Data.Json
    ( decodePoint
    , decodeSubmitTxPayload
    , decodeTip
    , encodeSubmitTxError
    , encodeSubmitTxPayload
    , encodeTip
    )
import Ogmios.Data.Json.Query
    ( encodePoint )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock, CardanoEras, GenTx (..), HardForkApplyTxErr (..) )
import Ouroboros.Network.Block
    ( Point (..), Tip (..) )
import Shelley.Spec.Ledger.API
    ( PraosCrypto )

import qualified Data.Aeson as Json

--
-- ToJSON
--

-- Only used for logging
instance (Crypto crypto, PraosCrypto crypto) => ToJSON
  ( TraceClient
      (GenTx (CardanoBlock crypto))
      (HardForkApplyTxErr (CardanoEras crypto))
  ) where
    toJSON = encodeTraceClient
        (inefficientEncodingToValue . encodeSubmitTxPayload)
        (inefficientEncodingToValue . encodeSubmitTxError)

-- Only used for logging & health
instance ToJSON (Tip (CardanoBlock crypto)) where
    toJSON = inefficientEncodingToValue . encodeTip
    toEncoding = encodeTip

-- Only used for logging & health
instance ToJSON (Point (CardanoBlock crypto)) where
    toJSON = inefficientEncodingToValue . encodePoint
    toEncoding = encodePoint

--
-- FromJSON
--

instance PraosCrypto crypto => FromJSON (GenTx (CardanoBlock crypto)) where
    parseJSON = decodeSubmitTxPayload

instance Crypto crypto => FromJSON (Point (CardanoBlock crypto)) where
    parseJSON = decodePoint

instance Crypto crypto => FromJSON (Tip (CardanoBlock crypto)) where
    parseJSON = decodeTip
