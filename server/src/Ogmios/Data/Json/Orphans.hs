--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.Data.Json.Orphans () where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Shelley.UTxO
    ( UTxO (..)
    )
import Cardano.Network.Protocol.NodeToClient
    ( GenTx
    )
import Cardano.Network.Protocol.NodeToClient.Trace
    ( TraceClient
    , encodeTraceClient
    )
import Ogmios.Data.EraTranslation
    ( MultiEraUTxO (..)
    )
import Ogmios.Data.Json
    ( decodePoint
    , decodeSerializedTransaction
    , decodeTip
    , decodeUtxo
    , encodeSerializedTransaction
    , encodeSubmitTransactionError
    , encodeTip
    )
import Ogmios.Data.Json.Query
    ( encodePoint
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    , HardForkApplyTxErr (..)
    )
import Ouroboros.Consensus.Protocol.Praos
    ( PraosCrypto
    )
import Ouroboros.Network.Block
    ( Point (..)
    , Tip (..)
    )

import qualified Cardano.Protocol.TPraos.API as TPraos
import qualified Data.Aeson as Json

--
-- ToJSON
--

-- Only used for logging
instance
    ( Crypto crypto
    , PraosCrypto crypto
    , TPraos.PraosCrypto crypto
    ) =>
    ToJSON
      ( TraceClient
          (GenTx (CardanoBlock crypto))
          (HardForkApplyTxErr (CardanoEras crypto))
      )
  where
    toJSON = encodeTraceClient
        (inefficientEncodingToValue . encodeSerializedTransaction)
        (inefficientEncodingToValue . encodeSubmitTransactionError (\_ _ -> fromMaybe (encodeObject mempty)))

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

instance
    ( TPraos.PraosCrypto crypto
    , PraosCrypto crypto
    ) =>
    FromJSON (MultiEraDecoder (GenTx (CardanoBlock crypto)))
  where
    parseJSON = Json.withObject "CBOR" $ \o ->
        o .: "cbor" >>= decodeSerializedTransaction

instance Crypto crypto => FromJSON (MultiEraUTxO (CardanoBlock crypto)) where
    parseJSON = decodeUtxo

instance Crypto crypto => FromJSON (Point (CardanoBlock crypto)) where
    parseJSON = decodePoint

instance Crypto crypto => FromJSON (Tip (CardanoBlock crypto)) where
    parseJSON = decodeTip

--
-- Monoid / Semigroup
--

deriving newtype instance Monoid (UTxO (AlonzoEra crypto))
deriving newtype instance Monoid (UTxO (BabbageEra crypto))
