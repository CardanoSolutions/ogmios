--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Ogmios.Data.Json.Mary where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Crypto
    ( Crypto )
import Data.ByteString.Base16
    ( encodeBase16 )
import GHC.Records
    ( getField )
import Ouroboros.Consensus.Cardano.Block
    ( MaryEra )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..) )
import Shelley.Spec.Ledger.BaseTypes
    ( StrictMaybe (..) )

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Shelley as Shelley

import qualified Cardano.Ledger.AuxiliaryData as MA
import qualified Cardano.Ledger.Era as Era
import qualified Cardano.Ledger.Mary.Value as MA
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as MA
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA
import qualified Shelley.Spec.Ledger.BlockChain as Sh
import qualified Shelley.Spec.Ledger.PParams as Sh
import qualified Shelley.Spec.Ledger.Tx as Sh
import qualified Shelley.Spec.Ledger.UTxO as Sh

--
-- Encoders
--

encodeAuxiliaryData
    :: Crypto crypto
    => MA.AuxiliaryData (MaryEra crypto)
    -> Json
encodeAuxiliaryData (MA.AuxiliaryData blob scripts) = encodeObject
    [ ( "blob"
      , Shelley.encodeMetadataBlob blob
      )
    , ( "scriptPreImages"
      , encodeFoldable Allegra.encodeTimelock scripts
      )
    ]

encodeMaryBlock
    :: Crypto crypto
    => ShelleyBlock (MaryEra crypto)
    -> Json
encodeMaryBlock (ShelleyBlock (Sh.Block blkHeader txs) headerHash) =
    encodeObject
    [ ( "body"
      , encodeFoldable encodeTx (Sh.txSeqTxns' txs)
      )
    , ( "header"
      , Shelley.encodeBHeader blkHeader
      )
    , ( "headerHash"
      , Shelley.encodeShelleyHash headerHash
      )
    ]

encodePParams'
    :: (forall a. (a -> Json) -> Sh.HKD f a -> Json)
    -> Sh.PParams' f era
    -> Json
encodePParams' =
    Shelley.encodePParams'

encodeProposedPPUpdates
    :: Sh.ProposedPPUpdates era
    -> Json
encodeProposedPPUpdates =
    Shelley.encodeProposedPPUpdates

encodeTx
    :: Crypto crypto
    => Sh.Tx (MaryEra crypto)
    -> Json
encodeTx x = encodeObject
    [ ( "id"
      , Shelley.encodeTxId (Sh.txid (Sh._body x))
      )
    , ( "body"
      , encodeTxBody (Sh._body x)
      )
    , ( "witness"
      , encodeWitnessSet (Sh._witnessSet x)
      )
    , ( "metadata", encodeObject
        [ ( "hash"
          , encodeStrictMaybe Shelley.encodeAuxiliaryDataHash (adHash (Sh._body x))
          )
        , ( "body"
          , encodeStrictMaybe encodeAuxiliaryData (Sh._metadata x)
          )
        ]
      )
    ]
  where
    adHash :: MA.TxBody era -> StrictMaybe (MA.AuxiliaryDataHash (Era.Crypto era))
    adHash = getField @"adHash"

encodeTxBody
    :: Crypto crypto
    => MA.TxBody (MaryEra crypto)
    -> Json
encodeTxBody (MA.TxBody inps outs certs wdrls fee validity updates _ _) = encodeObject
    [ ( "inputs"
      , encodeFoldable Shelley.encodeTxIn inps
      )
    , ( "outputs"
      , encodeFoldable encodeTxOut outs
      )
    , ( "certificates"
      , encodeFoldable Shelley.encodeDCert certs
      )
    , ( "withdrawals"
      , Shelley.encodeWdrl wdrls
      )
    , ( "fee"
      , Shelley.encodeCoin fee
      )
    , ( "valitityInterval"
      , Allegra.encodeValidityInterval validity
      )
    , ( "update"
      , encodeStrictMaybe Shelley.encodeUpdate updates
      )
    ]

encodeTxOut
    :: Crypto crypto
    => Sh.TxOut (MaryEra crypto)
    -> Json
encodeTxOut (Sh.TxOut addr value) = encodeObject
    [ ( "address"
      , Shelley.encodeAddress addr
      )
    , ( "value"
      , encodeValue value
      )
    ]

encodeUtxo
    :: Crypto crypto
    => Sh.UTxO (MaryEra crypto)
    -> Json
encodeUtxo =
    encodeList id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO = curry (encode2Tuple Shelley.encodeTxIn encodeTxOut)

encodeValue
    :: MA.Value crypto
    -> Json
encodeValue (MA.Value coins assets) = encodeObject
    [ ( "coins"
      , encodeInteger coins
      )
    , ( "assets"
      , encodeMap stringifyAssetId encodeInteger (flatten assets)
      )
    ]
  where
    flatten :: (Ord k1, Ord k2) => Map k1 (Map k2 a) -> Map (k1, k2) a
    flatten = Map.foldrWithKey
        (\k inner -> Map.union (Map.mapKeys (k,) inner))
        mempty

encodeWitnessSet
    :: Crypto crypto
    => Sh.WitnessSet (MaryEra crypto)
    -> Json
encodeWitnessSet x = encodeObject
    [ ( "address"
      , encodeFoldable Shelley.encodeWitVKey (Sh.addrWits x)
      )
    , ( "script"
      , encodeMap Shelley.stringifyScriptHash Allegra.encodeTimelock (Sh.scriptWits x)
      )
    , ( "bootstrap"
      , encodeFoldable Shelley.encodeBootstrapWitness (Sh.bootWits x)
      )
    ]

--
-- Conversion To Text
--

stringifyAssetId :: (MA.PolicyID crypto, MA.AssetName) -> Text
stringifyAssetId (MA.PolicyID pid, MA.AssetName bytes)
    | BS.null bytes = Shelley.stringifyScriptHash pid
    | otherwise     = Shelley.stringifyScriptHash pid <> "." <> encodeBase16 bytes
