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

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Shelley as Shelley

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as Era

import qualified Shelley.Spec.Ledger.BlockChain as Sh
import qualified Shelley.Spec.Ledger.PParams as Sh
import qualified Shelley.Spec.Ledger.STS.Ledger as Sh
import qualified Shelley.Spec.Ledger.Tx as Sh
import qualified Shelley.Spec.Ledger.UTxO as Sh

import qualified Cardano.Ledger.AuxiliaryData as MA
import qualified Cardano.Ledger.Mary.Value as MA
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as MA
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as MA
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA

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
    , ( "scripts"
      , encodeFoldable Allegra.encodeScript scripts
      )
    ]

encodeBlock
    :: Crypto crypto
    => SerializationMode
    -> ShelleyBlock (MaryEra crypto)
    -> Json
encodeBlock mode (ShelleyBlock (Sh.Block blkHeader txs) headerHash) =
    encodeObject
    [ ( "body"
      , encodeFoldable (encodeTx mode) (Sh.txSeqTxns' txs)
      )
    , ( "header"
      , Shelley.encodeBHeader mode blkHeader
      )
    , ( "headerHash"
      , Shelley.encodeShelleyHash headerHash
      )
    ]

encodeLedgerFailure
    :: Crypto crypto
    => Sh.LedgerPredicateFailure (MaryEra crypto)
    -> Json
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        Shelley.encodeUtxowFailure encodeUtxoFailure e
    Sh.DelegsFailure e ->
        Shelley.encodeDelegsFailure e

encodePolicyId
    :: MA.PolicyID crypto
    -> Json
encodePolicyId (MA.PolicyID hash) =
    Shelley.encodeScriptHash hash

encodePParams'
    :: (forall a. (a -> Json) -> Sh.HKD f a -> Json)
    -> Sh.PParams' f era
    -> Json
encodePParams' =
    Shelley.encodePParams'

encodeProposedPPUpdates
    :: (Core.PParamsDelta era ~ Sh.PParamsUpdate era)
    => Sh.ProposedPPUpdates era
    -> Json
encodeProposedPPUpdates =
    Shelley.encodeProposedPPUpdates

encodeTx
    :: forall crypto. (Crypto crypto)
    => SerializationMode
    -> Sh.Tx (MaryEra crypto)
    -> Json
encodeTx mode x = encodeObjectWithMode mode
    [ ( "id"
      , Shelley.encodeTxId (Sh.txid @(MaryEra crypto) (Sh.body x))
      )
    , ( "body"
      , encodeTxBody (Sh.body x)
      )
    , ( "metadata"
      , (,) <$> fmap (("hash",) . Shelley.encodeAuxiliaryDataHash) (adHash (Sh.body x))
            <*> fmap (("body",) . encodeAuxiliaryData) (Sh.auxiliaryData x)
        & encodeStrictMaybe (\(a, b) -> encodeObject [a,b])
      )
    ]
    [ ( "witness"
      , encodeWitnessSet (Sh.wits x)
      )
    ]
  where
    adHash :: MA.TxBody era -> StrictMaybe (MA.AuxiliaryDataHash (Era.Crypto era))
    adHash = getField @"adHash"

encodeTxBody
    :: Crypto crypto
    => MA.TxBody (MaryEra crypto)
    -> Json
encodeTxBody (MA.TxBody inps outs certs wdrls fee validity updates _ mint) = encodeObject
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
      , encodeCoin fee
      )
    , ( "validityInterval"
      , Allegra.encodeValidityInterval validity
      )
    , ( "update"
      , encodeStrictMaybe Shelley.encodeUpdate updates
      )
    , ( "mint"
      , encodeValue mint
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

encodeUtxoFailure
    :: Crypto crypto
    => MA.UtxoPredicateFailure (MaryEra crypto)
    -> Json
encodeUtxoFailure = \case
    MA.BadInputsUTxO inputs ->
        encodeObject
            [ ( "badInputs"
              , encodeFoldable Shelley.encodeTxIn inputs
              )
            ]
    MA.OutsideValidityIntervalUTxO itv currentSlot ->
        encodeObject
            [ ( "outsideOfValidityInterval", encodeObject
                [ ( "interval" , Allegra.encodeValidityInterval itv )
                , ( "currentSlot" , encodeSlotNo currentSlot )
                ]
              )
            ]
    MA.OutputTooBigUTxO outs ->
        encodeObject
            [ ( "tooManyAssetsInOutput"
              , encodeFoldable encodeTxOut outs
              )
            ]
    MA.MaxTxSizeUTxO actualSize maxSize ->
        encodeObject
            [ ( "txTooLarge", encodeObject
                [ ( "maximumSize", encodeInteger maxSize )
                , ( "actualSize", encodeInteger actualSize )
                ]
              )
            ]
    MA.InputSetEmptyUTxO ->
        encodeText "missingAtLeastOneInputUtxo"
    MA.FeeTooSmallUTxO required actual ->
        encodeObject
            [ ( "feeTooSmall", encodeObject
                [ ( "requiredFee", encodeCoin required )
                , ( "actualFee", encodeCoin actual )
                ]
              )
            ]
    MA.ValueNotConservedUTxO consumed produced ->
        encodeObject
            [ ( "valueNotConserved", encodeObject
                [ ( "consumed", encodeValue consumed )
                , ( "produced", encodeValue produced )
                ]
              )
            ]
    MA.WrongNetwork expected invalidAddrs ->
        encodeObject
            [ ( "networkMismatch", encodeObject
                [ ( "expectedNetwork"
                  , Shelley.encodeNetwork expected
                  )
                , ( "invalidEntities"
                  , Shelley.encodeEntities "address" Shelley.encodeAddress invalidAddrs
                  )
                ]
              )
            ]
    MA.WrongNetworkWithdrawal expected invalidAccts ->
        encodeObject
            [ ( "networkMismatch", encodeObject
                [ ( "expectedNetwork"
                  , Shelley.encodeNetwork expected
                  )
                , ( "invalidEntities"
                  , Shelley.encodeEntities "rewardAccount" Shelley.encodeRewardAcnt invalidAccts
                  )
                ]
              )
            ]
    MA.OutputTooSmallUTxO outs ->
        encodeObject
            [ ( "outputTooSmall"
              , encodeFoldable encodeTxOut outs
              )
            ]
    MA.OutputBootAddrAttrsTooBig outs ->
        encodeObject
            [ ( "addressAttributesTooLarge"
              , encodeFoldable Shelley.encodeAddress ((\(Sh.TxOut addr _) -> addr) <$> outs)
              )
            ]
    MA.TriesToForgeADA ->
        encodeString "triesToForgeAda"
    MA.UpdateFailure e ->
        Shelley.encodeUpdateFailure e

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
    [ ( "signatures"
      , encodeFoldable Shelley.encodeWitVKey (Sh.addrWits x)
      )
    , ( "scripts"
      , encodeMap Shelley.stringifyScriptHash Allegra.encodeScript (Sh.scriptWits x)
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
