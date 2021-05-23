--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Ogmios.Data.Json.Alonzo where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Crypto
    ( Crypto )
import Data.ByteString.Base16
    ( encodeBase16 )
import Data.MemoBytes
    ( memobytes )
import GHC.Records
    ( getField )
import Ouroboros.Consensus.Cardano.Block
    ( AlonzoEra )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..) )
import Shelley.Spec.Ledger.BaseTypes
    ( StrictMaybe (..) )

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Ledger.Era as Era
import qualified Cardano.Ledger.SafeHash as SafeHash

import qualified Shelley.Spec.Ledger.API as Spec
import qualified Shelley.Spec.Ledger.BlockChain as Spec
import qualified Shelley.Spec.Ledger.PParams as Spec
import qualified Shelley.Spec.Ledger.Tx as Spec
import qualified Shelley.Spec.Ledger.UTxO as Spec

import qualified Cardano.Ledger.Alonzo.Data as Al
import qualified Cardano.Ledger.Alonzo.Language as Al
import qualified Cardano.Ledger.Alonzo.PlutusScriptApi as Al
import qualified Cardano.Ledger.Alonzo.PParams as Al
import qualified Cardano.Ledger.Alonzo.Rules.Bbody as Al
import qualified Cardano.Ledger.Alonzo.Rules.Ledger as Al
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Al
import qualified Cardano.Ledger.Alonzo.Rules.Utxos as Al
import qualified Cardano.Ledger.Alonzo.Rules.Utxow as Al
import qualified Cardano.Ledger.Alonzo.Scripts as Al
import qualified Cardano.Ledger.Alonzo.Translation as Al
import qualified Cardano.Ledger.Alonzo.Tx as Al
import qualified Cardano.Ledger.Alonzo.TxBody as Al
import qualified Cardano.Ledger.Alonzo.TxInfo as Al
import qualified Cardano.Ledger.Alonzo.TxSeq as Al
import qualified Cardano.Ledger.Alonzo.TxWitness as Al

--
-- Encoders
--

encodeAlonzoBlock
    :: Crypto crypto
    => SerializationMode
    -> ShelleyBlock (AlonzoEra crypto)
    -> Json
encodeAlonzoBlock mode (ShelleyBlock (Spec.Block blkHeader txs) headerHash) =
    encodeObject
    [ ( "body"
      , encodeFoldable (encodeTx mode) (Al.txSeqTxns txs)
      )
    , ( "header"
      , Shelley.encodeBHeader mode blkHeader
      )
    , ( "headerHash"
      , Shelley.encodeShelleyHash headerHash
      )
    ]

encodeAuxiliaryData
    :: Crypto crypto
    => Al.AuxiliaryData (AlonzoEra crypto)
    -> Json
encodeAuxiliaryData (Al.AuxiliaryData blob scripts datums) = encodeObject
    [ ( "blob"
      , Shelley.encodeMetadataBlob blob
      )
    , ( "scripts"
      , encodeFoldable encodeScript scripts
      )
    , ( "datums"
      , encodeFoldable encodeData datums
      )
    ]

encodeCostModel
    :: Al.CostModel
    -> Json
encodeCostModel (Al.CostModel model) =
    encodeMap id encodeInteger model

encodeData
    :: Al.Data era
    -> Json
encodeData (Al.DataConstr datum) =
    -- TODO: Check whether 'memobytes' really is what we want here. Might be good
    -- to strip away the extra CBOR wrapping this if any.
    encodeShortByteString encodeByteStringBase64 (memobytes datum)

encodeExUnits
    :: Al.ExUnits
    -> Json
encodeExUnits units =  encodeObject
    [ ( "memory", encodeWord64 (Al.exUnitsMem units) )
    , ( "steps", encodeWord64 (Al.exUnitsSteps units) )
    ]

encodePParams'
    :: (forall a. (a -> Json) -> Spec.HKD f a -> Json)
    -> Al.PParams' f era
    -> Json
encodePParams' encodeF x = encodeObject
    [ ( "minFeeCoefficient"
      , encodeF encodeNatural (Al._minfeeA x)
      )
    , ( "minFeeConstant"
      , encodeF encodeNatural (Al._minfeeB x)
      )
    , ( "maxBlockBodySize"
      , encodeF encodeNatural (Al._maxBBSize x)
      )
    , ( "maxBlockHeaderSize"
      , encodeF encodeNatural (Al._maxBHSize x)
      )
    , ( "maxTxSize"
      , encodeF encodeNatural (Al._maxTxSize x)
      )
    , ( "stakeKeyDeposit"
      , encodeF Shelley.encodeCoin (Al._keyDeposit x)
      )
    , ( "poolDeposit"
      , encodeF Shelley.encodeCoin (Al._poolDeposit x)
      )
    , ( "poolRetirementEpochBound"
      , encodeF encodeEpochNo (Al._eMax x)
      )
    , ( "desiredNumberOfPools"
      , encodeF encodeNatural (Al._nOpt x)
      )
    , ( "poolInfluence"
      , encodeF encodeRational (Al._a0 x)
      )
    , ( "monetaryExpansion"
      , encodeF encodeUnitInterval (Al._rho x)
      )
    , ( "treasuryExpansion"
      , encodeF encodeUnitInterval (Al._tau x)
      )
    , ( "decentralizationParameter"
      , encodeF encodeUnitInterval (Al._d x)
      )
    , ( "extraEntropy"
      , encodeF Shelley.encodeNonce (Al._extraEntropy x)
      )
    , ( "protocolVersion"
      , encodeF Shelley.encodeProtVer (Al._protocolVersion x)
      )
    , ( "minPoolCost"
      , encodeF Shelley.encodeCoin (Al._minPoolCost x)
      )
    , ( "adaPerUtxoWord"
      , encodeF Shelley.encodeCoin (Al._adaPerUTxOWord x)
      )
    , ( "costModels"
      , encodeF (encodeMap stringifyLanguage encodeCostModel) (Al._costmdls x)
      )
    , ( "prices"
      , encodeF encodePrices (Al._prices x)
      )
    , ( "maxExecutionUnitsPerTransaction"
      , encodeF encodeExUnits (Al._maxTxExUnits x)
      )
    , ( "maxExecutionUnitsPerBlock"
      , encodeF encodeExUnits (Al._maxBlockExUnits x)
      )
    , ( "maxValueSize"
      , encodeF encodeNatural (Al._maxValSize x)
      )
    , ( "collateralPercentage"
      , encodeF encodeNatural (Al._collateralPercentage x)
      )
    , ( "maxCollateralInputs"
      , encodeF encodeNatural (Al._maxCollateralInputs x)
      )
    ]

encodePrices
    :: Al.Prices
    -> Json
encodePrices prices =  encodeObject
    [ ( "memory", Shelley.encodeCoin (Al.prMem prices) )
    , ( "steps", Shelley.encodeCoin (Al.prSteps prices) )
    ]

encodeProposedPPUpdates
    :: Spec.ProposedPPUpdates (AlonzoEra crypto)
    -> Json
encodeProposedPPUpdates (Spec.ProposedPPUpdates m) =
    encodeMap Shelley.stringifyKeyHash (encodePParams' encodeStrictMaybe) m

encodeRedeemers
    :: Crypto crypto
    => Al.Redeemers (AlonzoEra crypto)
    -> Json
encodeRedeemers (Al.Redeemers redeemers) =
    encodeMap stringifyRdmrPtr encodeDataAndUnits redeemers
  where
    encodeDataAndUnits
        :: (Al.Data era, Al.ExUnits)
        -> Json
    encodeDataAndUnits (redeemer, units) = encodeObject
        [ ( "redeemer", encodeData redeemer )
        , ( "executionUnits", encodeExUnits units )
        ]

encodeScript
    :: Crypto crypto
    => Al.Script (AlonzoEra crypto)
    -> Json
encodeScript = \case
    Al.TimelockScript nativeScript -> encodeObject
        [ ( "native"
          , Allegra.encodeTimelock nativeScript
          )
        ]
    Al.PlutusScript serializedScript -> encodeObject
        [ ( "plutus"
          , encodeShortByteString encodeByteStringBase64 serializedScript
          )
        ]

encodeTx
    :: forall crypto. Crypto crypto
    => SerializationMode
    -> Al.ValidatedTx (AlonzoEra crypto)
    -> Json
encodeTx mode x = encodeObjectWithMode mode
    [ ( "id"
      , Shelley.encodeTxId (Spec.txid @(AlonzoEra crypto) (Al.body x))
      )
    , ( "body"
      , encodeTxBody (Al.body x)
      )
    , ( "metadata", encodeObject
        [ ( "hash"
          , encodeStrictMaybe Shelley.encodeAuxiliaryDataHash (adHash (Al.body x))
          )
        , ( "body"
          , encodeStrictMaybe encodeAuxiliaryData (Al.auxiliaryData x)
          )
        ]
      )
    ]
    [ ( "witness"
      , encodeWitnessSet (Al.wits x)
      )
    ]
  where
    adHash :: Al.TxBody era -> StrictMaybe (Al.AuxiliaryDataHash (Era.Crypto era))
    adHash = getField @"adHash"

encodeTxBody
    :: Crypto crypto
    => Al.TxBody (AlonzoEra crypto)
    -> Json
encodeTxBody x = encodeObject
    [ ( "inputs"
      , encodeFoldable Shelley.encodeTxIn (Al.inputs x)
      )
    , ( "collateral"
      , encodeFoldable Shelley.encodeTxIn (Al.collateral x)
      )
    , ( "outputs"
      , encodeFoldable encodeTxOut (Al.outputs x)
      )
    , ( "certificates"
      , encodeFoldable Shelley.encodeDCert (Al.txcerts x)
      )
    , ( "withdrawals"
      , Shelley.encodeWdrl (Al.txwdrls x)
      )
    , ( "fee"
      , Shelley.encodeCoin (Al.txfee x)
      )
    , ( "validityInterval"
      , Allegra.encodeValidityInterval (Al.txvldt x)
      )
    , ( "update"
      , encodeStrictMaybe encodeUpdate (Al.txUpdates x)
      )
    , ( "mint"
      , Mary.encodeValue (Al.mint x)
      )
    , ( "network"
      , encodeStrictMaybe Shelley.encodeNetwork (Al.txnetworkid x)
      )
    , ( "requiredExtraData"
      , encodeStrictMaybe (Shelley.encodeHash . SafeHash.extractHash) (Al.wppHash x)
      )
    , ( "requiredExtraSignatures"
      , encodeFoldable Shelley.encodeKeyHash (Al.reqSignerHashes x)
      )
    ]

encodeTxOut
    :: Crypto crypto
    => Al.TxOut (AlonzoEra crypto)
    -> Json
encodeTxOut (Al.TxOut addr value datum) = encodeObject
    [ ( "address"
      , Shelley.encodeAddress addr
      )
    , ( "value"
      , Mary.encodeValue value
      )
    , ( "datum"
      , encodeStrictMaybe (Shelley.encodeHash . SafeHash.extractHash) datum
      )
    ]

encodeUpdate
    :: Spec.Update (AlonzoEra crypto)
    -> Json
encodeUpdate (Spec.Update update epoch) = encodeObject
    [ ( "proposal"
      , encodeProposedPPUpdates update
      )
    , ( "epoch"
      , encodeEpochNo epoch
      )
    ]

encodeWitnessSet
    :: Crypto crypto
    => Al.TxWitness (AlonzoEra crypto)
    -> Json
encodeWitnessSet x = encodeObject
    [ ( "signatures"
      , encodeFoldable Shelley.encodeWitVKey (Al.txwitsVKey x)
      )
    , ( "scripts"
      , encodeMap Shelley.stringifyScriptHash encodeScript (Al.txscripts x)
      )
    , ( "datums"
      , encodeMap stringifyDataHash encodeData (Al.txdats x)
      )
    , ( "redeemers"
      , encodeRedeemers (Al.txrdmrs x)
      )
    , ( "bootstrap"
      , encodeFoldable Shelley.encodeBootstrapWitness (Al.txwitsBoot x)
      )
    ]

--
-- Conversion To Text
--

stringifyDataHash
    :: Al.DataHash crypto
    -> Text
stringifyDataHash (SafeHash.extractHash -> (CC.UnsafeHash h)) =
    encodeBase16 (fromShort h)

stringifyLanguage
    :: Al.Language
    -> Text
stringifyLanguage = \case
    Al.PlutusV1 -> "plutus:v1"

stringifyRdmrPtr
    :: Al.RdmrPtr
    -> Text
stringifyRdmrPtr (Al.RdmrPtr tag ptr) =
    stringifyTag tag <> ":" <> show ptr
  where
    stringifyTag
        :: Al.Tag
        -> Text
    stringifyTag = \case
        Al.Spend -> "utxo"
        Al.Mint -> "mint"
        Al.Cert -> "certificate"
        Al.Rewrd -> "withdrawal"
