--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Ogmios.Data.Json.Babbage where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( serialize'
    )
import Cardano.Ledger.Crypto
    ( Crypto
    )
import GHC.Records
    ( getField
    )
import Ouroboros.Consensus.Cardano.Block
    ( BabbageEra
    )
import Ouroboros.Consensus.Protocol.Praos
    ( Praos
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )
import Ouroboros.Consensus.Shelley.Protocol.Praos
    ()

import qualified Data.Map.Strict as Map

import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos

import qualified Cardano.Ledger.AuxiliaryData as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import qualified Cardano.Ledger.Shelley.API as Ledger.Shelley
import qualified Cardano.Ledger.Shelley.PParams as Ledger.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxSeq as Ledger.Alonzo

import qualified Cardano.Ledger.Babbage.PParams as Ledger.Babbage
import qualified Cardano.Ledger.Babbage.Rules.Utxo as Ledger.Babbage
import qualified Cardano.Ledger.Babbage.Rules.Utxow as Ledger.Babbage
import qualified Cardano.Ledger.Babbage.Tx as Ledger.Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Ledger.Babbage

import qualified Cardano.Ledger.Shelley.Rules.Ledger as Rules

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

encodeBlock
    :: Crypto crypto
    => SerializationMode
    -> ShelleyBlock (Praos crypto) (BabbageEra crypto)
    -> Json
encodeBlock mode (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) = encodeObject
    [ ( "body"
      , encodeFoldable (encodeTx mode) (Ledger.Alonzo.txSeqTxns txs)
      )
    , ( "header"
      , encodeHeader mode blkHeader
      )
    , ( "headerHash"
      , Shelley.encodeShelleyHash headerHash
      )
    ]

encodeHeader
    :: Crypto crypto
    => SerializationMode
    -> Praos.Header crypto
    -> Json
encodeHeader mode (Praos.Header hBody hSig) = encodeObjectWithMode mode
    [ ( "blockHeight"
      , encodeBlockNo (Praos.hbBlockNo hBody)
      )
    , ( "slot"
      , encodeSlotNo (Praos.hbSlotNo hBody)
      )
    , ( "prevHash"
      , Shelley.encodePrevHash (Praos.hbPrev hBody)
      )
    , ( "issuerVk"
      , Shelley.encodeVKey (Praos.hbVk hBody)
      )
    , ( "issuerVrf"
      , Shelley.encodeVerKeyVRF (Praos.hbVrfVk hBody)
      )
    , ( "blockSize"
      , encodeWord32 (Praos.hbBodySize hBody)
      )
    , ( "blockHash"
      , Shelley.encodeHash (Praos.hbBodyHash hBody)
      )
    ]
    [ ( "protocolVersion"
      , Shelley.encodeProtVer (Praos.hbProtVer hBody)
      )
    , ( "opCert"
      , Shelley.encodeOCert (Praos.hbOCert hBody)
      )
    , ( "signature"
      , Shelley.encodeSignedKES hSig
      )
    , ( "vrfInput"
      , Shelley.encodeCertifiedVRF (Praos.hbVrfRes hBody)
      )
    ]

encodeUtxoFailure
    :: Crypto crypto
    => Rules.PredicateFailure (Ledger.EraRule "UTXO" (BabbageEra crypto))
    -> Json
encodeUtxoFailure = \case
    Ledger.Babbage.FromAlonzoUtxoFail e ->
        Alonzo.encodeUtxoFailure encodeUtxo encodeTxOut (\(Ledger.Babbage.TxOut addr _ _ _) -> addr)  e
    Ledger.Babbage.IncorrectTotalCollateralField computed declared ->
        encodeObject
            [ ( "totalCollateralMismatch"
              , encodeObject
                [ ( "computedFromDelta", encodeCoin computed )
                , ( "declaredInField", encodeCoin declared )
                ]
              )
            ]
    Ledger.Babbage.BabbageOutputTooSmallUTxO outs ->
        encodeObject
            [ ( "outputTooSmall"
              , encodeFoldable
                    (\(out, minimumValue) -> encodeObject
                        [ ( "output", encodeTxOut out )
                        , ( "minimumRequiredValue", encodeCoin minimumValue )
                        ]
                    )
                    outs
              )
            ]

encodeUtxowFailure
    :: Crypto crypto
    => Rules.PredicateFailure (Ledger.EraRule "UTXOW" (BabbageEra crypto))
    -> Json
encodeUtxowFailure = \case
    Ledger.Babbage.MalformedReferenceScripts scripts ->
        encodeObject
            [ ( "malformedReferenceScripts"
              , encodeFoldable Shelley.encodeScriptHash scripts
              )
            ]
    Ledger.Babbage.MalformedScriptWitnesses scripts ->
        encodeObject
            [ ( "malformedScriptWitnesses"
              , encodeFoldable Shelley.encodeScriptHash scripts
              )
            ]
    Ledger.Babbage.FromAlonzoUtxowFail e ->
        Alonzo.encodeUtxowPredicateFail encodeUtxoFailure e
    Ledger.Babbage.UtxoFailure e ->
        encodeUtxoFailure e

encodeLedgerFailure
    :: Crypto crypto
    => Rules.PredicateFailure (Ledger.EraRule "LEDGER" (BabbageEra crypto))
    -> Json
encodeLedgerFailure = \case
    Rules.UtxowFailure e ->
        encodeUtxowFailure e
    Rules.DelegsFailure e ->
        Shelley.encodeDelegsFailure e

encodeProposedPPUpdates
    :: Crypto crypto
    => Ledger.Shelley.ProposedPPUpdates (BabbageEra crypto)
    -> Json
encodeProposedPPUpdates (Ledger.Shelley.ProposedPPUpdates m) =
    encodeMap Shelley.stringifyKeyHash (encodePParams' encodeStrictMaybe) m

encodePParams'
    :: (forall a. (a -> Json) -> Ledger.Shelley.HKD f a -> Json)
    -> Ledger.Babbage.PParams' f era
    -> Json
encodePParams' encodeF x = encodeObject
    [ ( "minFeeCoefficient"
      , encodeF encodeNatural (Ledger.Babbage._minfeeA x)
      )
    , ( "minFeeConstant"
      , encodeF encodeNatural (Ledger.Babbage._minfeeB x)
      )
    , ( "maxBlockBodySize"
      , encodeF encodeNatural (Ledger.Babbage._maxBBSize x)
      )
    , ( "maxBlockHeaderSize"
      , encodeF encodeNatural (Ledger.Babbage._maxBHSize x)
      )
    , ( "maxTxSize"
      , encodeF encodeNatural (Ledger.Babbage._maxTxSize x)
      )
    , ( "stakeKeyDeposit"
      , encodeF encodeCoin (Ledger.Babbage._keyDeposit x)
      )
    , ( "poolDeposit"
      , encodeF encodeCoin (Ledger.Babbage._poolDeposit x)
      )
    , ( "poolRetirementEpochBound"
      , encodeF encodeEpochNo (Ledger.Babbage._eMax x)
      )
    , ( "desiredNumberOfPools"
      , encodeF encodeNatural (Ledger.Babbage._nOpt x)
      )
    , ( "poolInfluence"
      , encodeF encodeNonNegativeInterval (Ledger.Babbage._a0 x)
      )
    , ( "monetaryExpansion"
      , encodeF encodeUnitInterval (Ledger.Babbage._rho x)
      )
    , ( "treasuryExpansion"
      , encodeF encodeUnitInterval (Ledger.Babbage._tau x)
      )
    , ( "protocolVersion"
      , encodeF Shelley.encodeProtVer (Ledger.Babbage._protocolVersion x)
      )
    , ( "minPoolCost"
      , encodeF encodeCoin (Ledger.Babbage._minPoolCost x)
      )
    , ( "coinsPerUtxoByte"
      , encodeF encodeCoin (Ledger.Babbage._coinsPerUTxOByte x)
      )
    , ( "costModels"
      , encodeF Alonzo.encodeCostModels (Ledger.Babbage._costmdls x)
      )
    , ( "prices"
      , encodeF Alonzo.encodePrices (Ledger.Babbage._prices x)
      )
    , ( "maxExecutionUnitsPerTransaction"
      , encodeF Alonzo.encodeExUnits (Ledger.Babbage._maxTxExUnits x)
      )
    , ( "maxExecutionUnitsPerBlock"
      , encodeF Alonzo.encodeExUnits (Ledger.Babbage._maxBlockExUnits x)
      )
    , ( "maxValueSize"
      , encodeF encodeNatural (Ledger.Babbage._maxValSize x)
      )
    , ( "collateralPercentage"
      , encodeF encodeNatural (Ledger.Babbage._collateralPercentage x)
      )
    , ( "maxCollateralInputs"
      , encodeF encodeNatural (Ledger.Babbage._maxCollateralInputs x)
      )
    ]

encodeTx
    :: forall crypto. Crypto crypto
    => SerializationMode
    -> Ledger.Babbage.ValidatedTx (BabbageEra crypto)
    -> Json
encodeTx mode x = encodeObjectWithMode mode
   [ ( "id"
     , Shelley.encodeTxId (Ledger.txid @(BabbageEra crypto) (Ledger.Babbage.body x))
     )
   , ( "body"
     , encodeTxBody (Ledger.Babbage.body x)
     )
   , ( "metadata"
     , (,) <$> fmap (("hash",) . Shelley.encodeAuxiliaryDataHash) (adHash (Ledger.Babbage.body x))
           <*> fmap (("body",) . Alonzo.encodeAuxiliaryData) (Ledger.Babbage.auxiliaryData x)
       & encodeStrictMaybe (\(a, b) -> encodeObject [a,b])
     )
   , ( "inputSource"
     , Alonzo.encodeIsValid (Ledger.Babbage.isValid x)
     )
   ]
   [ ( "witness"
     , Alonzo.encodeWitnessSet (Ledger.Babbage.wits x)
     )
   , ( "raw"
     , encodeByteStringBase64 (serialize' x)
     )
   ]
 where
   adHash :: Ledger.Babbage.TxBody era -> StrictMaybe (Ledger.AuxiliaryDataHash (Ledger.Crypto era))
   adHash = getField @"adHash"

encodeTxBody
    :: Crypto crypto
    => Ledger.Babbage.TxBody (BabbageEra crypto)
    -> Json
encodeTxBody x = encodeObject
    [ ( "inputs"
      , encodeFoldable Shelley.encodeTxIn (Ledger.Babbage.inputs x)
      )
    , ( "collaterals"
      , encodeFoldable Shelley.encodeTxIn (Ledger.Babbage.collateral x)
      )
    , ( "references"
      , encodeFoldable Shelley.encodeTxIn (Ledger.Babbage.referenceInputs x)
      )
    , ( "collateralReturn"
      , encodeStrictMaybe encodeTxOut (Ledger.Babbage.collateralReturn' x)
      )
    , ( "totalCollateral"
      , encodeStrictMaybe encodeCoin (Ledger.Babbage.totalCollateral x)
      )
    , ( "outputs"
      , encodeFoldable encodeTxOut (Ledger.Babbage.outputs' x)
      )
    , ( "certificates"
      , encodeFoldable Shelley.encodeDCert (Ledger.Babbage.txcerts x)
      )
    , ( "withdrawals"
      , Shelley.encodeWdrl (Ledger.Babbage.txwdrls x)
      )
    , ( "fee"
      , encodeCoin (Ledger.Babbage.txfee x)
      )
    , ( "validityInterval"
      , Allegra.encodeValidityInterval (Ledger.Babbage.txvldt x)
      )
    , ( "update"
      , encodeStrictMaybe encodeUpdate (Ledger.Babbage.txUpdates x)
      )
    , ( "mint"
      , Mary.encodeValue (Ledger.Babbage.mint x)
      )
    , ( "network"
      , encodeStrictMaybe Shelley.encodeNetwork (Ledger.Babbage.txnetworkid x)
      )
    , ( "scriptIntegrityHash"
      , encodeStrictMaybe Alonzo.encodeScriptIntegrityHash (Ledger.Babbage.scriptIntegrityHash x)
      )
    , ( "requiredExtraSignatures"
      , encodeFoldable Shelley.encodeKeyHash (Ledger.Babbage.reqSignerHashes x)
      )
    ]

encodeTxOut
    :: Crypto crypto
    => Ledger.Babbage.TxOut (BabbageEra crypto)
    -> Json
encodeTxOut (Ledger.Babbage.TxOut addr value datum script) = encodeObject
    [ ( "address"
      , Shelley.encodeAddress addr
      )
    , ( "value"
      , Mary.encodeValue value
      )
    , ( "datumHash"
      , case datum of
            Ledger.Alonzo.NoDatum -> encodeNull
            Ledger.Alonzo.DatumHash h -> Alonzo.encodeDataHash h
            Ledger.Alonzo.Datum{} -> encodeNull
      )
    , ( "datum"
      , case datum of
            Ledger.Alonzo.NoDatum -> encodeNull
            Ledger.Alonzo.DatumHash{} -> encodeNull
            Ledger.Alonzo.Datum bin -> Alonzo.encodeBinaryData bin
      )
    , ( "script"
      , encodeStrictMaybe Alonzo.encodeScript script
      )
    ]

encodeUpdate
    :: Crypto crypto
    => Ledger.Shelley.Update (BabbageEra crypto)
    -> Json
encodeUpdate (Ledger.Shelley.Update update epoch) = encodeObject
    [ ( "proposal"
      , encodeProposedPPUpdates update
      )
    , ( "epoch"
      , encodeEpochNo epoch
      )
    ]

encodeUtxo
    :: Crypto crypto
    => Ledger.Shelley.UTxO (BabbageEra crypto)
    -> Json
encodeUtxo =
    encodeList id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Ledger.Shelley.unUTxO
  where
    encodeIO = curry (encode2Tuple Shelley.encodeTxIn encodeTxOut)

encodeUtxoWithMode
    :: Crypto crypto
    => SerializationMode
    -> Ledger.Shelley.UTxO (BabbageEra crypto)
    -> Json
encodeUtxoWithMode mode =
    encodeListWithMode mode id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Ledger.Shelley.unUTxO
  where
    encodeIO = curry (encode2Tuple Shelley.encodeTxIn encodeTxOut)
