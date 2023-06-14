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
import Cardano.Ledger.Val
    ( isZero
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
    => ShelleyBlock (Praos crypto) (BabbageEra crypto)
    -> Json
encodeBlock (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    "body" .=
        encodeFoldable encodeTx (Ledger.Alonzo.txSeqTxns txs) <>
    "header" .=
        encodeHeader blkHeader <>
    "headerHash" .=
        Shelley.encodeShelleyHash headerHash
    & encodeObject

encodeHeader
    :: Crypto crypto
    => Praos.Header crypto
    -> Json
encodeHeader (Praos.Header hBody hSig) =
    "blockHeight" .=
        encodeBlockNo (Praos.hbBlockNo hBody) <>
    "slot" .=
        encodeSlotNo (Praos.hbSlotNo hBody) <>
    "prevHash" .=
        Shelley.encodePrevHash (Praos.hbPrev hBody) <>
    "issuerVk" .=
        Shelley.encodeVKey (Praos.hbVk hBody) <>
    "issuerVrf" .=
        Shelley.encodeVerKeyVRF (Praos.hbVrfVk hBody) <>
    "blockSize" .=
        encodeWord32 (Praos.hbBodySize hBody) <>
    "blockHash" .=
        Shelley.encodeHash (Praos.hbBodyHash hBody) <>
    "protocolVersion" .=
        Shelley.encodeProtVer (Praos.hbProtVer hBody) <>
    "opCert" .=
        Shelley.encodeOCert (Praos.hbOCert hBody) <>
    "signature" .=
        Shelley.encodeSignedKES hSig <>
    "vrfInput" .=
        Shelley.encodeCertifiedVRF (Praos.hbVrfRes hBody)
    & encodeObject

encodeUtxoFailure
    :: Crypto crypto
    => Rules.PredicateFailure (Ledger.EraRule "UTXO" (BabbageEra crypto))
    -> Json
encodeUtxoFailure = \case
    Ledger.Babbage.FromAlonzoUtxoFail e ->
        Alonzo.encodeUtxoFailure
            encodeUtxo
            encodeTxOut
            (\(Ledger.Babbage.TxOut addr _ _ _) -> addr)
            e
    Ledger.Babbage.IncorrectTotalCollateralField computed declared ->
        "totalCollateralMismatch" .= encodeObject
            ( "computedFromDelta" .= encodeCoin computed <>
              "declaredInField" .= encodeCoin declared
            )
        & encodeObject
    Ledger.Babbage.BabbageOutputTooSmallUTxO outs ->
        "outputTooSmall" .= encodeFoldable
            (\(out, minimumValue) -> encodeObject
                ( "output" .= encodeTxOut out <>
                  "minimumRequiredValue" .= encodeCoin minimumValue
                )
            )
            outs
        & encodeObject

encodeUtxowFailure
    :: Crypto crypto
    => Rules.PredicateFailure (Ledger.EraRule "UTXOW" (BabbageEra crypto))
    -> Json
encodeUtxowFailure = \case
    Ledger.Babbage.MalformedReferenceScripts scripts ->
        "malformedReferenceScripts" .=
            encodeFoldable Shelley.encodeScriptHash scripts
        & encodeObject
    Ledger.Babbage.MalformedScriptWitnesses scripts ->
        "malformedScriptWitnesses" .=
            encodeFoldable Shelley.encodeScriptHash scripts
        & encodeObject
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
    encodeMap
        Shelley.stringifyKeyHash
        (encodePParams' (\k encode v -> k .=? OmitWhenNothing encode v))
        m

encodePParams'
    :: (forall a. Text -> (a -> Json) -> Ledger.Shelley.HKD f a -> Series)
    -> Ledger.Babbage.PParams' f era
    -> Json
encodePParams' encode x =
    encode "minFeeCoefficient"
        encodeNatural (Ledger.Babbage._minfeeA x) <>
    encode "minFeeConstant"
        encodeNatural (Ledger.Babbage._minfeeB x) <>
    encode "maxBlockBodySize"
        encodeNatural (Ledger.Babbage._maxBBSize x) <>
    encode "maxBlockHeaderSize"
        encodeNatural (Ledger.Babbage._maxBHSize x) <>
    encode "maxTxSize"
        encodeNatural (Ledger.Babbage._maxTxSize x) <>
    encode "stakeKeyDeposit"
        encodeCoin (Ledger.Babbage._keyDeposit x) <>
    encode "poolDeposit"
        encodeCoin (Ledger.Babbage._poolDeposit x) <>
    encode "poolRetirementEpochBound"
        encodeEpochNo (Ledger.Babbage._eMax x) <>
    encode "desiredNumberOfPools"
        encodeNatural (Ledger.Babbage._nOpt x) <>
    encode "poolInfluence"
        encodeNonNegativeInterval (Ledger.Babbage._a0 x) <>
    encode "monetaryExpansion"
        encodeUnitInterval (Ledger.Babbage._rho x) <>
    encode "treasuryExpansion"
        encodeUnitInterval (Ledger.Babbage._tau x) <>
    encode "protocolVersion"
        Shelley.encodeProtVer (Ledger.Babbage._protocolVersion x) <>
    encode "minPoolCost"
        encodeCoin (Ledger.Babbage._minPoolCost x) <>
    encode "coinsPerUtxoByte"
        encodeCoin (Ledger.Babbage._coinsPerUTxOByte x) <>
    encode "costModels"
        Alonzo.encodeCostModels (Ledger.Babbage._costmdls x) <>
    encode "prices"
        Alonzo.encodePrices (Ledger.Babbage._prices x) <>
    encode "maxExecutionUnitsPerTransaction"
        Alonzo.encodeExUnits (Ledger.Babbage._maxTxExUnits x) <>
    encode "maxExecutionUnitsPerBlock"
        Alonzo.encodeExUnits (Ledger.Babbage._maxBlockExUnits x) <>
    encode "maxValueSize"
        encodeNatural (Ledger.Babbage._maxValSize x) <>
    encode "collateralPercentage"
        encodeNatural (Ledger.Babbage._collateralPercentage x) <>
    encode "maxCollateralInputs"
        encodeNatural (Ledger.Babbage._maxCollateralInputs x)
    & encodeObject

encodeTx
    :: forall crypto. Crypto crypto
    => Ledger.Babbage.ValidatedTx (BabbageEra crypto)
    -> Json
encodeTx x =
    "id" .= Shelley.encodeTxId (Ledger.txid @(BabbageEra crypto) (Ledger.Babbage.body x))
        <>
    "inputSource" .= Alonzo.encodeIsValid (Ledger.Babbage.isValid x)
        <>
    encodeTxBody (Ledger.Babbage.body x)
        <>
    "metadata" .=? OmitWhenNothing fst auxiliary
        <>
    Alonzo.encodeWitnessSet (snd <$> auxiliary) (Ledger.Babbage.wits x)
        <>
    "cbor" .= encodeByteStringBase16 (serialize' x)
        & encodeObject
  where
    adHash :: Ledger.Babbage.TxBody era -> StrictMaybe (Ledger.AuxiliaryDataHash (Ledger.Crypto era))
    adHash = getField @"adHash"

    auxiliary = do
        hash <- Shelley.encodeAuxiliaryDataHash <$> adHash (Ledger.Babbage.body x)
        (labels, scripts) <- Alonzo.encodeAuxiliaryData <$> Ledger.Babbage.auxiliaryData x
        pure
            ( encodeObject ("hash" .= hash <> "labels" .= labels)
            , scripts
            )


encodeTxBody
    :: Crypto crypto
    => Ledger.Babbage.TxBody (BabbageEra crypto)
    -> Series
encodeTxBody x =
    "inputs" .=
        encodeFoldable Shelley.encodeTxIn (Ledger.Babbage.inputs x) <>
    "references" .=? OmitWhen null
        (encodeFoldable Shelley.encodeTxIn) (Ledger.Babbage.referenceInputs x) <>
    "outputs" .=
        encodeFoldable encodeTxOut (Ledger.Babbage.outputs' x) <>
    "collaterals" .=? OmitWhen null
        (encodeFoldable Shelley.encodeTxIn) (Ledger.Babbage.collateral x) <>
    "collateralReturn" .=? OmitWhenNothing
        encodeTxOut (Ledger.Babbage.collateralReturn' x) <>
    "collateral" .=? OmitWhenNothing
        encodeCoin (Ledger.Babbage.totalCollateral x) <>
    "certificates" .=? OmitWhen null
        (encodeFoldable Shelley.encodeDCert) (Ledger.Babbage.txcerts x) <>
    "withdrawals" .=? OmitWhen (null . Ledger.Shelley.unWdrl)
        Shelley.encodeWdrl (Ledger.Babbage.txwdrls x) <>
    "mint" .=? OmitWhen isZero
        Mary.encodeValue (Ledger.Babbage.mint x) <>
    "requiredExtraSignatories" .=? OmitWhen null
        (encodeFoldable Shelley.encodeKeyHash) (Ledger.Babbage.reqSignerHashes x) <>
    "network" .=? OmitWhenNothing
        Shelley.encodeNetwork (Ledger.Babbage.txnetworkid x) <>
    "scriptIntegrityHash" .=? OmitWhenNothing
        Alonzo.encodeScriptIntegrityHash (Ledger.Babbage.scriptIntegrityHash x) <>
    "fee" .=
        encodeCoin (Ledger.Babbage.txfee x) <>
    "validityInterval" .=
        Allegra.encodeValidityInterval (Ledger.Babbage.txvldt x) <>
    "governanceActions" .=? OmitWhenNothing
        (encodeFoldable identity . pure @[] . encodeUpdate)
        (Ledger.Babbage.txUpdates x)

encodeTxOut
    :: Crypto crypto
    => Ledger.Babbage.TxOut (BabbageEra crypto)
    -> Json
encodeTxOut (Ledger.Babbage.TxOut addr value datum script) =
    "address" .=
        Shelley.encodeAddress addr <>
    "value" .=
        Mary.encodeValue value <>
    ( case datum of
        Ledger.Alonzo.NoDatum ->
            mempty
        Ledger.Alonzo.DatumHash h ->
            "datumHash" .= Alonzo.encodeDataHash h
        Ledger.Alonzo.Datum bin ->
            "datum" .= Alonzo.encodeBinaryData bin
    ) <>
    "script" .=? OmitWhenNothing
        Alonzo.encodeScript script
    & encodeObject

encodeUpdate
    :: Crypto crypto
    => Ledger.Shelley.Update (BabbageEra crypto)
    -> Json
encodeUpdate (Ledger.Shelley.Update update epoch) =
    "proposal" .=
        encodeProposedPPUpdates update <>
    "epoch" .=
        encodeEpochNo epoch
    & encodeObject

encodeUtxo
    :: Crypto crypto
    => Ledger.Shelley.UTxO (BabbageEra crypto)
    -> Json
encodeUtxo =
    encodeList id
    . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) []
    . Ledger.Shelley.unUTxO
  where
    encodeIO = curry (encode2Tuple Shelley.encodeTxIn encodeTxOut)
