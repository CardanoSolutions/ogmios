--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Alonzo where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( serialize'
    )
import Data.ByteString.Base16
    ( encodeBase16
    )
import Data.Maybe.Strict
    ( fromSMaybe
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()

import qualified Data.Map.Strict as Map

import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Protocol.TPraos.BHeader as TPraos

import qualified Cardano.Ledger.Api as Ledger.Api

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger hiding
    ( TranslationError
    )
import qualified Cardano.Ledger.SafeHash as Ledger

import qualified Cardano.Ledger.Shelley.API as Sh
import qualified Cardano.Ledger.Shelley.PParams as Sh

import qualified Cardano.Ledger.Alonzo.Core as Al hiding
    ( TranslationError
    )
import qualified Cardano.Ledger.Alonzo.Genesis as Al
import qualified Cardano.Ledger.Alonzo.Language as Al
import qualified Cardano.Ledger.Alonzo.PParams as Al
import qualified Cardano.Ledger.Alonzo.Scripts as Al
import qualified Cardano.Ledger.Alonzo.Scripts.Data as Al
import qualified Cardano.Ledger.Alonzo.Tx as Al
import qualified Cardano.Ledger.Alonzo.TxAuxData as Al
import qualified Cardano.Ledger.Alonzo.TxBody as Al
import qualified Cardano.Ledger.Alonzo.TxInfo as Al hiding
    ( txscripts
    )
import qualified Cardano.Ledger.Alonzo.TxSeq as Al
import qualified Cardano.Ledger.Alonzo.TxWits as Al

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley


type AuxiliaryScripts era =
    Map (Ledger.ScriptHash (Ledger.EraCrypto era)) (Ledger.Script era)

--
-- Encoders
--

encodeAuxiliaryData
    :: forall era.
        ( Ledger.Script era ~ Al.AlonzoScript era
        , Ledger.Api.EraScript era
        )
    => Al.AlonzoTxAuxData era
    -> (Json, AuxiliaryScripts era)
encodeAuxiliaryData (Al.AlonzoTxAuxData blob timelocks plutus) =
    ( Shelley.encodeMetadataBlob blob
    , foldr
        (\(Al.TimelockScript -> script) -> Map.insert (Ledger.hashScript @era script) script)
        (Map.foldrWithKey
            (\lang ->
                flip $ foldr (\(Al.BinaryPlutus bytes) ->
                    let script = Al.PlutusScript lang bytes
                     in Map.insert (Ledger.hashScript @era script) script
                )
            )
            mempty
            plutus
        )
        timelocks
    )

encodeBinaryData
    :: Al.BinaryData era
    -> Json
encodeBinaryData =
    encodeByteStringBase16 . Ledger.originalBytes

encodeBlock
    :: ( Crypto crypto
       )
    => ShelleyBlock (TPraos crypto) (AlonzoEra crypto)
    -> Json
encodeBlock (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "type" .= encodeText "praos"
        <>
          "era" .= encodeText "alonzo"
        <>
          "header" .= encodeObject
            ( "hash" .= Shelley.encodeShelleyHash headerHash
            )
        <>
        Shelley.encodeBHeader blkHeader
        <>
        "size" .= encodeNatural (TPraos.bsize hBody)
        <>
        "transactions" .= encodeFoldable encodeTx (Al.txSeqTxns txs)
        )
  where
    TPraos.BHeader hBody _ = blkHeader

encodeCostModel
    :: Al.CostModel
    -> Json
encodeCostModel =
    encodeList encodeInteger . Al.getCostModelParams

encodeCostModels
    :: Al.CostModels
    -> Json
encodeCostModels =
    encodeMap stringifyLanguage encodeCostModel . Al.costModelsValid

encodeData
    :: Ledger.Era era
    => Al.Data era
    -> Json
encodeData =
    encodeByteStringBase16 . serialize'

encodeDataHash
    :: Crypto crypto
    => Al.DataHash crypto
    -> Json
encodeDataHash =
    Shelley.encodeHash . Ledger.extractHash

encodeExUnits
    :: Al.ExUnits
    -> Json
encodeExUnits units =
    "memory" .=
        encodeNatural (Al.exUnitsMem units) <>
    "steps" .=
        encodeNatural (Al.exUnitsSteps units)
    & encodeObject

encodeGenesis
    :: Al.AlonzoGenesis
    -> Json
encodeGenesis x =
    "coinsPerUtxoWord" .=
        (encodeInteger . unCoin) (Al.unCoinPerWord (Al.agCoinsPerUTxOWord x)) <>
    "costModels" .=
        encodeCostModels (Al.agCostModels x) <>
    "prices" .=
        encodePrices (Al.agPrices x) <>
    "maxExecutionUnitsPerTransaction" .=
        encodeExUnits (Al.agMaxTxExUnits x) <>
    "maxExecutionUnitsPerBlock" .=
        encodeExUnits (Al.agMaxBlockExUnits x) <>
    "maxValueSize" .=
        encodeNatural (Al.agMaxValSize x) <>
    "collateralPercentage" .=
        encodeNatural (Al.agCollateralPercentage x) <>
    "maxCollateralInputs" .=
        encodeNatural (Al.agMaxCollateralInputs x)
    & encodeObject

encodeIsValid
    :: Al.IsValid
    -> Json
encodeIsValid = \case
    Al.IsValid True ->
        encodeText "inputs"
    Al.IsValid False ->
        encodeText "collaterals"

encodeLanguage
    :: Al.Language
    -> Json
encodeLanguage =
    encodeText . stringifyLanguage

encodeMissingRedeemer
    :: Crypto crypto
    => (Al.ScriptPurpose crypto, Sh.ScriptHash crypto)
    -> Json
encodeMissingRedeemer (purpose, hash) =
    Shelley.stringifyScriptHash hash .=
        encodeScriptPurpose purpose
    & encodeObject

encodePParams
    :: (Ledger.PParamsHKD Identity era ~ Al.AlonzoPParams Identity era)
    => Ledger.PParams era
    -> Json
encodePParams (Ledger.PParams x) =
    encodePParamsHKD (\k encode v -> k .= encode v) x

encodePParamsUpdate
    :: (Ledger.PParamsHKD StrictMaybe era ~ Al.AlonzoPParams StrictMaybe era)
    => Ledger.PParamsUpdate era
    -> Json
encodePParamsUpdate (Ledger.PParamsUpdate x) =
    encodePParamsHKD (\k encode v -> k .=? OmitWhenNothing encode v) x

encodePParamsHKD
    :: (forall a. Text -> (a -> Json) -> Sh.HKD f a -> Series)
    -> Al.AlonzoPParams f era
    -> Json
encodePParamsHKD encode x =
    encode "minFeeCoefficient"
        (encodeInteger . unCoin) (Al.appMinFeeA x) <>
    encode "minFeeConstant"
        encodeCoin (Al.appMinFeeB x) <>
    encode "maxBlockBodySize"
        encodeNatural (Al.appMaxBBSize x) <>
    encode "maxBlockHeaderSize"
        encodeNatural (Al.appMaxBHSize x) <>
    encode "maxTxSize"
        encodeNatural (Al.appMaxTxSize x) <>
    encode "stakeKeyDeposit"
        encodeCoin (Al.appKeyDeposit x) <>
    encode "poolDeposit"
        encodeCoin (Al.appPoolDeposit x) <>
    encode "poolRetirementEpochBound"
        encodeEpochNo (Al.appEMax x) <>
    encode "desiredNumberOfPools"
        encodeNatural (Al.appNOpt x) <>
    encode "poolInfluence"
        encodeNonNegativeInterval (Al.appA0 x) <>
    encode "monetaryExpansion"
        encodeUnitInterval (Al.appRho x) <>
    encode "treasuryExpansion"
        encodeUnitInterval (Al.appTau x) <>
    encode "decentralizationParameter"
        encodeUnitInterval (Al.appD x) <>
    encode "extraEntropy"
        Shelley.encodeNonce (Al.appExtraEntropy x) <>
    encode "protocolVersion"
        Shelley.encodeProtVer (Al.appProtocolVersion x) <>
    encode "minPoolCost"
        encodeCoin (Al.appMinPoolCost x) <>
    encode "coinsPerUtxoWord"
        (encodeInteger . unCoin . Al.unCoinPerWord) (Al.appCoinsPerUTxOWord x) <>
    encode "costModels"
        encodeCostModels (Al.appCostModels x) <>
    encode "prices"
        encodePrices (Al.appPrices x) <>
    encode "maxExecutionUnitsPerTransaction"
        (encodeExUnits . Al.unOrdExUnits) (Al.appMaxTxExUnits x) <>
    encode "maxExecutionUnitsPerBlock"
        (encodeExUnits . Al.unOrdExUnits) (Al.appMaxBlockExUnits x) <>
    encode "maxValueSize"
        encodeNatural (Al.appMaxValSize x) <>
    encode "collateralPercentage"
        encodeNatural (Al.appCollateralPercentage x) <>
    encode "maxCollateralInputs"
        encodeNatural (Al.appMaxCollateralInputs x)
    & encodeObject

encodePrices
    :: Al.Prices
    -> Json
encodePrices prices =
    "memory" .=
        encodeNonNegativeInterval (Al.prMem prices) <>
    "steps" .=
        encodeNonNegativeInterval (Al.prSteps prices)
    & encodeObject

encodeProposedPPUpdates
    :: Crypto crypto
    => Sh.ProposedPPUpdates (AlonzoEra crypto)
    -> Json
encodeProposedPPUpdates (Sh.ProposedPPUpdates m) =
    encodeMap Shelley.stringifyKeyHash encodePParamsUpdate m

encodeRdmrPtr
    :: Al.RdmrPtr
    -> Json
encodeRdmrPtr =
    encodeText . stringifyRdmrPtr

encodeRedeemers
    :: forall era. (Ledger.Era era)
    => Al.Redeemers era
    -> Json
encodeRedeemers (Al.Redeemers redeemers) =
    encodeMap stringifyRdmrPtr encodeDataAndUnits redeemers
  where
    encodeDataAndUnits
        :: (Al.Data era, Al.ExUnits)
        -> Json
    encodeDataAndUnits (redeemer, units) =
        "redeemer" .=
            encodeData redeemer <>
        "executionUnits" .=
            encodeExUnits units
        & encodeObject

encodeScript
    :: ( Era era
       , Ledger.Api.Script era ~ Al.AlonzoScript era
       )
    => Al.Script era
    -> Json
encodeScript = \case
    Al.TimelockScript nativeScript ->
        "native" .=
            Allegra.encodeTimelock nativeScript
        & encodeObject
    Al.PlutusScript lang serializedScript ->
        stringifyLanguage lang .=
            encodeShortByteString encodeByteStringBase16 serializedScript
        & encodeObject

encodeScriptPurpose
    :: Crypto crypto
    => Al.ScriptPurpose crypto
    -> Json
encodeScriptPurpose = encodeObject . \case
    Al.Spending txIn ->
        "spend" .=
            Shelley.encodeTxIn txIn
    Al.Minting policyId ->
        "mint" .=
            Mary.encodePolicyId policyId
    Al.Certifying cert ->
        "certificate" .=
            Shelley.encodeDCert cert
    Al.Rewarding acct ->
        "withdrawal" .=
            Shelley.encodeRewardAcnt acct

encodeTx
    :: forall crypto.
        ( Crypto crypto
        )
    => Al.AlonzoTx (AlonzoEra crypto)
    -> Json
encodeTx x =
    "id" .= Shelley.encodeTxId (Ledger.txid @(AlonzoEra crypto) (Al.body x))
        <>
    "inputSource" .= encodeIsValid (Al.isValid x)
        <>
    encodeTxBody (Al.body x)
        <>
    "metadata" .=? OmitWhenNothing fst auxiliary
        <>
    encodeWitnessSet (snd <$> auxiliary) (Al.wits x)
        <>
    "cbor" .= encodeByteStringBase16 (serialize' x)
        & encodeObject
  where
    auxiliary = do
        hash <- Shelley.encodeAuxiliaryDataHash <$> Al.atbAuxDataHash (Al.body x)
        (labels, scripts) <- encodeAuxiliaryData <$> Al.auxiliaryData x
        pure
            ( encodeObject ("hash" .= hash <> "labels" .= labels)
            , scripts
            )

encodeTxBody
    :: ( Crypto crypto
       )
    => Al.AlonzoTxBody (AlonzoEra crypto)
    -> Series
encodeTxBody x =
    "inputs" .=
        encodeFoldable Shelley.encodeTxIn (Al.atbInputs x) <>
    "outputs" .=
        encodeFoldable encodeTxOut (Al.atbOutputs x) <>
    "collaterals" .=? OmitWhen null
        (encodeFoldable Shelley.encodeTxIn) (Al.atbCollateral x) <>
    "certificates" .=? OmitWhen null
        (encodeFoldable Shelley.encodeDCert) (Al.atbCerts x) <>
    "withdrawals" .=? OmitWhen (null . Ledger.unWithdrawals)
        Shelley.encodeWdrl (Al.atbWithdrawals x) <>
            "mint" .=? OmitWhen (== mempty)
        (encodeObject . Mary.encodeMultiAsset) (Al.atbMint x) <>
    "requiredExtraSignatories".=? OmitWhen null
        (encodeFoldable Shelley.encodeKeyHash) (Al.atbReqSignerHashes x) <>
    "network" .=? OmitWhenNothing
        Shelley.encodeNetwork (Al.atbTxNetworkId x) <>
    "scriptIntegrityHash" .=? OmitWhenNothing
        encodeScriptIntegrityHash (Al.atbScriptIntegrityHash x) <>
    "fee" .=
        encodeCoin (Al.atbTxFee x) <>
    "validityInterval" .=
        Allegra.encodeValidityInterval (Al.atbValidityInterval x) <>
    "governanceActions" .=? OmitWhenNothing
        (encodeFoldable identity . pure @[] . encodeUpdate)
        (Al.atbUpdate x)

encodeTxOut
    :: Crypto crypto
    => Al.AlonzoTxOut (AlonzoEra crypto)
    -> Json
encodeTxOut (Al.AlonzoTxOut addr value datum) =
    "address" .=
        Shelley.encodeAddress addr <>
    "value" .=
        Mary.encodeValue value <>
    "datumHash" .=? OmitWhenNothing
        encodeDataHash datum <>
    -- NOTE: backward-compatibility, since v5.5.0
    "datum" .=? OmitWhenNothing
        encodeDataHash datum
    & encodeObject

encodeUpdate
    :: Crypto crypto
    => Sh.Update (AlonzoEra crypto)
    -> Json
encodeUpdate (Sh.Update update epoch) =
    "proposal" .=
        encodeProposedPPUpdates update <>
    "epoch" .=
        encodeEpochNo epoch
    & encodeObject

encodeUtxo
    :: Crypto crypto
    => Sh.UTxO (AlonzoEra crypto)
    -> Json
encodeUtxo =
    encodeList id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO = curry (encode2Tuple Shelley.encodeTxIn encodeTxOut)

encodeScriptIntegrityHash
    :: Crypto crypto
    => Al.ScriptIntegrityHash crypto
    -> Json
encodeScriptIntegrityHash =
    Shelley.encodeHash . Ledger.extractHash

encodeTranslationError
    :: Crypto crypto
    => Al.TranslationError crypto
    -> Json
encodeTranslationError err = encodeText $ case err of
    Al.ByronTxOutInContext Al.TxOutFromInput{} ->
        "Found inputs locked by a (legacy) Byron/Bootstrap address. Don't use those."
    Al.ByronTxOutInContext Al.TxOutFromOutput{} ->
        "Found outputs to a (legacy) Byron/Bootstrap address. Don't use those."
    Al.TranslationLogicMissingInput i ->
        "Unknown transaction input (missing from UTxO set): " <> Shelley.stringifyTxIn i
    Al.LanguageNotSupported Al.PlutusV1 ->
       "Unsupported language in era."
    Al.LanguageNotSupported Al.PlutusV2 ->
       "Unsupported language in era. Did you try to use PlutusV2 before Babbage is enabled?"
    Al.LanguageNotSupported Al.PlutusV3 ->
       "Unsupported language in era. Did you try to use PlutusV3 before Conway is enabled?"
    Al.InlineDatumsNotSupported{} ->
       "Inline datums not supported in PlutusV1. Use PlutusV2."
    Al.ReferenceScriptsNotSupported{} ->
       "Reference scripts not supported in PlutusV1. Use PlutusV2."
    Al.ReferenceInputsNotSupported{} ->
       "Reference inputs not supported in PlutusV1. Use PlutusV2."
    Al.RdmrPtrPointsToNothing ptr ->
       "Couldn't resolve redeemer pointer (" <> stringifyRdmrPtr ptr <> "). Verify your transaction's construction."
    Al.TimeTranslationPastHorizon e ->
        "Uncomputable slot arithmetic; transaction's validity bounds go beyond the foreseeable end of the current era: " <> e

encodeWitnessSet
    :: (Ledger.Era era, Ledger.Script era ~ Al.AlonzoScript era)
    => StrictMaybe (AuxiliaryScripts era)
    -> Al.AlonzoTxWits era
    -> Series
encodeWitnessSet (fromSMaybe mempty -> auxScripts) x =
    "signatories" .=
        encodeFoldable2
            Shelley.encodeBootstrapWitness
            Shelley.encodeWitVKey
            (Al.txwitsBoot x)
            (Al.txwitsVKey x) <>
    "scripts" .=? OmitWhen null
        (encodeMap Shelley.stringifyScriptHash encodeScript)
        (Al.txscripts x <> auxScripts) <>
    "datums" .=? OmitWhen null
        (encodeMap stringifyDataHash encodeData)
        (Al.unTxDats $ Al.txdats x) <>
    "redeemers" .=? OmitWhen (\(Al.Redeemers redeemers) -> null redeemers)
        encodeRedeemers
        (Al.txrdmrs x)

--
-- Conversion To Text
--

stringifyDataHash
    :: Crypto crypto
    => Al.DataHash crypto
    -> Text
stringifyDataHash (Ledger.extractHash -> (CC.UnsafeHash h)) =
    encodeBase16 (fromShort h)

stringifyLanguage
    :: Al.Language
    -> Text
stringifyLanguage = \case
    Al.PlutusV1 -> "plutus:v1"
    Al.PlutusV2 -> "plutus:v2"
    Al.PlutusV3 -> "plutus:v3"

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
        Al.Spend -> "spend"
        Al.Mint -> "mint"
        Al.Cert -> "certificate"
        Al.Rewrd -> "withdrawal"
