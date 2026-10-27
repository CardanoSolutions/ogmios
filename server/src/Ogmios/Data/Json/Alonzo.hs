--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-- TODO(dijkstra): warnings disabled while accessor and constructor renames are stubbed.
{-# OPTIONS_GHC -Wno-unused-imports -Wno-incomplete-patterns -Wno-unused-matches -Wno-unused-top-binds -Wno-deprecations -Wno-redundant-constraints #-}

module Ogmios.Data.Json.Alonzo where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Allegra.Scripts (
    Timelock,
 )
import Cardano.Ledger.Api (
    AsIx,
    PlutusPurpose,
 )
import Data.SatInt (
    fromSatInt,
 )
import Ouroboros.Consensus.Shelley.Ledger.Block (
    ShelleyBlock (..),
 )
import Ouroboros.Consensus.Shelley.Protocol.TPraos (

 )

import qualified Data.Map.Strict as Map

import Cardano.Ledger.Compactible (
    fromCompact,
 )

import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Protocol.TPraos.BlockHeader as TPraos

import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Plutus.Language as Ledger

import qualified Cardano.Ledger.Shelley.API as Sh
import qualified Cardano.Ledger.Shelley.PParams as Sh
import qualified Cardano.Ledger.Shelley.TxCert as Sh

import qualified Cardano.Ledger.Alonzo.BlockBody as Al
import qualified Cardano.Ledger.Alonzo.Core as Al hiding (
    TranslationError,
 )
import qualified Cardano.Ledger.Alonzo.Genesis as Al
import qualified Cardano.Ledger.Alonzo.PParams as Al
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Al
import qualified Cardano.Ledger.Alonzo.Scripts as Al
import qualified Cardano.Ledger.Alonzo.Tx as Al
import qualified Cardano.Ledger.Alonzo.TxAuxData as Al
import qualified Cardano.Ledger.Alonzo.TxBody as Al
import qualified Cardano.Ledger.Alonzo.TxWits as Al

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley
import qualified PlutusLedgerApi.Common as Plutus

type AuxiliaryScripts era =
    Map Ledger.ScriptHash (Ledger.Script era)

--
-- Encoders
--

encodeAuxiliaryData ::
    forall era.
    ( Ledger.Script era ~ Al.AlonzoScript era
    , Al.AlonzoEraScript era
    , HasCallStack
    ) =>
    (MetadataFormat, IncludeCbor) ->
    Al.AlonzoTxAuxData era ->
    (Json, AuxiliaryScripts era)
encodeAuxiliaryData opts (Al.AlonzoTxAuxData blob timelocks plutus) =
    ( Shelley.encodeMetadataBlob @era opts blob
    , foldr
        (\(Al.NativeScript -> script) -> Map.insert (Ledger.hashScript @era script) script)
        ( Map.foldrWithKey
            ( \lang ->
                flip
                    $ foldr
                        ( \bytes ->
                            let script =
                                    maybe
                                        (error ("mkBinaryPlutusScript: incompatible language and script: " <> show lang <> " for " <> show bytes))
                                        Al.PlutusScript
                                        (Al.mkBinaryPlutusScript @Maybe @era lang bytes)
                             in Map.insert (Ledger.hashScript @era script) script
                        )
            )
            mempty
            plutus
        )
        timelocks
    )

encodeBinaryData ::
    Ledger.BinaryData era ->
    Json
encodeBinaryData =
    encodeByteStringBase16 . Ledger.originalBytes

encodeBlock ::
    (MetadataFormat, IncludeCbor) ->
    ShelleyBlock (TPraos StandardCrypto) AlonzoEra ->
    Json
encodeBlock opts (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "type"
            .= encodeText "praos"
            <> "era"
            .= encodeText "alonzo"
            <> "id"
            .= Shelley.encodeShelleyHash headerHash
            <> Shelley.encodeBHeader blkHeader
            <> "size"
            .= encodeSingleton "bytes" (encodeWord32 (TPraos.bsize hBody))
            <> "transactions"
            .= encodeFoldable (encodeTx opts . Al.unAlonzoTx) (Al.alonzoBlockBodyTxs txs)
        )
  where
    TPraos.BHeader hBody _ = blkHeader

encodeContextError ::
    Al.AlonzoContextError era ->
    Json
encodeContextError err = encodeText $ case err of
    Al.TranslationLogicMissingInput i ->
        "Unknown transaction input (missing from UTxO set): " <> Shelley.stringifyTxIn i
    Al.TimeTranslationPastHorizon e ->
        "Uncomputable slot arithmetic; transaction's validity bounds go beyond the foreseeable end of the current era: " <> e
    Al.CertificateNotSupported{} ->
        "A certificate in the transaction isn't supported in neither plutus:v1 nor plutus:v2. Use plutus:v3 or higher."
    Al.PlutusPurposeNotSupported{} ->
        "A script purpose in the transaction isn't supported in neither plutus:v1 nor plutus:v2. Use plutus:v3 or higher."
    Al.ImpossibleContextError e ->
        "Unexpected failure while building the script context: " <> e

encodeCostModel ::
    Al.CostModel ->
    Json
encodeCostModel =
    encodeList (encodeInteger . toInteger) . Al.getCostModelParams

encodeCostModels ::
    Al.CostModels ->
    Json
encodeCostModels =
    encodeMap stringifyLanguage encodeCostModel . Al.costModelsValid

encodeData ::
    forall era.
    (Ledger.Era era) =>
    Al.Data era ->
    Json
encodeData =
    encodeByteStringBase16 . encodeCbor @era

encodeDataHash ::
    Al.DataHash ->
    Json
encodeDataHash =
    Shelley.encodeHash . Ledger.extractHash

encodeExUnits ::
    Al.ExUnits ->
    Json
encodeExUnits units =
    "memory"
        .= encodeNatural (Al.exUnitsMem units)
        <> "cpu"
        .= encodeNatural (Al.exUnitsSteps units)
        & encodeObject

encodeExBudget ::
    Plutus.ExBudget ->
    Json
encodeExBudget budget =
    "memory"
        .= encodeInteger (fromSatInt mem)
        <> "cpu"
        .= encodeInteger (fromSatInt cpu)
        & encodeObject
  where
    Plutus.ExMemory mem = Plutus.exBudgetMemory budget
    Plutus.ExCPU cpu = Plutus.exBudgetCPU budget

encodeGenesis ::
    Al.AlonzoGenesis ->
    Json
encodeGenesis x =
    encodeObject
        ( "era"
            .= encodeText "alonzo"
            <> "updatableParameters"
            .= encodeObject
                ( "minUtxoDepositCoefficient"
                    .= (encodeInteger . (`div` 8) . unCoin . Al.unCoinPerWord) (Al.agCoinsPerUTxOWord x)
                    <> "plutusCostModels"
                    .= encodeCostModels
                        ( Al.mkCostModels
                            ( Map.singleton Ledger.PlutusV1 (Al.agPlutusV1CostModel x)
                                <> maybe mempty Al.costModelsValid (strictMaybe Nothing Al.aecCostModels (Al.agExtraConfig x))
                            )
                        )
                    <> "scriptExecutionPrices"
                    .= encodePrices (Al.agPrices x)
                    <> "maxExecutionUnitsPerTransaction"
                    .= encodeExUnits (Al.agMaxTxExUnits x)
                    <> "maxExecutionUnitsPerBlock"
                    .= encodeExUnits (Al.agMaxBlockExUnits x)
                    <> "maxValueSize"
                    .= (encodeSingleton "bytes" . encodeWord32) (Al.agMaxValSize x)
                    <> "collateralPercentage"
                    .= encodeWord16 (Al.agCollateralPercentage x)
                    <> "maxCollateralInputs"
                    .= encodeWord16 (Al.agMaxCollateralInputs x)
                )
        )

encodeIsValid ::
    Al.IsPhase2Valid ->
    Json
encodeIsValid = \case
    Al.Phase2Valid ->
        encodeText "inputs"
    Al.Phase2Invalid ->
        encodeText "collaterals"

encodeLanguage ::
    Ledger.Language ->
    Json
encodeLanguage =
    encodeText . stringifyLanguage

encodePParams ::
    (Ledger.PParamsHKD Identity era ~ Al.AlonzoPParams Identity era) =>
    Ledger.PParams era ->
    Json
encodePParams (Ledger.PParams x) =
    encodePParamsHKD (\k encode v -> k .= encode v) identity x

encodePParamsUpdate ::
    forall era.
    (Ledger.PParamsHKD StrictMaybe era ~ Al.AlonzoPParams StrictMaybe era) =>
    Ledger.PParamsUpdate era ->
    [Json]
encodePParamsUpdate (Ledger.PParamsUpdate x) =
    case (Al.appProtocolVersion x, x' == Al.emptyAlonzoPParamsUpdate) of
        (SJust version, True) ->
            [ encodeObject
                ( "type"
                    .= encodeText "hardForkInitiation"
                    <> "version"
                    .= Shelley.encodeProtVer version
                )
            ]
        (SJust version, False) ->
            [ encodeObject
                ( "type"
                    .= encodeText "hardForkInitiation"
                    <> "version"
                    .= Shelley.encodeProtVer version
                )
            , encodeObject
                ( "type"
                    .= encodeText "protocolParametersUpdate"
                    <> "parameters"
                    .= encodePParamsHKD
                        (\k encode v -> k .=? OmitWhenNothing encode v)
                        (const SNothing)
                        x'
                )
            ]
        (SNothing, _) ->
            [ encodeObject
                ( "type"
                    .= encodeText "protocolParametersUpdate"
                    <> "parameters"
                    .= encodePParamsHKD
                        (\k encode v -> k .=? OmitWhenNothing encode v)
                        (const SNothing)
                        x'
                )
            ]
  where
    x' :: Ledger.PParamsHKD StrictMaybe era
    x' = x{Al.appProtocolVersion = SNothing}

encodeProposedPPUpdates ::
    forall era.
    (Ledger.PParamsHKD StrictMaybe era ~ Al.AlonzoPParams StrictMaybe era) =>
    Sh.ProposedPPUpdates era ->
    Json
encodeProposedPPUpdates (Sh.ProposedPPUpdates m) =
    encodeFoldable
        ( \(Ledger.PParamsUpdate x) ->
            encodePParamsHKD
                (\k encode v -> k .=? OmitWhenNothing encode v)
                (const SNothing)
                x
        )
        m

encodePParamsHKD ::
    (forall a. Text -> (a -> Json) -> Sh.HKD f a -> Series) ->
    (Integer -> Sh.HKD f Integer) ->
    Al.AlonzoPParams f era ->
    Json
encodePParamsHKD encode pure_ x =
    encode
        "minFeeCoefficient"
        (encodeInteger . unCoin . fromCompact . Al.unCoinPerByte)
        (Al.appTxFeePerByte x)
        <> encode
            "minFeeConstant"
            (encodeCoin . fromCompact)
            (Al.appTxFeeFixed x)
        <> encode
            "maxBlockBodySize"
            (encodeSingleton "bytes" . encodeWord32)
            (Al.appMaxBBSize x)
        <> encode
            "maxBlockHeaderSize"
            (encodeSingleton "bytes" . encodeWord16)
            (Al.appMaxBHSize x)
        <> encode
            "maxTransactionSize"
            (encodeSingleton "bytes" . encodeWord32)
            (Al.appMaxTxSize x)
        <> encode
            "stakeCredentialDeposit"
            (encodeCoin . fromCompact)
            (Al.appKeyDeposit x)
        <> encode
            "stakePoolDeposit"
            (encodeCoin . fromCompact)
            (Al.appPoolDeposit x)
        <> encode
            "stakePoolRetirementEpochBound"
            encodeEpochInterval
            (Al.appEMax x)
        <> encode
            "desiredNumberOfStakePools"
            encodeWord16
            (Al.appNOpt x)
        <> encode
            "stakePoolPledgeInfluence"
            encodeNonNegativeInterval
            (Al.appA0 x)
        <> encode
            "monetaryExpansion"
            encodeUnitInterval
            (Al.appRho x)
        <> encode
            "treasuryExpansion"
            encodeUnitInterval
            (Al.appTau x)
        <> encode
            "federatedBlockProductionRatio"
            encodeUnitInterval
            (Al.appD x)
        <> encode
            "extraEntropy"
            Shelley.encodeNonce
            (Al.appExtraEntropy x)
        <> encode
            "minStakePoolCost"
            (encodeCoin . fromCompact)
            (Al.appMinPoolCost x)
        <> encode
            "minUtxoDepositConstant"
            (encodeCoin . Coin)
            (pure_ 0)
        <> encode
            "minUtxoDepositCoefficient"
            (encodeInteger . (`div` 8) . unCoin . Al.unCoinPerWord)
            (Al.appCoinsPerUTxOWord x)
        <> encode
            "plutusCostModels"
            encodeCostModels
            (Al.appCostModels x)
        <> encode
            "scriptExecutionPrices"
            encodePrices
            (Al.appPrices x)
        <> encode
            "maxExecutionUnitsPerTransaction"
            (encodeExUnits . Al.unOrdExUnits)
            (Al.appMaxTxExUnits x)
        <> encode
            "maxExecutionUnitsPerBlock"
            (encodeExUnits . Al.unOrdExUnits)
            (Al.appMaxBlockExUnits x)
        <> encode
            "maxValueSize"
            (encodeSingleton "bytes" . encodeWord32)
            (Al.appMaxValSize x)
        <> encode
            "collateralPercentage"
            encodeWord16
            (Al.appCollateralPercentage x)
        <> encode
            "maxCollateralInputs"
            encodeWord16
            (Al.appMaxCollateralInputs x)
        <> encode
            "version"
            Shelley.encodeProtVer
            (Al.appProtocolVersion x)
        & encodeObject

encodePrices ::
    Al.Prices ->
    Json
encodePrices prices =
    "memory"
        .= encodeNonNegativeInterval (Al.prMem prices)
        <> "cpu"
        .= encodeNonNegativeInterval (Al.prSteps prices)
        & encodeObject

encodeRedeemers ::
    forall era.
    (Al.AlonzoEraScript era) =>
    (PlutusPurpose AsIx era -> Json) ->
    Al.Redeemers era ->
    Json
encodeRedeemers encodeScriptPurposeIndexInEra (Al.Redeemers redeemers) =
    encodeMapAsList encodeDataAndUnits redeemers
  where
    encodeDataAndUnits ::
        PlutusPurpose AsIx era ->
        (Al.Data era, Al.ExUnits) ->
        Json
    encodeDataAndUnits ptr (redeemer, units) =
        "validator"
            .= encodeScriptPurposeIndexInEra ptr
            <> "redeemer"
            .= encodeData redeemer
            <> "executionUnits"
            .= encodeExUnits units
            & encodeObject

encodeScript ::
    ( Ledger.Script era ~ Al.AlonzoScript era
    , Al.AlonzoEraScript era
    , Ledger.NativeScript era ~ Timelock era
    ) =>
    IncludeCbor ->
    Al.Script era ->
    Json
encodeScript opts =
    encodeObject . \case
        Al.NativeScript nativeScript ->
            "language"
                .= encodeText "native"
                <> "json"
                .= Allegra.encodeTimelock nativeScript
                <> if includeScriptCbor opts
                    then
                        "cbor"
                            .= encodeByteStringBase16 (Ledger.originalBytes nativeScript)
                    else
                        mempty
        Al.PlutusScript script ->
            "language"
                .= encodeText (stringifyLanguage (Al.plutusScriptLanguage script))
                <> "cbor"
                .= encodeByteStringBase16 (Ledger.originalBytes (Al.plutusScriptBinary script))

encodeScriptPurposeIndex ::
    Al.AlonzoPlutusPurpose Ledger.AsIx era ->
    Json
encodeScriptPurposeIndex =
    encodeObject . \case
        Al.AlonzoSpending (Ledger.AsIx ix) ->
            ( "index"
                .= encodeWord32 ix
                <> "purpose"
                .= encodeText "spend"
            )
        Al.AlonzoMinting (Ledger.AsIx ix) ->
            ( "index"
                .= encodeWord32 ix
                <> "purpose"
                .= encodeText "mint"
            )
        Al.AlonzoCertifying (Ledger.AsIx ix) ->
            ( "index"
                .= encodeWord32 ix
                <> "purpose"
                .= encodeText "publish"
            )
        Al.AlonzoWithdrawing (Ledger.AsIx ix) ->
            ( "index"
                .= encodeWord32 ix
                <> "purpose"
                .= encodeText "withdraw"
            )

encodeScriptPurposeItem ::
    forall era.
    (Ledger.TxCert era ~ Sh.ShelleyTxCert era) =>
    Al.AlonzoPlutusPurpose Ledger.AsItem era ->
    StrictMaybe Json
encodeScriptPurposeItem =
    fmap encodeObject . \case
        Al.AlonzoSpending (Ledger.AsItem txIn) ->
            SJust
                $ "purpose"
                .= encodeText "spend"
                <> "outputReference"
                .= encodeObject (Shelley.encodeTxIn txIn)
        Al.AlonzoMinting (Ledger.AsItem policyId) ->
            SJust
                $ "purpose"
                .= encodeText "mint"
                <> "policy"
                .= Mary.encodePolicyId policyId
        Al.AlonzoWithdrawing (Ledger.AsItem acct) ->
            SJust
                $ "purpose"
                .= encodeText "withdraw"
                <> "rewardAccount"
                .= Shelley.encodeRewardAcnt acct
        Al.AlonzoCertifying (Ledger.AsItem cert) -> do
            -- NOTE: MIR certificate never appear in certifying purpose.
            c <- fst (Shelley.encodeTxCert cert)
            pure
                $ "purpose"
                .= encodeText "publish"
                <> "certificate"
                .= encodeObject c

encodeTx ::
    (MetadataFormat, IncludeCbor) ->
    Al.AlonzoTx Ledger.TopTx AlonzoEra ->
    Json
encodeTx (fmt, opts) x =
    encodeObject
        ( Shelley.encodeTxId (Ledger.txIdTxBody @AlonzoEra (Al.atBody x))
            <> "spends"
            .= encodeIsValid (Al.atIsPhase2Valid x)
            <> encodeTxBody (Al.atBody x) (strictMaybe mempty (Map.keys . snd) auxiliary)
            <> "metadata"
            .=? OmitWhenNothing fst auxiliary
            <> encodeWitnessSet opts (snd <$> auxiliary) encodeScriptPurposeIndex (Al.atWits x)
            <> if includeTransactionCbor opts
                then
                    "cbor" .= encodeByteStringBase16 (encodeCbor @AlonzoEra x)
                else
                    mempty
        )
  where
    auxiliary = do
        hash <- Shelley.encodeAuxiliaryDataHash <$> Al.atbAuxDataHash (Al.atBody x)
        (labels, scripts) <- encodeAuxiliaryData (fmt, opts) <$> Al.atAuxData x
        pure
            ( encodeObject ("hash" .= hash <> "labels" .= labels)
            , scripts
            )

encodeTxBody ::
    Ledger.TxBody Ledger.TopTx AlonzoEra ->
    [Ledger.ScriptHash] ->
    Series
encodeTxBody x scripts =
    "inputs"
        .= encodeFoldable (encodeObject . Shelley.encodeTxIn) (Al.atbInputs x)
        <> "outputs"
        .= encodeFoldable (encodeObject . encodeTxOut) (Al.atbOutputs x)
        <> "collaterals"
        .=? OmitWhen
            null
            (encodeFoldable (encodeObject . Shelley.encodeTxIn))
            (Al.atbCollateral x)
        <> "certificates"
        .=? OmitWhen
            null
            (encodeList encodeObject)
            certs
        <> "withdrawals"
        .=? OmitWhen
            (null . Ledger.unWithdrawals)
            Shelley.encodeWdrl
            (Al.atbWithdrawals x)
        <> "mint"
        .=? OmitWhen
            (== mempty)
            (encodeObject . Mary.encodeMultiAsset)
            (Al.atbMint x)
        <> "requiredExtraSignatories"
        .=? OmitWhen
            null
            (encodeFoldable Shelley.encodeKeyHash)
            (Al.atbReqSignerHashes x)
        <> "requiredExtraScripts"
        .=? OmitWhen
            null
            (encodeFoldable Shelley.encodeScriptHash)
            scripts
        <> "network"
        .=? OmitWhenNothing
            Shelley.encodeNetwork
            (Al.atbTxNetworkId x)
        <> "scriptIntegrityHash"
        .=? OmitWhenNothing
            encodeScriptIntegrityHash
            (Al.atbScriptIntegrityHash x)
        <> "fee"
        .= encodeCoin (Al.atbTxFee x)
        <> "validityInterval"
        .= Allegra.encodeValidityInterval (Al.atbValidityInterval x)
        <> "proposals"
        .=? OmitWhen
            null
            (encodeList (encodeSingleton "action"))
            actions
        <> "votes"
        .=? OmitWhen
            null
            (encodeList Shelley.encodeGenesisVote)
            votes
  where
    (certs, mirs) =
        Shelley.encodeTxCerts (Al.atbCerts x)

    (votes, actions) =
        fromSMaybe ([], mirs)
            $ Shelley.encodeUpdate encodePParamsUpdate mirs
            <$> Al.atbUpdate x

encodeTxOut ::
    Al.AlonzoTxOut AlonzoEra ->
    Series
encodeTxOut (Al.AlonzoTxOut addr value datum) =
    "address"
        .= Shelley.encodeAddress addr
        <> "value"
        .= Mary.encodeValue value
        <> "datumHash"
        .=? OmitWhenNothing
            encodeDataHash
            datum

encodeUtxo ::
    Sh.UTxO AlonzoEra ->
    Json
encodeUtxo =
    encodeList id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO i o = encodeObject (Shelley.encodeTxIn i <> encodeTxOut o)

encodeScriptIntegrityHash ::
    Al.ScriptIntegrityHash ->
    Json
encodeScriptIntegrityHash =
    Shelley.encodeHash . Ledger.extractHash

encodeWitnessSet ::
    ( Ledger.Script era ~ Al.AlonzoScript era
    , Al.AlonzoEraScript era
    , Ledger.NativeScript era ~ Timelock era
    ) =>
    IncludeCbor ->
    StrictMaybe (AuxiliaryScripts era) ->
    (PlutusPurpose AsIx era -> Json) ->
    Al.AlonzoTxWits era ->
    Series
encodeWitnessSet opts (fromSMaybe mempty -> auxScripts) encodeScriptPurposeIndexInEra x =
    "signatories"
        .= encodeFoldable2
            Shelley.encodeBootstrapWitness
            Shelley.encodeWitVKey
            (Al.txwitsBoot x)
            (Al.txwitsVKey x)
        <> "scripts"
        .=? OmitWhen
            null
            (encodeMap Shelley.stringifyScriptHash (encodeScript opts))
            (Al.txscripts x <> auxScripts)
        <> "datums"
        .=? OmitWhen
            null
            (encodeMap stringifyDataHash encodeData)
            (Al.unTxDats $ Al.txdats x)
        <> "redeemers"
        .=? OmitWhen
            (\(Al.Redeemers redeemers) -> null redeemers)
            (encodeRedeemers encodeScriptPurposeIndexInEra)
            (Al.txrdmrs x)

--
-- Conversion To Text
--

stringifyDataHash ::
    Al.DataHash ->
    Text
stringifyDataHash (Ledger.extractHash -> (CC.UnsafeHash h)) =
    encodeBase16 (fromShort h)

stringifyLanguage ::
    Ledger.Language ->
    Text
stringifyLanguage = \case
    Ledger.PlutusV1 -> "plutus:v1"
    Ledger.PlutusV2 -> "plutus:v2"
    Ledger.PlutusV3 -> "plutus:v3"
    Ledger.PlutusV4 -> "plutus:v4"
