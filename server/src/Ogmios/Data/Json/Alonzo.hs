--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Alonzo where

import Ogmios.Data.Json.Prelude

import Data.ByteString.Base16
    ( encodeBase16
    )
import Data.Maybe.Strict
    ( fromSMaybe
    , strictMaybe
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
import qualified Cardano.Ledger.Binary as Binary

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
    => IncludeCbor
    -> Al.AlonzoTxAuxData era
    -> (Json, AuxiliaryScripts era)
encodeAuxiliaryData opts (Al.AlonzoTxAuxData blob timelocks plutus) =
    ( Shelley.encodeMetadataBlob @era opts blob
    , foldr
        (\(Al.TimelockScript -> script) -> Map.insert (Ledger.hashScript @era script) script)
        (Map.foldrWithKey
            (\lang ->
                flip $ foldr (\(Al.BinaryPlutus bytes) ->
                    let script = Al.PlutusScript (Al.Plutus lang (Al.BinaryPlutus bytes))
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
    :: Crypto crypto
    => IncludeCbor
    -> ShelleyBlock (TPraos crypto) (AlonzoEra crypto)
    -> Json
encodeBlock opts (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "type" .= encodeText "praos"
        <>
          "era" .= encodeText "alonzo"
        <>
          "id" .= Shelley.encodeShelleyHash headerHash
        <>
          Shelley.encodeBHeader blkHeader
        <>
          "size" .= encodeSingleton "bytes" (encodeNatural (TPraos.bsize hBody))
        <>
          "transactions" .= encodeFoldable (encodeTx opts) (Al.txSeqTxns txs)
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
    :: forall era.
        ( Ledger.Era era
        )
    => Al.Data era
    -> Json
encodeData =
    encodeByteStringBase16 . Binary.serialize' (Ledger.eraProtVerLow @era)

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
    "cpu" .=
        encodeNatural (Al.exUnitsSteps units)
    & encodeObject

encodeGenesis
    :: Al.AlonzoGenesis
    -> Json
encodeGenesis x =
    encodeObject
        ( "era" .= encodeText "alonzo"
       <> "updatableParameters" .= encodeObject
            ( "minUtxoDepositCoefficient" .=
                (encodeInteger . (`div` 8) . unCoin . Al.unCoinPerWord) (Al.agCoinsPerUTxOWord x) <>
              "plutusCostModels" .=
                  encodeCostModels (Al.agCostModels x) <>
              "scriptExecutionPrices" .=
                  encodePrices (Al.agPrices x) <>
              "maxExecutionUnitsPerTransaction" .=
                  encodeExUnits (Al.agMaxTxExUnits x) <>
              "maxExecutionUnitsPerBlock" .=
                  encodeExUnits (Al.agMaxBlockExUnits x) <>
              "maxValueSize" .=
                  (encodeSingleton "bytes" . encodeNatural) (Al.agMaxValSize x) <>
              "collateralPercentage" .=
                  encodeNatural (Al.agCollateralPercentage x) <>
              "maxCollateralInputs" .=
                    encodeNatural (Al.agMaxCollateralInputs x)
            )
        )

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

encodePParams
    :: (Ledger.PParamsHKD Identity era ~ Al.AlonzoPParams Identity era)
    => Ledger.PParams era
    -> Json
encodePParams (Ledger.PParams x) =
    encodePParamsHKD (\k encode v -> k .= encode v) identity x

encodePParamsUpdate
    :: forall era.
        ( Ledger.PParamsHKD StrictMaybe era ~ Al.AlonzoPParams StrictMaybe era
        )
    => Ledger.PParamsUpdate era
    -> [Json]
encodePParamsUpdate (Ledger.PParamsUpdate x) =
    case (Al.appProtocolVersion x, x' == Al.emptyAlonzoPParamsUpdate) of
        (SJust version, True) ->
            [ encodeObject
                ( "type" .=
                    encodeText "hardForkInitiation"
               <> "version" .=
                    Shelley.encodeProtVer version
                )
            ]
        (SJust version, False) ->
            [ encodeObject
                ( "type" .=
                    encodeText "hardForkInitiation"
               <> "version" .=
                    Shelley.encodeProtVer version
                )
            , encodeObject
                ( "type" .=
                    encodeText "protocolParametersUpdate"
               <> "parameters" .=
                    encodePParamsHKD
                        (\k encode v -> k .=? OmitWhenNothing encode v)
                        (const SNothing)
                        x'
                )
            ]
        (SNothing, _) ->
            [ encodeObject
                ( "type" .=
                    encodeText "protocolParametersUpdate"
               <> "parameters" .=
                    encodePParamsHKD
                        (\k encode v -> k .=? OmitWhenNothing encode v)
                        (const SNothing)
                        x'
                )
            ]
  where
    x' :: Ledger.PParamsHKD StrictMaybe era
    x' = x { Al.appProtocolVersion = SNothing }

encodeProposedPPUpdates
    :: forall era.
        ( Ledger.PParamsHKD StrictMaybe era ~ Al.AlonzoPParams StrictMaybe era
        )
    => Sh.ProposedPPUpdates era
    -> Json
encodeProposedPPUpdates (Sh.ProposedPPUpdates m) =
    encodeFoldable
        (\(Ledger.PParamsUpdate x) ->
            encodePParamsHKD
                (\k encode v -> k .=? OmitWhenNothing encode v)
                (const SNothing)
                x
        )
        m

encodePParamsHKD
    :: (forall a. Text -> (a -> Json) -> Sh.HKD f a -> Series)
    -> (Integer -> Sh.HKD f Integer)
    -> Al.AlonzoPParams f era
    -> Json
encodePParamsHKD encode pure_ x =
    encode "minFeeCoefficient"
        (encodeInteger . unCoin) (Al.appMinFeeA x) <>
    encode "minFeeConstant"
        encodeCoin (Al.appMinFeeB x) <>
    encode "maxBlockBodySize"
        (encodeSingleton "bytes" . encodeNatural) (Al.appMaxBBSize x) <>
    encode "maxBlockHeaderSize"
        (encodeSingleton "bytes" . encodeNatural) (Al.appMaxBHSize x) <>
    encode "maxTransactionSize"
        (encodeSingleton "bytes" . encodeNatural) (Al.appMaxTxSize x) <>
    encode "stakeCredentialDeposit"
        encodeCoin (Al.appKeyDeposit x) <>
    encode "stakePoolDeposit"
        encodeCoin (Al.appPoolDeposit x) <>
    encode "stakePoolRetirementEpochBound"
        encodeEpochNo (Al.appEMax x) <>
    encode "desiredNumberOfStakePools"
        encodeNatural (Al.appNOpt x) <>
    encode "stakePoolPledgeInfluence"
        encodeNonNegativeInterval (Al.appA0 x) <>
    encode "monetaryExpansion"
        encodeUnitInterval (Al.appRho x) <>
    encode "treasuryExpansion"
        encodeUnitInterval (Al.appTau x) <>
    encode "federatedBlockProductionRatio"
        encodeUnitInterval (Al.appD x) <>
    encode "extraEntropy"
        Shelley.encodeNonce (Al.appExtraEntropy x) <>
    encode "minStakePoolCost"
        encodeCoin (Al.appMinPoolCost x) <>
    encode "minUtxoDepositConstant"
        encodeInteger (pure_ 0) <>
    encode "minUtxoDepositCoefficient"
        (encodeInteger . (`div` 8) . unCoin . Al.unCoinPerWord) (Al.appCoinsPerUTxOWord x) <>
    encode "plutusCostModels"
        encodeCostModels (Al.appCostModels x) <>
    encode "scriptExecutionPrices"
        encodePrices (Al.appPrices x) <>
    encode "maxExecutionUnitsPerTransaction"
        (encodeExUnits . Al.unOrdExUnits) (Al.appMaxTxExUnits x) <>
    encode "maxExecutionUnitsPerBlock"
        (encodeExUnits . Al.unOrdExUnits) (Al.appMaxBlockExUnits x) <>
    encode "maxValueSize"
        (encodeSingleton "bytes" . encodeNatural) (Al.appMaxValSize x) <>
    encode "collateralPercentage"
        encodeNatural (Al.appCollateralPercentage x) <>
    encode "maxCollateralInputs"
        encodeNatural (Al.appMaxCollateralInputs x) <>
    encode "version"
        Shelley.encodeProtVer (Al.appProtocolVersion x)
    & encodeObject

encodePrices
    :: Al.Prices
    -> Json
encodePrices prices =
    "memory" .=
        encodeNonNegativeInterval (Al.prMem prices) <>
    "cpu" .=
        encodeNonNegativeInterval (Al.prSteps prices)
    & encodeObject

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
    => IncludeCbor
    -> Al.Script era
    -> Json
encodeScript opts = encodeObject . \case
    Al.TimelockScript nativeScript ->
        "language" .=
            encodeText "native" <>
        "json" .=
            Allegra.encodeTimelock nativeScript <>
        if includeScriptCbor opts then
            "cbor" .=
                encodeByteStringBase16 (Ledger.originalBytes nativeScript)
        else
            mempty
    Al.PlutusScript Al.Plutus{Al.plutusLanguage, Al.plutusScript} ->
        "language" .=
            encodeText (stringifyLanguage plutusLanguage) <>
        "cbor" .=
            encodeByteStringBase16 (Ledger.originalBytes plutusScript)

encodeScriptPurpose
    :: forall era crypto.
        ( EraCrypto (era crypto) ~ crypto
        , Crypto crypto
        )
    => (Ledger.TxCert (era crypto) -> [Series])
    -> Al.ScriptPurpose (era crypto)
    -> StrictMaybe Json
encodeScriptPurpose encodeTxCert = fmap encodeObject . \case
    Al.Spending txIn ->
        SJust $
            "purpose" .= encodeText "spend" <>
            "outputReference" .= encodeObject (Shelley.encodeTxIn txIn)
    Al.Minting policyId ->
        SJust $
            "purpose" .= encodeText "mint" <>
            "policy" .= Mary.encodePolicyId policyId
    Al.Rewarding acct ->
        SJust $
            "purpose" .= encodeText "withdraw" <>
            "rewardAccount" .= Shelley.encodeRewardAcnt acct
    Al.Certifying cert ->
        case encodeTxCert cert of
            [] ->
                SNothing
            [c] ->
                SJust $
                    "purpose" .= encodeText "publish" <>
                    "certificate" .= encodeObject c
            (_:c:_) ->
                SJust $
                    "purpose" .= encodeText "publish" <>
                    "certificate" .= encodeObject c

encodeTx
    :: forall era crypto.
        ( Crypto crypto
        , era ~ AlonzoEra crypto
        )
    => IncludeCbor
    -> Al.AlonzoTx era
    -> Json
encodeTx opts x =
    encodeObject
        ( Shelley.encodeTxId (Ledger.txid @(AlonzoEra crypto) (Al.body x))
       <>
        "spends" .= encodeIsValid (Al.isValid x)
       <>
        encodeTxBody (Al.body x) (strictMaybe mempty (Map.keys . snd) auxiliary)
       <>
        "metadata" .=? OmitWhenNothing fst auxiliary
       <>
        encodeWitnessSet opts (snd <$> auxiliary) (Al.wits x)
       <>
        if includeTransactionCbor opts then
           "cbor" .= encodeByteStringBase16 (Binary.serialize' (Ledger.eraProtVerLow @era) x)
        else
           mempty
       )
  where
    auxiliary = do
        hash <- Shelley.encodeAuxiliaryDataHash <$> Al.atbAuxDataHash (Al.body x)
        (labels, scripts) <- encodeAuxiliaryData opts <$> Al.auxiliaryData x
        pure
            ( encodeObject ("hash" .= hash <> "labels" .= labels)
            , scripts
            )

encodeTxBody
    :: Crypto crypto
    => Al.AlonzoTxBody (AlonzoEra crypto)
    -> [Ledger.ScriptHash crypto]
    -> Series
encodeTxBody x scripts =
    "inputs" .=
        encodeFoldable (encodeObject . Shelley.encodeTxIn) (Al.atbInputs x) <>
    "outputs" .=
        encodeFoldable (encodeObject . encodeTxOut) (Al.atbOutputs x) <>
    "collaterals" .=? OmitWhen null
        (encodeFoldable (encodeObject . Shelley.encodeTxIn)) (Al.atbCollateral x) <>
    "certificates" .=? OmitWhen null
        (encodeList encodeObject) certs <>
    "withdrawals" .=? OmitWhen (null . Ledger.unWithdrawals)
        Shelley.encodeWdrl (Al.atbWithdrawals x) <>
            "mint" .=? OmitWhen (== mempty)
        (encodeObject . Mary.encodeMultiAsset) (Al.atbMint x) <>
    "requiredExtraSignatories".=? OmitWhen null
        (encodeFoldable Shelley.encodeKeyHash) (Al.atbReqSignerHashes x) <>
    "requiredExtraScripts" .=? OmitWhen null
        (encodeFoldable Shelley.encodeScriptHash) scripts <>
    "network" .=? OmitWhenNothing
        Shelley.encodeNetwork (Al.atbTxNetworkId x) <>
    "scriptIntegrityHash" .=? OmitWhenNothing
        encodeScriptIntegrityHash (Al.atbScriptIntegrityHash x) <>
    "fee" .=
        encodeCoin (Al.atbTxFee x) <>
    "validityInterval" .=
        Allegra.encodeValidityInterval (Al.atbValidityInterval x) <>
    "proposals" .=? OmitWhen null
        (encodeList (encodeSingleton "action")) actions <>
    "votes" .=? OmitWhen null
        (encodeList Shelley.encodeGenesisVote) votes
  where
    (certs, mirs) =
        Shelley.encodeTxCerts (Al.atbCerts x)

    (votes, actions) = fromSMaybe ([], mirs) $
        Shelley.encodeUpdate encodePParamsUpdate mirs <$> Al.atbUpdate x

encodeTxOut
    :: Crypto crypto
    => Al.AlonzoTxOut (AlonzoEra crypto)
    -> Series
encodeTxOut (Al.AlonzoTxOut addr value datum) =
    "address" .=
        Shelley.encodeAddress addr <>
    "value" .=
        Mary.encodeValue value <>
    "datumHash" .=? OmitWhenNothing
        encodeDataHash datum

encodeUtxo
    :: Crypto crypto
    => Sh.UTxO (AlonzoEra crypto)
    -> Json
encodeUtxo =
    encodeList id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO i o = encodeObject (Shelley.encodeTxIn i <> encodeTxOut o)

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
    => IncludeCbor
    -> StrictMaybe (AuxiliaryScripts era)
    -> Al.AlonzoTxWits era
    -> Series
encodeWitnessSet opts (fromSMaybe mempty -> auxScripts) x =
    "signatories" .=
        encodeFoldable2
            Shelley.encodeBootstrapWitness
            Shelley.encodeWitVKey
            (Al.txwitsBoot x)
            (Al.txwitsVKey x) <>
    "scripts" .=? OmitWhen null
        (encodeMap Shelley.stringifyScriptHash (encodeScript opts))
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
