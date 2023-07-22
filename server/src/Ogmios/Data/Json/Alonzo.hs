--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Alonzo where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( serialize'
    )
import Cardano.Ledger.Crypto
    ( Crypto
    )
import Cardano.Ledger.Era
    ( Era
    )
import Data.ByteString.Base16
    ( encodeBase16
    )
import Data.Maybe.Strict
    ( fromSMaybe
    )
import Ouroboros.Consensus.Cardano.Block
    ( AlonzoEra
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()
import Prettyprinter
    ( pretty
    )

import qualified Data.Map.Strict as Map

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

import qualified Cardano.Protocol.TPraos.BHeader as TPraos

import qualified Cardano.Crypto.Hash.Class as CC

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger hiding
    ( TranslationError
    )
import qualified Cardano.Ledger.SafeHash as Ledger

import qualified Cardano.Ledger.Shelley.API as Sh
import qualified Cardano.Ledger.Shelley.PParams as Sh
import qualified Cardano.Ledger.Shelley.Rules as Sh

import qualified Cardano.Ledger.Mary.Value as Ma

import qualified Cardano.Ledger.Alonzo.Core as Al hiding
    ( TranslationError
    )
import qualified Cardano.Ledger.Alonzo.Genesis as Al
import qualified Cardano.Ledger.Alonzo.Language as Al
import qualified Cardano.Ledger.Alonzo.PParams as Al
import qualified Cardano.Ledger.Alonzo.PlutusScriptApi as Al
import qualified Cardano.Ledger.Alonzo.Rules as Al
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

import qualified Cardano.Ledger.Api as Ledger.Api



type AuxiliaryScripts era =
    Map (Ledger.ScriptHash (Ledger.EraCrypto era)) (Ledger.Script era)

--
-- Encoders
--

encodeUtxowPredicateFail
    :: forall era crypto.
        ( crypto ~ Ledger.EraCrypto era
        , Ledger.Era era
        )
    => (Sh.PredicateFailure (Ledger.EraRule "UTXO" era) -> Json)
    -> Al.AlonzoUtxowPredFailure era
    -> Json
encodeUtxowPredicateFail encodeUtxoFailureInEra = \case
    Al.MissingRedeemers missing ->
        "missingRequiredRedeemers" .= encodeObject
            ( "missing" .=
                encodeFoldable encodeMissingRedeemer missing
            )
        & encodeObject
    Al.MissingRequiredDatums missing provided ->
        "missingRequiredDatums" .= encodeObject
            ( "provided" .=
                encodeFoldable encodeDataHash provided <>
              "missing" .=
                encodeFoldable encodeDataHash missing
            )
        & encodeObject
    Al.NonOutputSupplimentaryDatums unallowed acceptable ->
        "unspendableDatums" .= encodeObject
            ( "nonSpendable" .=
                encodeFoldable encodeDataHash unallowed <>
              "acceptable" .=
                encodeFoldable encodeDataHash acceptable
            )
        & encodeObject
    Al.PPViewHashesDontMatch provided inferred ->
        "extraDataMismatch" .= encodeObject
            ( "provided" .=? OmitWhenNothing
                encodeScriptIntegrityHash provided <>
              "inferredFromParameters" .=? OmitWhenNothing
                encodeScriptIntegrityHash inferred
            )
        & encodeObject
    Al.MissingRequiredSigners keys ->
        "missingRequiredSignatures" .=
            encodeFoldable Shelley.encodeKeyHash keys
        & encodeObject
    Al.UnspendableUTxONoDatumHash utxos ->
        "unspendableScriptInputs" .=
            encodeFoldable Shelley.encodeTxIn utxos
        & encodeObject
    Al.ExtraRedeemers redeemers ->
        "extraRedeemers" .=
            encodeFoldable (encodeText . stringifyRdmrPtr) redeemers
        & encodeObject
    Al.ShelleyInAlonzoUtxowPredFailure e ->
        Shelley.encodeUtxowFailure encodeUtxoFailureInEra e

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
        ( "era" .= encodeText "alonzo"
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

encodeCollectError
    :: Crypto crypto
    => Al.CollectError crypto
    -> Json
encodeCollectError = \case
    Al.NoRedeemer purpose ->
        encodeObject ("noRedeemer" .= encodeScriptPurpose purpose)
    Al.NoWitness hash ->
        encodeObject ("noWitness" .= Shelley.encodeScriptHash hash)
    Al.NoCostModel lang ->
        encodeObject ("noCostModel" .= encodeLanguage lang)
    Al.BadTranslation err ->
        encodeObject ("badTranslation" .= encodeTranslationError err)

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
        encodeCoin (Al.unCoinPerWord (Al.agCoinsPerUTxOWord x)) <>
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

encodeLedgerFailure
    :: Crypto crypto
    => Sh.ShelleyLedgerPredFailure (AlonzoEra crypto)
    -> Json
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        encodeUtxowPredicateFail
            (encodeUtxoFailure
                encodeUtxo
                encodeTxOut
                (\(Al.AlonzoTxOut addr _ _) -> addr)
            ) e
    Sh.DelegsFailure e ->
        Shelley.encodeDelegsFailure e

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
        encodeCoin (Al.appMinFeeA x) <>
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
        (encodeCoin . Al.unCoinPerWord) (Al.appCoinsPerUTxOWord x) <>
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
        Mary.encodeMultiAsset (Al.atbMint x) <>
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

encodeUtxoFailure
    :: forall era.
        ( Era era
        , Al.Value era ~ Ma.MaryValue (Ledger.EraCrypto era)
        , Sh.PredicateFailure (Ledger.EraRule "UTXOS" era) ~ Al.AlonzoUtxosPredFailure era
        , Sh.PPUPPredFailure era ~ Sh.ShelleyPpupPredFailure era
        )
    => (Sh.UTxO era -> Json)
    -> (Ledger.TxOut era -> Json)
    -> (Ledger.TxOut era -> Sh.Addr (Ledger.EraCrypto era))
    -> Al.AlonzoUtxoPredFailure era
    -> Json
encodeUtxoFailure encodeUtxoInEra encodeTxOutInEra extractAddress = \case
    Al.BadInputsUTxO inputs ->
        "badInputs" .=
            encodeFoldable Shelley.encodeTxIn inputs
        & encodeObject
    Al.OutsideValidityIntervalUTxO itv currentSlot ->
        "outsideOfValidityInterval" .= encodeObject
            ( "interval" .=
                Allegra.encodeValidityInterval itv <>
              "currentSlot" .=
                encodeSlotNo currentSlot
            )
        & encodeObject
    Al.MaxTxSizeUTxO actualSize maxSize ->
        "txTooLarge" .= encodeObject
            ( "maximumSize" .=
                encodeInteger maxSize <>
              "actualSize" .=
                encodeInteger actualSize
            )
        & encodeObject
    Al.InputSetEmptyUTxO ->
        "missingAtLeastOneInputUtxo" .=
            encodeNull
        & encodeObject
    Al.FeeTooSmallUTxO required actual ->
        "feeTooSmall" .= encodeObject
            ( "requiredFee" .=
                encodeCoin required <>
              "actualFee" .=
                encodeCoin actual
            )
        & encodeObject
    Al.ValueNotConservedUTxO consumed produced ->
        "valueNotConserved" .= encodeObject
            ( "consumed" .=
                Mary.encodeValue @(Ledger.EraCrypto era) consumed <>
              "produced" .=
                Mary.encodeValue produced
            )
        & encodeObject
    Al.WrongNetwork expected invalidAddrs ->
        "networkMismatch" .= encodeObject
            ( "expectedNetwork" .=
                Shelley.encodeNetwork expected <>
              "invalidEntities" .=
                Shelley.encodeEntities "address" Shelley.encodeAddress invalidAddrs
            )
        & encodeObject
    Al.WrongNetworkWithdrawal expected invalidAccts ->
        "networkMismatch" .= encodeObject
            ( "expectedNetwork" .=
                Shelley.encodeNetwork expected <>
              "invalidEntities" .=
                Shelley.encodeEntities "rewardAccount" Shelley.encodeRewardAcnt invalidAccts
            )
        & encodeObject
    Al.WrongNetworkInTxBody expected actual ->
        "networkMismatch" .= encodeObject
            ( "expectedNetwork" .=
                Shelley.encodeNetwork expected<>
              "invalidEntities" .=
                Shelley.encodeEntities "transactionBody" Shelley.encodeNetwork [actual]
            )
        & encodeObject
    Al.OutputTooSmallUTxO outs ->
        "outputTooSmall" .=
            encodeFoldable encodeTxOutInEra outs
        & encodeObject
    Al.OutputBootAddrAttrsTooBig outs ->
        "addressAttributesTooLarge" .=
            encodeFoldable Shelley.encodeAddress (extractAddress <$> outs)
        & encodeObject
    Al.TriesToForgeADA ->
        "triesToForgeAda" .=
            encodeNull
        & encodeObject
    Al.OutputTooBigUTxO outs ->
        "tooManyAssetsInOutput" .=
            encodeFoldable (\(_, _, o) -> encodeTxOutInEra o) outs
        & encodeObject
    Al.NoCollateralInputs ->
        "missingCollateralInputs" .=
            encodeNull
        & encodeObject
    Al.InsufficientCollateral actual required ->
        "collateralTooSmall" .= encodeObject
            ( "requiredCollateral" .=
                encodeCoin required <>
              "actualCollateral" .=
                encodeCoin actual
            )
        & encodeObject
    Al.ScriptsNotPaidUTxO utxo ->
        "collateralIsScript" .=
            encodeUtxoInEra utxo
        & encodeObject
    Al.CollateralContainsNonADA value ->
        "collateralHasNonAdaAssets" .=
            Mary.encodeValue value
        & encodeObject
    Al.TooManyCollateralInputs maxInputs actualInputs ->
        "tooManyCollateralInputs" .= encodeObject
            ( "maximumCollateralInputs" .=
                encodeNatural maxInputs <>
              "actualCollateralInputs" .=
                encodeNatural actualInputs
            )
        & encodeObject
    Al.ExUnitsTooBigUTxO maxUnit actualUnit ->
        "executionUnitsTooLarge" .= encodeObject
            ( "maximumExecutionUnits" .=
                encodeExUnits maxUnit <>
              "actualExecutionUnits" .=
                encodeExUnits actualUnit
            )
        & encodeObject
    Al.OutsideForecast slot ->
        "outsideForecast" .=
            encodeSlotNo slot
        & encodeObject
    Al.UtxosFailure e ->
        encodeUtxosPredicateFailure e

encodeUtxosPredicateFailure
    :: forall era.
        ( Sh.PPUPPredFailure era ~ Sh.ShelleyPpupPredFailure era
        , Ledger.Era era
        )
    => Al.AlonzoUtxosPredFailure era
    -> Json
encodeUtxosPredicateFailure = \case
    Al.ValidationTagMismatch{} ->
        "validationTagMismatch" .=
            encodeNull
        & encodeObject
    Al.CollectErrors errors ->
        "collectErrors" .=
            encodeFoldable encodeCollectError errors
        & encodeObject
    Al.UpdateFailure e ->
        Shelley.encodeUpdateFailure e

encodeScriptIntegrityHash
    :: Crypto crypto
    => Al.ScriptIntegrityHash crypto
    -> Json
encodeScriptIntegrityHash =
    Shelley.encodeHash . Ledger.extractHash

encodeScriptFailure
    :: Crypto crypto
    => Ledger.Api.TransactionScriptFailure crypto
    -> Json
encodeScriptFailure = encodeObject . \case
    -- NOTE: This 'RedeemerNotNeeded' error is likely redundant and misleading
    -- in the ledger code. It is raised when the script's language pointed by
    -- the redeemer is unknown.
    Ledger.Api.RedeemerNotNeeded ptr _ ->
        "extraRedeemers" .=
            encodeFoldable (encodeText . stringifyRdmrPtr) [ptr]
    Ledger.Api.RedeemerPointsToUnknownScriptHash ptr ->
        "extraRedeemers" .=
            encodeFoldable (encodeText . stringifyRdmrPtr) [ptr]
    Ledger.Api.MissingScript ptr resolved ->
        "missingRequiredScripts" .= encodeObject
            ( "missing" .=
                encodeFoldable (encodeText . stringifyRdmrPtr) [ptr] <>
              "resolved" .=
                encodeMap
                    stringifyRdmrPtr
                    (\(_, _, h) -> Shelley.encodeScriptHash h)
                    resolved
            )
    Ledger.Api.MissingDatum h ->
        "missingRequiredDatums" .= encodeObject
            ( "missing" .=
                encodeFoldable encodeDataHash [h]
            )
    Ledger.Api.ValidationFailure (Ledger.Api.ValidationFailedV1 err traces _debugLang) ->
        "validatorFailed" .= encodeObject
            ( "error" .=
                encodeText (show (pretty err)) <>
              "traces" .=
                encodeFoldable encodeText traces
            )
    Ledger.Api.ValidationFailure (Ledger.Api.ValidationFailedV2 err traces _debugLang) ->
        "validatorFailed" .= encodeObject
            ( "error" .=
                encodeText (show (pretty err)) <>
              "traces" .=
                encodeFoldable encodeText traces
            )
    Ledger.Api.ValidationFailure (Ledger.Api.ValidationFailedV3 err traces _debugLang) ->
        "validatorFailed" .= encodeObject
            ( "error" .=
                encodeText (show (pretty err)) <>
              "traces" .=
                encodeFoldable encodeText traces
            )
    Ledger.Api.UnknownTxIn i ->
        "unknownInputReferencedByRedeemer" .=
            Shelley.encodeTxIn i
    Ledger.Api.InvalidTxIn i ->
        "nonScriptInputReferencedByRedeemer" .=
            Shelley.encodeTxIn i
    Ledger.Api.IncompatibleBudget budget ->
        "illFormedExecutionBudget" .=? OmitWhenNothing
            encodeExUnits (maybeToStrictMaybe (Al.exBudgetToExUnits budget))
    Ledger.Api.NoCostModelInLedgerState lang ->
        "noCostModelForLanguage" .=
            encodeLanguage lang

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
