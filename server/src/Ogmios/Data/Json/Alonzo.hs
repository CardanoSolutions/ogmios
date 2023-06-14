--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Ogmios.Data.Json.Alonzo where

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
import Data.ByteString.Base16
    ( encodeBase16
    )
import Data.Maybe.Strict
    ( maybeToStrictMaybe
    )
import GHC.Records
    ( getField
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
import Data.Maybe.Strict
    ( fromSMaybe
    )

import qualified Data.Map.Strict as Map

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

import qualified Cardano.Crypto.Hash.Class as CC

import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Core as Ledger

import qualified Cardano.Ledger.Shelley.API as Sh
import qualified Cardano.Ledger.Shelley.PParams as Sh
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Sh

import qualified Cardano.Ledger.Alonzo.Data as Al
import qualified Cardano.Ledger.Alonzo.Genesis as Al
import qualified Cardano.Ledger.Alonzo.Language as Al
import qualified Cardano.Ledger.Alonzo.PParams as Al
import qualified Cardano.Ledger.Alonzo.PlutusScriptApi as Al
import qualified Cardano.Ledger.Alonzo.Scripts as Al
import qualified Cardano.Ledger.Alonzo.Tx as Al
import qualified Cardano.Ledger.Alonzo.TxBody as Al
import qualified Cardano.Ledger.Alonzo.TxInfo as Al hiding
    ( txscripts
    )
import qualified Cardano.Ledger.Alonzo.TxSeq as Al
import qualified Cardano.Ledger.Alonzo.TxWitness as Al

import qualified Cardano.Ledger.Alonzo.Tools as Ledger.Tools

import qualified Cardano.Ledger.Mary.Value as Ledger.Mary

import qualified Cardano.Ledger.Shelley.Rules.Deleg as Rules.Shelley
import qualified Cardano.Ledger.Shelley.Rules.Ppup as Rules.Shelley

import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Rules.Alonzo
import qualified Cardano.Ledger.Alonzo.Rules.Utxos as Rules.Alonzo
import qualified Cardano.Ledger.Alonzo.Rules.Utxow as Rules.Alonzo

type AuxiliaryScripts era =
    Map (Ledger.ScriptHash (Ledger.Crypto era)) (Ledger.Script era)

--
-- Encoders
--

encodeUtxowPredicateFail
    :: forall era crypto.
        ( crypto ~ Ledger.Crypto era
        , Ledger.Era era
        )
    => (Rules.Shelley.PredicateFailure (Ledger.EraRule "UTXO" era) -> Json)
    -> Rules.Alonzo.UtxowPredicateFail era
    -> Json
encodeUtxowPredicateFail encodeUtxoFailureInEra = \case
    Rules.Alonzo.MissingRedeemers missing ->
        "missingRequiredRedeemers" .= encodeObject
            ( "missing" .=
                encodeFoldable encodeMissingRedeemer missing
            )
        & encodeObject
    Rules.Alonzo.MissingRequiredDatums missing provided ->
        "missingRequiredDatums" .= encodeObject
            ( "provided" .=
                encodeFoldable encodeDataHash provided <>
              "missing" .=
                encodeFoldable encodeDataHash missing
            )
        & encodeObject
    Rules.Alonzo.NonOutputSupplimentaryDatums unallowed acceptable ->
        "unspendableDatums" .= encodeObject
            ( "nonSpendable" .=
                encodeFoldable encodeDataHash unallowed <>
              "acceptable" .=
                encodeFoldable encodeDataHash acceptable
            )
        & encodeObject
    Rules.Alonzo.PPViewHashesDontMatch provided inferred ->
        "extraDataMismatch" .= encodeObject
            ( "provided" .=? OmitWhenNothing
                encodeScriptIntegrityHash provided <>
              "inferredFromParameters" .=? OmitWhenNothing
                encodeScriptIntegrityHash inferred
            )
        & encodeObject
    Rules.Alonzo.MissingRequiredSigners keys ->
        "missingRequiredSignatures" .=
            encodeFoldable Shelley.encodeKeyHash keys
        & encodeObject
    Rules.Alonzo.UnspendableUTxONoDatumHash utxos ->
        "unspendableScriptInputs" .=
            encodeFoldable Shelley.encodeTxIn utxos
        & encodeObject
    Rules.Alonzo.ExtraRedeemers redeemers ->
        "extraRedeemers" .=
            encodeFoldable (encodeText . stringifyRdmrPtr) redeemers
        & encodeObject
    Rules.Alonzo.WrappedShelleyEraFailure e ->
        Shelley.encodeUtxowFailure encodeUtxoFailureInEra e

encodeAuxiliaryData
    :: forall era.
        ( Ledger.Era era
        , Ledger.ValidateScript era
        , Ledger.Script era ~ Al.Script era
        )
    => Al.AuxiliaryData era
    -> (Json, AuxiliaryScripts era)
encodeAuxiliaryData (Al.AuxiliaryData blob scripts) =
    ( Shelley.encodeMetadataBlob blob
    , foldr
        (\script -> Map.insert (Ledger.hashScript @era script) script)
        mempty
        scripts
    )

encodeBinaryData
    :: Al.BinaryData era
    -> Json
encodeBinaryData =
    encodeByteStringBase16 . Ledger.originalBytes

encodeBlock
    :: Crypto crypto
    => ShelleyBlock (TPraos crypto) (AlonzoEra crypto)
    -> Json
encodeBlock (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    "body" .=
        encodeFoldable encodeTx (Al.txSeqTxns txs) <>
    "header" .=
        Shelley.encodeBHeader blkHeader <>
    "headerHash" .=
        Shelley.encodeShelleyHash headerHash
    & encodeObject

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
    encodeMap id encodeInteger . Al.getCostModelParams

encodeCostModels
    :: Al.CostModels
    -> Json
encodeCostModels =
    encodeMap stringifyLanguage encodeCostModel . Al.unCostModels

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
        encodeCoin (Al.coinsPerUTxOWord x) <>
    "costModels" .=
        encodeCostModels (Al.costmdls x) <>
    "prices" .=
        encodePrices (Al.prices x) <>
    "maxExecutionUnitsPerTransaction" .=
        encodeExUnits (Al.maxTxExUnits x) <>
    "maxExecutionUnitsPerBlock" .=
        encodeExUnits (Al.maxBlockExUnits x) <>
    "maxValueSize" .=
        encodeNatural (Al.maxValSize x) <>
    "collateralPercentage" .=
        encodeNatural (Al.collateralPercentage x) <>
    "maxCollateralInputs" .=
        encodeNatural (Al.maxCollateralInputs x)
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
    :: (Crypto crypto)
    => Sh.LedgerPredicateFailure (AlonzoEra crypto)
    -> Json
encodeLedgerFailure = \case
    Sh.UtxowFailure e ->
        encodeUtxowPredicateFail
            (encodeUtxoFailure
                encodeUtxo
                encodeTxOut
                (\(Al.TxOut addr _ _) -> addr)
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

encodePParams'
    :: (forall a. Text -> (a -> Json) -> Sh.HKD f a -> Series)
    -> Al.PParams' f era
    -> Json
encodePParams' encode x =
    encode "minFeeCoefficient"
        encodeNatural (Al._minfeeA x) <>
    encode "minFeeConstant"
        encodeNatural (Al._minfeeB x) <>
    encode "maxBlockBodySize"
        encodeNatural (Al._maxBBSize x) <>
    encode "maxBlockHeaderSize"
        encodeNatural (Al._maxBHSize x) <>
    encode "maxTxSize"
        encodeNatural (Al._maxTxSize x) <>
    encode "stakeKeyDeposit"
        encodeCoin (Al._keyDeposit x) <>
    encode "poolDeposit"
        encodeCoin (Al._poolDeposit x) <>
    encode "poolRetirementEpochBound"
        encodeEpochNo (Al._eMax x) <>
    encode "desiredNumberOfPools"
        encodeNatural (Al._nOpt x) <>
    encode "poolInfluence"
        encodeNonNegativeInterval (Al._a0 x) <>
    encode "monetaryExpansion"
        encodeUnitInterval (Al._rho x) <>
    encode "treasuryExpansion"
        encodeUnitInterval (Al._tau x) <>
    encode "decentralizationParameter"
        encodeUnitInterval (Al._d x) <>
    encode "extraEntropy"
        Shelley.encodeNonce (Al._extraEntropy x) <>
    encode "protocolVersion"
        Shelley.encodeProtVer (Al._protocolVersion x) <>
    encode "minPoolCost"
        encodeCoin (Al._minPoolCost x) <>
    encode "coinsPerUtxoWord"
        encodeCoin (Al._coinsPerUTxOWord x) <>
    encode "costModels"
        encodeCostModels (Al._costmdls x) <>
    encode "prices"
        encodePrices (Al._prices x) <>
    encode "maxExecutionUnitsPerTransaction"
        encodeExUnits (Al._maxTxExUnits x) <>
    encode "maxExecutionUnitsPerBlock"
        encodeExUnits (Al._maxBlockExUnits x) <>
    encode "maxValueSize"
        encodeNatural (Al._maxValSize x) <>
    encode "collateralPercentage"
        encodeNatural (Al._collateralPercentage x) <>
    encode "maxCollateralInputs"
        encodeNatural (Al._maxCollateralInputs x)
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
    encodeMap
        Shelley.stringifyKeyHash
        (encodePParams' (\k encode v -> k .=? OmitWhenNothing encode v))
        m

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
    :: (Crypto (Ledger.Crypto era))
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
    :: forall crypto. Crypto crypto
    => Al.ValidatedTx (AlonzoEra crypto)
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
    adHash :: Al.TxBody era -> StrictMaybe (Al.AuxiliaryDataHash (Ledger.Crypto era))
    adHash = getField @"adHash"

    auxiliary = do
        hash <- Shelley.encodeAuxiliaryDataHash <$> adHash (Al.body x)
        (labels, scripts) <- encodeAuxiliaryData <$> Al.auxiliaryData x
        pure
            ( encodeObject ("hash" .= hash <> "labels" .= labels)
            , scripts
            )

encodeTxBody
    :: Crypto crypto
    => Al.TxBody (AlonzoEra crypto)
    -> Series
encodeTxBody x =
    "inputs" .=
        encodeFoldable Shelley.encodeTxIn (Al.inputs x) <>
    "outputs" .=
        encodeFoldable encodeTxOut (Al.outputs x) <>
    "collaterals" .=? OmitWhen null
        (encodeFoldable Shelley.encodeTxIn) (Al.collateral x) <>
    "certificates" .=? OmitWhen null
        (encodeFoldable Shelley.encodeDCert) (Al.txcerts x) <>
    "withdrawals" .=? OmitWhen (null . Sh.unWdrl)
        Shelley.encodeWdrl (Al.txwdrls x) <>
    "mint" .=? OmitWhen isZero
        Mary.encodeValue (Al.mint x) <>
    "requiredExtraSignatories".=? OmitWhen null
        (encodeFoldable Shelley.encodeKeyHash) (Al.reqSignerHashes x) <>
    "network" .=? OmitWhenNothing
        Shelley.encodeNetwork (Al.txnetworkid x) <>
    "scriptIntegrityHash" .=? OmitWhenNothing
        encodeScriptIntegrityHash (Al.scriptIntegrityHash x) <>
    "fee" .=
        encodeCoin (Al.txfee x) <>
    "validityInterval" .=
        Allegra.encodeValidityInterval (Al.txvldt x) <>
    "governanceActions" .=? OmitWhenNothing
        (encodeFoldable identity . pure @[] . encodeUpdate)
        (Al.txUpdates x)

encodeTxOut
    :: Crypto crypto
    => Al.TxOut (AlonzoEra crypto)
    -> Json
encodeTxOut (Al.TxOut addr value datum) =
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
    :: forall era crypto.
        ( crypto ~ Ledger.Crypto era
        , Ledger.Era era
        , Rules.Shelley.PredicateFailure (Ledger.EraRule "UTXOS" era)
            ~ Rules.Alonzo.UtxosPredicateFailure era
        , Rules.Shelley.PredicateFailure (Ledger.EraRule "PPUP" era)
            ~ Rules.Shelley.PpupPredicateFailure era
        , Ledger.Value era ~ Ledger.Mary.Value crypto
        )
    => (Sh.UTxO era -> Json)
    -> (Ledger.TxOut era -> Json)
    -> (Ledger.TxOut era -> Sh.Addr crypto)
    -> Rules.Alonzo.UtxoPredicateFailure era
    -> Json
encodeUtxoFailure encodeUtxoInEra encodeTxOutInEra extractAddress = \case
    Rules.Alonzo.BadInputsUTxO inputs ->
        "badInputs" .=
            encodeFoldable Shelley.encodeTxIn inputs
        & encodeObject
    Rules.Alonzo.OutsideValidityIntervalUTxO itv currentSlot ->
        "outsideOfValidityInterval" .= encodeObject
            ( "interval" .=
                Allegra.encodeValidityInterval itv <>
              "currentSlot" .=
                encodeSlotNo currentSlot
            )
        & encodeObject
    Rules.Alonzo.MaxTxSizeUTxO actualSize maxSize ->
        "txTooLarge" .= encodeObject
            ( "maximumSize" .=
                encodeInteger maxSize <>
              "actualSize" .=
                encodeInteger actualSize
            )
        & encodeObject
    Rules.Alonzo.InputSetEmptyUTxO ->
        "missingAtLeastOneInputUtxo" .=
            encodeNull
        & encodeObject
    Rules.Alonzo.FeeTooSmallUTxO required actual ->
        "feeTooSmall" .= encodeObject
            ( "requiredFee" .=
                encodeCoin required <>
              "actualFee" .=
                encodeCoin actual
            )
        & encodeObject
    Rules.Alonzo.ValueNotConservedUTxO consumed produced ->
        "valueNotConserved" .= encodeObject
            ( "consumed" .=
                Mary.encodeValue consumed <>
              "produced" .=
                Mary.encodeValue produced
            )
        & encodeObject
    Rules.Alonzo.WrongNetwork expected invalidAddrs ->
        "networkMismatch" .= encodeObject
            ( "expectedNetwork" .=
                Shelley.encodeNetwork expected <>
              "invalidEntities" .=
                Shelley.encodeEntities "address" Shelley.encodeAddress invalidAddrs
            )
        & encodeObject
    Rules.Alonzo.WrongNetworkWithdrawal expected invalidAccts ->
        "networkMismatch" .= encodeObject
            ( "expectedNetwork" .=
                Shelley.encodeNetwork expected <>
              "invalidEntities" .=
                Shelley.encodeEntities "rewardAccount" Shelley.encodeRewardAcnt invalidAccts
            )
        & encodeObject
    Rules.Alonzo.WrongNetworkInTxBody expected actual ->
        "networkMismatch" .= encodeObject
            ( "expectedNetwork" .=
                Shelley.encodeNetwork expected<>
              "invalidEntities" .=
                Shelley.encodeEntities "transactionBody" Shelley.encodeNetwork [actual]
            )
        & encodeObject
    Rules.Alonzo.OutputTooSmallUTxO outs ->
        "outputTooSmall" .=
            encodeFoldable encodeTxOutInEra outs
        & encodeObject
    Rules.Alonzo.OutputBootAddrAttrsTooBig outs ->
        "addressAttributesTooLarge" .=
            encodeFoldable Shelley.encodeAddress (extractAddress <$> outs)
        & encodeObject
    Rules.Alonzo.TriesToForgeADA ->
        "triesToForgeAda" .=
            encodeNull
        & encodeObject
    Rules.Alonzo.OutputTooBigUTxO outs ->
        "tooManyAssetsInOutput" .=
            encodeFoldable (\(_, _, o) -> encodeTxOutInEra o) outs
        & encodeObject
    Rules.Alonzo.NoCollateralInputs ->
        "missingCollateralInputs" .=
            encodeNull
        & encodeObject
    Rules.Alonzo.InsufficientCollateral actual required ->
        "collateralTooSmall" .= encodeObject
            ( "requiredCollateral" .=
                encodeCoin required <>
              "actualCollateral" .=
                encodeCoin actual
            )
        & encodeObject
    Rules.Alonzo.ScriptsNotPaidUTxO utxo ->
        "collateralIsScript" .=
            encodeUtxoInEra utxo
        & encodeObject
    Rules.Alonzo.CollateralContainsNonADA value ->
        "collateralHasNonAdaAssets" .=
            Mary.encodeValue value
        & encodeObject
    Rules.Alonzo.TooManyCollateralInputs maxInputs actualInputs ->
        "tooManyCollateralInputs" .= encodeObject
            ( "maximumCollateralInputs" .=
                encodeNatural maxInputs <>
              "actualCollateralInputs" .=
                encodeNatural actualInputs
            )
        & encodeObject
    Rules.Alonzo.ExUnitsTooBigUTxO maxUnit actualUnit ->
        "executionUnitsTooLarge" .= encodeObject
            ( "maximumExecutionUnits" .=
                encodeExUnits maxUnit <>
              "actualExecutionUnits" .=
                encodeExUnits actualUnit
            )
        & encodeObject
    Rules.Alonzo.OutsideForecast slot ->
        "outsideForecast" .=
            encodeSlotNo slot
        & encodeObject
    Rules.Alonzo.UtxosFailure e ->
        encodeUtxosPredicateFailure e

encodeUtxosPredicateFailure
    :: forall era.
        ( Sh.PredicateFailure (Ledger.EraRule "PPUP" era) ~ Rules.Shelley.PpupPredicateFailure era
        , Ledger.Era era
        )
    => Rules.Alonzo.UtxosPredicateFailure era
    -> Json
encodeUtxosPredicateFailure = \case
    Rules.Alonzo.ValidationTagMismatch{} ->
        "validationTagMismatch" .=
            encodeNull
        & encodeObject
    Rules.Alonzo.CollectErrors errors ->
        "collectErrors" .=
            encodeFoldable encodeCollectError errors
        & encodeObject
    Rules.Alonzo.UpdateFailure e ->
        Shelley.encodeUpdateFailure e

encodeScriptIntegrityHash
    :: Crypto crypto
    => Al.ScriptIntegrityHash crypto
    -> Json
encodeScriptIntegrityHash =
    Shelley.encodeHash . Ledger.extractHash

encodeScriptFailure
    :: Crypto crypto
    => Ledger.Tools.TransactionScriptFailure crypto
    -> Json
encodeScriptFailure = encodeObject . \case
    -- NOTE: This 'RedeemerNotNeeded' error is likely redundant and misleading
    -- in the ledger code. It is raised when the script's language pointed by
    -- the redeemer is unknown.
    Ledger.Tools.RedeemerNotNeeded ptr _ ->
        "extraRedeemers" .=
            encodeFoldable (encodeText . stringifyRdmrPtr) [ptr]
    Ledger.Tools.RedeemerPointsToUnknownScriptHash ptr ->
        "extraRedeemers" .=
            encodeFoldable (encodeText . stringifyRdmrPtr) [ptr]
    Ledger.Tools.MissingScript ptr resolved ->
        "missingRequiredScripts" .= encodeObject
            ( "missing" .=
                encodeFoldable (encodeText . stringifyRdmrPtr) [ptr] <>
              "resolved" .=
                encodeMap
                    stringifyRdmrPtr
                    (\(_, _, h) -> Shelley.encodeScriptHash h)
                    resolved
            )
    Ledger.Tools.MissingDatum h ->
        "missingRequiredDatums" .= encodeObject
            ( "missing" .=
                encodeFoldable encodeDataHash [h]
            )
    Ledger.Tools.ValidationFailedV1 err traces ->
        "validatorFailed" .= encodeObject
            ( "error" .=
                encodeText (show (pretty err)) <>
              "traces" .=
                encodeFoldable encodeText traces
            )
    Ledger.Tools.ValidationFailedV2 err traces ->
        "validatorFailed" .= encodeObject
            ( "error" .=
                encodeText (show (pretty err)) <>
              "traces" .=
                encodeFoldable encodeText traces
            )
    Ledger.Tools.UnknownTxIn i ->
        "unknownInputReferencedByRedeemer" .=
            Shelley.encodeTxIn i
    Ledger.Tools.InvalidTxIn i ->
        "nonScriptInputReferencedByRedeemer" .=
            Shelley.encodeTxIn i
    Ledger.Tools.IncompatibleBudget budget ->
        "illFormedExecutionBudget" .=? OmitWhenNothing
            encodeExUnits (maybeToStrictMaybe (Al.exBudgetToExUnits budget))
    Ledger.Tools.NoCostModelInLedgerState lang ->
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
    :: (Ledger.Era era, Ledger.Script era ~ Al.Script era)
    => StrictMaybe (AuxiliaryScripts era)
    -> Al.TxWitness era
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
