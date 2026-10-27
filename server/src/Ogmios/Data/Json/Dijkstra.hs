--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Dijkstra (
    encodeBlock,
    encodeNativeScript,
    encodeScript,
    encodeTx,
    encodeTxOut,
    encodeUtxo,
    encodeDelegCert,
    encodeTxCert,
    encodeScriptPurposeItem,
    encodeScriptPurposeIndex,
    encodeGovAction,
    encodeContextError,
    encodePParams,
    encodePParamsUpdate,
    encodePParamsHKD,
) where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Alonzo.Plutus.Context (
    ContextError,
 )
import Cardano.Ledger.Api (
    AsItem (..),
    AsIx (..),
    PlutusPurpose,
 )
import Cardano.Ledger.MemoBytes (
    getMemoRawType,
 )
import Cardano.Ledger.Plutus.TxInfo (
    TxOutSource (..),
 )

import Cardano.Ledger.BaseTypes (
    BoundedRational (..),
    NonNegativeInterval,
    ProtVer,
 )
import Cardano.Ledger.Compactible (
    fromCompact,
 )
import Cardano.Ledger.Conway.PParams (
    THKD (..),
 )
import Cardano.Ledger.HKD (
    HKDFunctor (..),
 )

import qualified Cardano.Ledger.HKD as Ledger

import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Al
import qualified Cardano.Ledger.Alonzo.Scripts as Al
import qualified Cardano.Ledger.Babbage.Core as Ba
import qualified Cardano.Ledger.Babbage.TxBody as Ba
import qualified Cardano.Ledger.Babbage.TxInfo as Ba
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Conway.Governance as Cn
import qualified Cardano.Ledger.Conway.TxInfo as Cn
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Dijkstra.PParams as Di
import qualified Cardano.Ledger.Dijkstra.Scripts as Di
import qualified Cardano.Ledger.Dijkstra.Tx as Di
import qualified Cardano.Ledger.Dijkstra.TxCert as Di
import qualified Cardano.Ledger.Dijkstra.TxInfo as Di
import qualified Cardano.Ledger.Mary.Value as Ma
import qualified Cardano.Ledger.Plutus.Data as Ledger
import qualified Cardano.Ledger.Shelley.API as Sh

import qualified Data.Map.Strict as Map

import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Babbage as Babbage
import qualified Ogmios.Data.Json.Conway as Conway
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

import Ouroboros.Consensus.Shelley.Ledger.Block (
    ShelleyBlock (..),
 )

encodeBlock ::
    (MetadataFormat, IncludeCbor) ->
    ShelleyBlock (Praos StandardCrypto) DijkstraEra ->
    Json
encodeBlock opts (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "type"
            .= encodeText "praos"
            <> "era"
            .= encodeText "dijkstra"
            <> "id"
            .= Shelley.encodeShelleyHash headerHash
            <> Babbage.encodeHeader blkHeader
            <> "transactions"
            .= encodeFoldable (encodeTx opts . Di.unDijkstraTx) (txs ^. Ledger.txSeqBlockBodyL)
        )

-- TODO(dijkstra): minimal body — emits txId, isValid, and (optionally)
-- raw CBOR. Body / witness / metadata destructuring against the new
-- DijkstraTxBody surface is a follow-up.
encodeTx ::
    (MetadataFormat, IncludeCbor) ->
    Di.DijkstraTx Ledger.TopTx DijkstraEra ->
    Json
encodeTx (_fmt, opts) tx@Di.DijkstraTx{Di.dtBody, Di.dtIsPhase2Valid} =
    encodeObject
        ( Shelley.encodeTxId (Ledger.txIdTxBody @DijkstraEra dtBody)
            <> "spends"
            .= Alonzo.encodeIsValid dtIsPhase2Valid
            <> if includeTransactionCbor opts
                then
                    "cbor" .= encodeByteStringBase16 (encodeCbor @DijkstraEra tx)
                else
                    mempty
        )

encodeNativeScript ::
    forall era.
    (Ledger.NativeScript era ~ Di.DijkstraNativeScript era) =>
    Di.DijkstraNativeScript era ->
    Json
encodeNativeScript = encodeObject . go . getMemoRawType
  where
    go = \case
        Di.DijkstraRequireSignature sig ->
            "clause"
                .= encodeText "signature"
                <> "from"
                .= Shelley.encodeKeyHash sig
        Di.DijkstraRequireAllOf xs ->
            "clause"
                .= encodeText "all"
                <> "from"
                .= encodeFoldable encodeNativeScript xs
        Di.DijkstraRequireAnyOf xs ->
            "clause"
                .= encodeText "any"
                <> "from"
                .= encodeFoldable encodeNativeScript xs
        Di.DijkstraRequireMOf n xs ->
            "clause"
                .= encodeText "some"
                <> "atLeast"
                .= encodeInteger (toInteger n)
                <> "from"
                .= encodeFoldable encodeNativeScript xs
        Di.DijkstraTimeStart s ->
            "clause"
                .= encodeText "after"
                <> "slot"
                .= encodeSlotNo s
        Di.DijkstraTimeExpire s ->
            "clause"
                .= encodeText "before"
                <> "slot"
                .= encodeSlotNo s
        Di.DijkstraRequireGuard cred ->
            "clause"
                .= encodeText "guard"
                <> "from"
                .= Shelley.encodeCredentialRaw cred

encodeScript ::
    forall era.
    ( Al.AlonzoEraScript era
    , Ledger.NativeScript era ~ Di.DijkstraNativeScript era
    ) =>
    IncludeCbor ->
    Al.AlonzoScript era ->
    Json
encodeScript opts =
    encodeObject . \case
        Al.NativeScript nativeScript ->
            "language"
                .= encodeText "native"
                <> "json"
                .= encodeNativeScript nativeScript
                <> if includeScriptCbor opts
                    then
                        "cbor"
                            .= encodeByteStringBase16 (Ledger.originalBytes nativeScript)
                    else
                        mempty
        Al.PlutusScript script ->
            "language"
                .= encodeText (Alonzo.stringifyLanguage (Al.plutusScriptLanguage script))
                <> "cbor"
                .= encodeByteStringBase16 (Ledger.originalBytes (Al.plutusScriptBinary script))

encodeTxOut ::
    forall era.
    ( Ledger.Script era ~ Al.AlonzoScript era
    , Ledger.Value era ~ Ma.MaryValue
    , Al.AlonzoEraScript era
    , Ledger.NativeScript era ~ Di.DijkstraNativeScript era
    ) =>
    IncludeCbor ->
    Ba.BabbageTxOut era ->
    Series
encodeTxOut opts (Ba.BabbageTxOut addr value datum script) =
    "address"
        .= Shelley.encodeAddress addr
        <> "value"
        .= Mary.encodeValue value
        <> ( case datum of
                Ledger.NoDatum ->
                    mempty
                Ledger.DatumHash h ->
                    "datumHash" .= Alonzo.encodeDataHash h
                Ledger.Datum bin ->
                    "datum" .= Alonzo.encodeBinaryData bin
           )
        <> "script"
        .=? OmitWhenNothing
            (encodeScript opts)
            script

encodeUtxo ::
    forall era.
    ( Di.DijkstraEraScript era
    , Ledger.Script era ~ Al.AlonzoScript era
    , Ledger.Value era ~ Ma.MaryValue
    , Ledger.TxOut era ~ Ba.BabbageTxOut era
    , Ledger.NativeScript era ~ Di.DijkstraNativeScript era
    ) =>
    Sh.UTxO era ->
    Json
encodeUtxo =
    encodeList id
        . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) []
        . Sh.unUTxO
  where
    encodeIO i o = encodeObject (Shelley.encodeTxIn i <> encodeTxOut includeAllCbor o)

encodeDelegCert ::
    Di.DijkstraDelegCert ->
    NonEmpty Series
encodeDelegCert = \case
    Di.DijkstraRegCert credential deposit ->
        ( "type"
            .= encodeText "stakeCredentialRegistration"
            <> "credential"
            `Shelley.encodeCredential` credential
            <> "deposit"
            .= encodeCoin deposit
        )
            :| []
    Di.DijkstraUnRegCert credential deposit ->
        ( "type"
            .= encodeText "stakeCredentialDeregistration"
            <> "credential"
            `Shelley.encodeCredential` credential
            <> "deposit"
            .= encodeCoin deposit
        )
            :| []
    Di.DijkstraDelegCert credential delegatee ->
        ( "type"
            .= encodeText "stakeDelegation"
            <> "credential"
            `Shelley.encodeCredential` credential
            <> Conway.encodeDelegatee delegatee
        )
            :| []
    Di.DijkstraRegDelegCert credential delegatee deposit ->
        ( "type"
            .= encodeText "stakeCredentialRegistration"
            <> "credential"
            `Shelley.encodeCredential` credential
            <> "deposit"
            .= encodeCoin deposit
        )
            :| [ "type"
                    .= encodeText "stakeDelegation"
                    <> "credential"
                    `Shelley.encodeCredential` credential
                    <> Conway.encodeDelegatee delegatee
               ]

encodeTxCert ::
    Di.DijkstraTxCert era ->
    NonEmpty Series
encodeTxCert = \case
    Di.DijkstraTxCertDeleg dCert ->
        encodeDelegCert dCert
    Di.DijkstraTxCertPool pCert ->
        Shelley.encodePoolCert pCert :| []
    Di.DijkstraTxCertGov cCert ->
        Conway.encodeConwayGovCert cCert :| []

encodeScriptPurposeItem ::
    forall era.
    ( Ledger.TxCert era ~ Di.DijkstraTxCert era
    , Ledger.PParamsHKD StrictMaybe era ~ Di.DijkstraPParams StrictMaybe era
    ) =>
    Di.DijkstraPlutusPurpose AsItem era ->
    Json
encodeScriptPurposeItem =
    encodeObject . \case
        Di.DijkstraSpending (AsItem txIn) ->
            "purpose"
                .= encodeText "spend"
                <> "outputReference"
                .= encodeObject (Shelley.encodeTxIn txIn)
        Di.DijkstraMinting (AsItem policyId) ->
            "purpose"
                .= encodeText "mint"
                <> "policy"
                .= Mary.encodePolicyId policyId
        Di.DijkstraWithdrawing (AsItem acct) ->
            "purpose"
                .= encodeText "withdraw"
                <> "rewardAccount"
                .= Shelley.encodeRewardAcnt acct
        Di.DijkstraVoting (AsItem voter) ->
            "purpose"
                .= encodeText "vote"
                <> "issuer"
                .= Conway.encodeVoter voter
        Di.DijkstraProposing (AsItem proposal) ->
            "purpose"
                .= encodeText "propose"
                <> "proposal"
                .= encodeObject
                    ( "deposit"
                        .= encodeCoin (Cn.pProcDeposit proposal)
                        <> "returnAccount"
                        .= Shelley.encodeRewardAcnt (Cn.pProcReturnAddr proposal)
                        <> "metadata"
                        .= Conway.encodeAnchor (Cn.pProcAnchor proposal)
                        <> "action"
                        .= encodeGovAction (Cn.pProcGovAction proposal)
                    )
        Di.DijkstraCertifying (AsItem cert) ->
            case encodeTxCert cert of
                -- Delegation certificate or credential de-registration certificate
                c :| [] ->
                    "purpose"
                        .= encodeText "publish"
                        <> "certificate"
                        .= encodeObject c
                -- Combination of credential registration & delegation certificate.
                --
                -- Registering credentials is not _scriptable_, so never associated
                -- with a purpose.
                _ :| (c : _) ->
                    "purpose"
                        .= encodeText "publish"
                        <> "certificate"
                        .= encodeObject c
        Di.DijkstraGuarding (AsItem scriptHash) ->
            "purpose"
                .= encodeText "guard"
                <> "script"
                .= Shelley.encodeScriptHash scriptHash

encodeScriptPurposeIndex ::
    forall era.
    () =>
    Di.DijkstraPlutusPurpose AsIx era ->
    Json
encodeScriptPurposeIndex = \case
    Di.DijkstraSpending ix ->
        translate (Al.AlonzoSpending ix)
    Di.DijkstraMinting ix ->
        translate (Al.AlonzoMinting ix)
    Di.DijkstraCertifying (AsIx (AsIx -> ix)) ->
        translate (Al.AlonzoCertifying ix)
    Di.DijkstraWithdrawing ix ->
        translate (Al.AlonzoWithdrawing ix)
    Di.DijkstraVoting (AsIx ix) ->
        encodeObject
            ( "index"
                .= encodeWord32 ix
                <> "purpose"
                .= encodeText "vote"
            )
    Di.DijkstraProposing (AsIx ix) ->
        encodeObject
            ( "index"
                .= encodeWord32 ix
                <> "purpose"
                .= encodeText "propose"
            )
    Di.DijkstraGuarding (AsIx ix) ->
        encodeObject
            ( "index"
                .= encodeWord32 ix
                <> "purpose"
                .= encodeText "guard"
            )
  where
    translate = Alonzo.encodeScriptPurposeIndex @AlonzoEra

encodePParams ::
    (Ledger.PParamsHKD Identity era ~ Di.DijkstraPParams Identity era) =>
    Ledger.PParams era ->
    Json
encodePParams (Ledger.PParams x) =
    encodePParamsHKD (\k encode v -> k .= encode v) identity x

encodePParamsUpdate ::
    (Ledger.PParamsHKD StrictMaybe era ~ Di.DijkstraPParams StrictMaybe era) =>
    Ledger.PParamsUpdate era ->
    Json
encodePParamsUpdate (Ledger.PParamsUpdate x) =
    encodePParamsHKD (\k encode v -> k .=? OmitWhenNothing encode v) (const SNothing) x

encodePParamsHKD ::
    forall f era.
    (HKDFunctor f) =>
    (forall a. Text -> (a -> Json) -> Ledger.HKD f a -> Series) ->
    (forall a. a -> Ledger.HKD f a) ->
    Di.DijkstraPParams f era ->
    Json
encodePParamsHKD encode pure_ x =
    encode
        "minFeeCoefficient"
        (encodeInteger . unCoin . fromCompact . Ba.unCoinPerByte)
        (unTHKD (Di.dppTxFeePerByte x))
        <> encode
            "minFeeConstant"
            (encodeCoin . fromCompact)
            (unTHKD (Di.dppTxFeeFixed x))
        <> encode
            "minFeeReferenceScripts"
            ( \(base :: NonNegativeInterval) ->
                encodeObject
                    ( "base"
                        .= encodeDouble (fromRational (unboundRational base))
                        <> "range"
                        .= encodeWord32 25600
                        <> "multiplier"
                        .= encodeDouble 1.2
                    )
            )
            (unTHKD (Di.dppMinFeeRefScriptCostPerByte x))
        <> encode
            "maxBlockBodySize"
            (encodeSingleton "bytes" . encodeWord32)
            (unTHKD (Di.dppMaxBBSize x))
        <> encode
            "maxBlockHeaderSize"
            (encodeSingleton "bytes" . encodeWord16)
            (unTHKD (Di.dppMaxBHSize x))
        <> encode
            "maxTransactionSize"
            (encodeSingleton "bytes" . encodeWord32)
            (unTHKD (Di.dppMaxTxSize x))
        <> encode
            "stakeCredentialDeposit"
            (encodeCoin . fromCompact)
            (unTHKD (Di.dppKeyDeposit x))
        <> encode
            "stakePoolDeposit"
            (encodeCoin . fromCompact)
            (unTHKD (Di.dppPoolDeposit x))
        <> encode
            "stakePoolRetirementEpochBound"
            encodeEpochInterval
            (unTHKD (Di.dppEMax x))
        <> encode
            "desiredNumberOfStakePools"
            encodeWord16
            (unTHKD (Di.dppNOpt x))
        <> encode
            "stakePoolPledgeInfluence"
            encodeNonNegativeInterval
            (unTHKD (Di.dppA0 x))
        <> encode
            "monetaryExpansion"
            encodeUnitInterval
            (unTHKD (Di.dppRho x))
        <> encode
            "treasuryExpansion"
            encodeUnitInterval
            (unTHKD (Di.dppTau x))
        <> encode
            "minStakePoolCost"
            (encodeCoin . fromCompact)
            (unTHKD (Di.dppMinPoolCost x))
        <> encode
            "minUtxoDepositConstant"
            encodeCoin
            (pure_ (Coin 0))
        <> encode
            "minUtxoDepositCoefficient"
            (encodeInteger . unCoin . fromCompact . Ba.unCoinPerByte)
            (unTHKD (Di.dppCoinsPerUTxOByte x))
        <> encode
            "plutusCostModels"
            Alonzo.encodeCostModels
            (unTHKD (Di.dppCostModels x))
        <> encode
            "scriptExecutionPrices"
            Alonzo.encodePrices
            (unTHKD (Di.dppPrices x))
        <> encode
            "maxExecutionUnitsPerTransaction"
            (Alonzo.encodeExUnits . Al.unOrdExUnits)
            (unTHKD (Di.dppMaxTxExUnits x))
        <> encode
            "maxExecutionUnitsPerBlock"
            (Alonzo.encodeExUnits . Al.unOrdExUnits)
            (unTHKD (Di.dppMaxBlockExUnits x))
        <> encode
            "maxValueSize"
            (encodeSingleton "bytes" . encodeWord32)
            (unTHKD (Di.dppMaxValSize x))
        <> encode
            "collateralPercentage"
            encodeWord16
            (unTHKD (Di.dppCollateralPercentage x))
        <> encode
            "maxCollateralInputs"
            encodeWord16
            (unTHKD (Di.dppMaxCollateralInputs x))
        <> encode
            "version"
            Shelley.encodeProtVer
            (fromNoUpdate @f @ProtVer (Di.dppProtocolVersion x))
        <> encode
            "stakePoolVotingThresholds"
            Conway.encodePoolVotingThresholds
            (unTHKD (Di.dppPoolVotingThresholds x))
        <> encode
            "delegateRepresentativeVotingThresholds"
            Conway.encodeDRepVotingThresholds
            (unTHKD (Di.dppDRepVotingThresholds x))
        <> encode
            "constitutionalCommitteeMinSize"
            encodeWord16
            (unTHKD (Di.dppCommitteeMinSize x))
        <> encode
            "constitutionalCommitteeMaxTermLength"
            encodeEpochInterval
            (unTHKD (Di.dppCommitteeMaxTermLength x))
        <> encode
            "governanceActionLifetime"
            encodeEpochInterval
            (unTHKD (Di.dppGovActionLifetime x))
        <> encode
            "governanceActionDeposit"
            (encodeCoin . fromCompact)
            (unTHKD (Di.dppGovActionDeposit x))
        <> encode
            "delegateRepresentativeDeposit"
            (encodeCoin . fromCompact)
            (unTHKD (Di.dppDRepDeposit x))
        <> encode
            "delegateRepresentativeMaxIdleTime"
            encodeEpochInterval
            (unTHKD (Di.dppDRepActivity x))
        <> encode
            "maxReferenceScriptsSizePerBlock"
            (encodeSingleton "bytes" . encodeWord32)
            (unTHKD (Di.dppMaxRefScriptSizePerBlock x))
        <> encode
            "maxReferenceScriptsSizePerTransaction"
            (encodeSingleton "bytes" . encodeWord32)
            (unTHKD (Di.dppMaxRefScriptSizePerTx x))
        & encodeObject

encodeGovAction ::
    forall era.
    (Ledger.PParamsHKD StrictMaybe era ~ Di.DijkstraPParams StrictMaybe era) =>
    Cn.GovAction era ->
    Json
encodeGovAction = \case
    Cn.ParameterChange ancestor pparamsUpdate guardrails ->
        encodeObject
            ( "type"
                .= encodeText "protocolParametersUpdate"
                <> "ancestor"
                .=? OmitWhenNothing
                    (Conway.encodeGovActionId . Cn.unGovPurposeId)
                    ancestor
                <> "parameters"
                .= encodePParamsUpdate pparamsUpdate
                <> "guardrails"
                .= encodeStrictMaybe (\s -> encodeObject ("hash" .= Shelley.encodeScriptHash s)) guardrails
            )
    Cn.HardForkInitiation ancestor version ->
        encodeObject
            ( "type"
                .= encodeText "hardForkInitiation"
                <> "ancestor"
                .=? OmitWhenNothing
                    (Conway.encodeGovActionId . Cn.unGovPurposeId)
                    ancestor
                <> "version"
                .= Shelley.encodeProtVer version
            )
    Cn.TreasuryWithdrawals withdrawals guardrails ->
        encodeObject
            ( "type"
                .= encodeText "treasuryWithdrawals"
                <> "withdrawals"
                .= encodeMap Shelley.stringifyRewardAcnt encodeCoin withdrawals
                <> "guardrails"
                .= encodeStrictMaybe (\s -> encodeObject ("hash" .= Shelley.encodeScriptHash s)) guardrails
            )
    Cn.UpdateCommittee ancestor removed added quorum ->
        encodeObject
            ( "type"
                .= encodeText "constitutionalCommittee"
                <> "ancestor"
                .=? OmitWhenNothing
                    (Conway.encodeGovActionId . Cn.unGovPurposeId)
                    ancestor
                <> "members"
                .= encodeObject
                    ( "added"
                        .= encodeMapAsList (\k -> Conway.encodeConstitutionalCommitteeMember k . SJust) added
                        <> "removed"
                        .= encodeFoldable (`Conway.encodeConstitutionalCommitteeMember` SNothing) removed
                    )
                <> "quorum"
                .= encodeUnitInterval quorum
            )
    Cn.NewConstitution ancestor constitution ->
        encodeObject
            ( "type"
                .= encodeText "constitution"
                <> "ancestor"
                .=? OmitWhenNothing
                    (Conway.encodeGovActionId . Cn.unGovPurposeId)
                    ancestor
                <> Conway.encodeConstitution constitution
            )
    Cn.NoConfidence ancestor ->
        encodeObject
            ( "type"
                .= encodeText "noConfidence"
                <> "ancestor"
                .=? OmitWhenNothing
                    (Conway.encodeGovActionId . Cn.unGovPurposeId)
                    ancestor
            )
    Cn.InfoAction ->
        encodeObject
            ( "type"
                .= encodeText "information"
            )

encodeContextError ::
    ( PlutusPurpose AsIx era ~ Di.DijkstraPlutusPurpose AsIx era
    , ContextError era ~ Di.DijkstraContextError era
    , ToJSON (Cn.ConwayContextError era)
    ) =>
    Di.DijkstraContextError era ->
    Json
encodeContextError =
    encodeText . stringifyContextError

stringifyContextError ::
    ( PlutusPurpose AsIx era ~ Di.DijkstraPlutusPurpose AsIx era
    , ContextError era ~ Di.DijkstraContextError era
    , ToJSON (Cn.ConwayContextError era)
    ) =>
    Di.DijkstraContextError era ->
    Text
stringifyContextError err = case err of
    Di.PointerPresentInOutput{} ->
        "Found outputs to a (legacy) pointer address. Pointer addresses are no longer supported as of Dijkstra."
    Di.ConwayContextError (Cn.CurrentTreasuryFieldNotSupported{}) ->
        "Unsupported field in transaction: 'treasury'. Use plutus:v3 or higher, or remove this field."
    Di.ConwayContextError (Cn.VotingProceduresFieldNotSupported{}) ->
        "Unsupported field in transaction: 'votes'. Use plutus:v3 or higher, or remove this field."
    Di.ConwayContextError (Cn.ProposalProceduresFieldNotSupported{}) ->
        "Unsupported field in transaction: 'proposals'. Use plutus:v3 or higher, or remove this field."
    Di.ConwayContextError (Cn.TreasuryDonationFieldNotSupported{}) ->
        "Unsupported field in transaction: 'donation'. Use plutus:v3 or higher, or remove this field."
    Di.ConwayContextError (Cn.ReferenceInputsNotDisjointFromInputs{}) ->
        "Reference inputs must not overlap with regular spending inputs. Move duplicates out of one of the two sets."
    Di.ConwayContextError (Cn.BabbageContextError (Ba.ByronTxOutInContext TxOutFromInput{})) ->
        "Found inputs locked by a (legacy) Byron/Bootstrap address. Don't use those."
    Di.ConwayContextError (Cn.BabbageContextError (Ba.ByronTxOutInContext TxOutFromOutput{})) ->
        "Found outputs to a (legacy) Byron/Bootstrap address. Don't use those."
    Di.ConwayContextError (Cn.BabbageContextError (Ba.InlineDatumsNotSupported{})) ->
        "Inline datums not supported in plutus:v1. Use plutus:v2 or higher."
    Di.ConwayContextError (Cn.BabbageContextError (Ba.ReferenceScriptsNotSupported{})) ->
        "Reference scripts not supported in plutus:v1. Use plutus:v2 or higher."
    Di.ConwayContextError (Cn.BabbageContextError (Ba.ReferenceInputsNotSupported{})) ->
        "Reference inputs not supported in plutus:v1. Use plutus:v2 or higher."
    Di.ConwayContextError (Cn.BabbageContextError (Ba.RedeemerPointerPointsToNothing purpose)) ->
        let (title, ptr) = describePurpose purpose
         in "Couldn't find corresponding redeemer for " <> title <> " #" <> show ptr <> ". Verify your transaction's construction."
    Di.ConwayContextError (Cn.BabbageContextError (Ba.AlonzoContextError (Al.TimeTranslationPastHorizon e))) ->
        "Uncomputable slot arithmetic; transaction's validity bounds go beyond the foreseeable end of the current era: " <> e
    Di.ConwayContextError (Cn.BabbageContextError (Ba.AlonzoContextError (Al.TranslationLogicMissingInput i))) ->
        "Unknown transaction input (missing from UTxO set): " <> Shelley.stringifyTxIn i
    Di.ConwayContextError (Cn.BabbageContextError (Ba.AlonzoContextError (Al.CertificateNotSupported{}))) ->
        "A certificate in the transaction isn't supported in neither plutus:v1 nor plutus:v2. Use plutus:v3 or higher."
    Di.ConwayContextError (Cn.BabbageContextError (Ba.AlonzoContextError (Al.PlutusPurposeNotSupported{}))) ->
        "A script purpose in the transaction isn't supported in neither plutus:v1 nor plutus:v2. Use plutus:v3 or higher."
    Di.ConwayContextError (Cn.BabbageContextError (Ba.AlonzoContextError (Al.ImpossibleContextError e))) ->
        "Unexpected failure while building the script context: " <> e
    Di.ConwayContextError e ->
        Conway.stringifyUnsupportedContextError e
    Di.SubTxContextError txId e ->
        "Failed to build the script context of a sub-transaction (id: " <> Shelley.stringifyTxId txId <> "): " <> stringifyContextError e
    Di.UnsupportedScriptInSubTx lang txId ->
        "Unsupported script language " <> Alonzo.stringifyLanguage lang <> " in sub-transaction (id: " <> Shelley.stringifyTxId txId <> "). Use plutus:v4 or higher in sub-transactions."
    Di.DirectDepositsNotSupported{} ->
        "Direct deposits aren't supported in plutus:v1, plutus:v2 nor plutus:v3. Use plutus:v4 or higher, or remove them."
    Di.AccountBalanceIntervalsNotSupported{} ->
        "Account balance intervals aren't supported in plutus:v1, plutus:v2 nor plutus:v3. Use plutus:v4 or higher, or remove them."
    Di.GuardScriptHashesNotSupported{} ->
        "Script hashes in guards aren't supported in plutus:v1, plutus:v2 nor plutus:v3. Use plutus:v4 or higher."
    Di.RequiredTopLevelGuardsNotSupported{} ->
        "Required top-level guards aren't supported in plutus:v1, plutus:v2 nor plutus:v3. Use plutus:v4 or higher, or remove them."
    Di.ScriptHashNotFoundForPurpose purpose ->
        let (title, ptr) = describePurpose purpose
         in "Couldn't find the script executed for " <> title <> " #" <> show ptr <> ". Verify your transaction's construction."
  where
    describePurpose :: Di.DijkstraPlutusPurpose AsIx era -> (Text, Word32)
    describePurpose = \case
        Di.DijkstraSpending (AsIx ix) -> ("spending input", ix)
        Di.DijkstraMinting (AsIx ix) -> ("minting policy", ix)
        Di.DijkstraCertifying (AsIx ix) -> ("publishing certificate", ix)
        Di.DijkstraWithdrawing (AsIx ix) -> ("withdrawing from account", ix)
        Di.DijkstraVoting (AsIx ix) -> ("voting as voter", ix)
        Di.DijkstraProposing (AsIx ix) -> ("proposing governance proposal", ix)
        Di.DijkstraGuarding (AsIx ix) -> ("guarding script", ix)
