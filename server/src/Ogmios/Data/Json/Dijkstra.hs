--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- | Dijkstra is a new era following Conway (protocol version 12).
-- The ledger types share the same CBOR format as Conway's but are nominally
-- distinct (different data family instances). We bridge via CBOR serialization
-- for safe type conversion.
module Ogmios.Data.Json.Dijkstra where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Api
    ( AsItem (..)
    , AsIx (..)
    )
import Cardano.Ledger.BaseTypes
    ( BoundedRational (..)
    , NonNegativeInterval
    , PositiveInterval
    , ProtVer
    )
import Cardano.Ledger.Compactible
    ( fromCompact
    )
import Cardano.Ledger.Conway.PParams
    ( THKD (..)
    )
import Cardano.Ledger.HKD
    ( HKDFunctor (..)
    )
import Cardano.Ledger.MemoBytes
    ( getMemoRawType
    )
import Cardano.Ledger.Plutus
    ( TxOutSource (..)
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )
import Ouroboros.Consensus.Shelley.Protocol.Praos
    ()

import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Binary as Binary
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.HKD as Ledger

import qualified Cardano.Ledger.Alonzo.PParams as Al
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Al
import qualified Cardano.Ledger.Alonzo.Scripts as Al

import qualified Cardano.Ledger.Babbage.Core as Ba
import qualified Cardano.Ledger.Babbage.TxBody as Ba
import qualified Cardano.Ledger.Babbage.TxInfo as Ba

import qualified Cardano.Ledger.Conway.Governance as Cn
import qualified Cardano.Ledger.Conway.TxInfo as Cn

import qualified Cardano.Ledger.Dijkstra.PParams as Dp
import qualified Cardano.Ledger.Dijkstra.Scripts as Dp
import qualified Cardano.Ledger.Dijkstra.TxCert as Dp
import qualified Cardano.Ledger.Dijkstra.TxInfo as Dp

import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Babbage as Babbage
import qualified Ogmios.Data.Json.Conway as Conway
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

encodeBlock
    :: (MetadataFormat, IncludeCbor)
    -> ShelleyBlock (Praos StandardCrypto) DijkstraEra
    -> Json
encodeBlock opts (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "type" .= encodeText "praos"
        <>
          "era" .= encodeText "dijkstra"
        <>
          "id" .= Shelley.encodeShelleyHash headerHash
        <>
          Babbage.encodeHeader blkHeader
        <>
          "transactions" .= encodeFoldable (encodeTx opts) (txs ^. Ledger.txSeqBlockBodyL)
        )

-- | Encode a Dijkstra transaction by converting to Conway via CBOR roundtrip.
-- Dijkstra and Conway share the same CBOR wire format, so this is safe.
encodeTx
    :: (MetadataFormat, IncludeCbor)
    -> Ledger.Tx Ledger.TopTx DijkstraEra
    -> Json
encodeTx opts x =
    case dijkstraToConwayTx x of
        Left err -> error $ "encodeTx/Dijkstra: CBOR roundtrip failed: " <> show err
        Right conwayTx -> Conway.encodeTx opts conwayTx

dijkstraToConwayTx
    :: Ledger.Tx Ledger.TopTx DijkstraEra
    -> Either Binary.DecoderError (Ledger.Tx Ledger.TopTx ConwayEra)
dijkstraToConwayTx tx =
    let bytes = Binary.serialize (Ledger.eraProtVerHigh @DijkstraEra) tx
     in Binary.decodeFullAnnotator
            (Ledger.eraProtVerHigh @ConwayEra)
            "Tx"
            Binary.decCBOR
            bytes

encodeNativeScript
    :: Dp.DijkstraNativeScript DijkstraEra
    -> Json
encodeNativeScript ns = encodeObject (go (getMemoRawType ns))
  where
    go = \case
        Dp.DijkstraRequireSignature sig ->
            "clause" .= encodeText "signature" <>
            "from" .= Shelley.encodeKeyHash sig
        Dp.DijkstraRequireAllOf xs ->
            "clause" .= encodeText "all" <>
            "from" .= encodeFoldable encodeNativeScript xs
        Dp.DijkstraRequireAnyOf xs ->
            "clause" .= encodeText "any" <>
            "from" .= encodeFoldable encodeNativeScript xs
        Dp.DijkstraRequireMOf n xs ->
            "clause" .= encodeText "some" <>
            "atLeast" .= encodeInteger (toInteger n) <>
            "from" .= encodeFoldable encodeNativeScript xs
        Dp.DijkstraTimeStart s ->
            "clause" .= encodeText "after" <>
            "slot" .= encodeSlotNo s
        Dp.DijkstraTimeExpire s ->
            "clause" .= encodeText "before" <>
            "slot" .= encodeSlotNo s
        Dp.DijkstraRequireGuard cred ->
            "clause" .= encodeText "guard" <>
            "from" .= Shelley.encodeCredentialRaw cred

encodeScript
    :: IncludeCbor
    -> Al.AlonzoScript DijkstraEra
    -> Json
encodeScript opts = encodeObject . \case
    Al.NativeScript nativeScript ->
        "language" .=
            encodeText "native" <>
        "json" .=
            encodeNativeScript nativeScript <>
        if includeScriptCbor opts then
            "cbor" .=
                encodeByteStringBase16 (Ledger.originalBytes nativeScript)
        else
            mempty
    Al.PlutusScript script ->
        "language" .=
            encodeText (Alonzo.stringifyLanguage (Al.plutusScriptLanguage script)) <>
        "cbor" .=
            encodeByteStringBase16 (Ledger.originalBytes (Al.plutusScriptBinary script))

encodeTxOut
    :: IncludeCbor
    -> Ba.BabbageTxOut DijkstraEra
    -> Series
encodeTxOut opts (Ba.BabbageTxOut addr value datum script) =
    "address" .=
        Shelley.encodeAddress addr <>
    "value" .=
        Mary.encodeValue value <>
    ( case datum of
        Ledger.NoDatum ->
            mempty
        Ledger.DatumHash h ->
            "datumHash" .= Alonzo.encodeDataHash h
        Ledger.Datum bin ->
            "datum" .= Alonzo.encodeBinaryData bin
    ) <>
    "script" .=? OmitWhenNothing
        (encodeScript opts) script

encodeDelegCert
    :: Dp.DijkstraDelegCert
    -> NonEmpty Series
encodeDelegCert = \case
    Dp.DijkstraRegCert credential deposit ->
        ( "type" .=
            encodeText "stakeCredentialRegistration"
       <> "credential" `Shelley.encodeCredential` credential
       <> "deposit" .=
            encodeCoin deposit
        ) :| []
    Dp.DijkstraUnRegCert credential deposit ->
        ( "type" .=
            encodeText "stakeCredentialDeregistration"
        <> "credential" `Shelley.encodeCredential` credential
        <> "deposit" .=
            encodeCoin deposit
        ) :| []
    Dp.DijkstraDelegCert credential delegatee ->
        ( "type" .=
            encodeText "stakeDelegation"
        <> "credential" `Shelley.encodeCredential` credential
        <> Conway.encodeDelegatee delegatee
        ) :| []
    Dp.DijkstraRegDelegCert credential delegatee deposit ->
        ( "type" .=
            encodeText "stakeCredentialRegistration"
       <> "credential" `Shelley.encodeCredential` credential
       <> "deposit" .=
            encodeCoin deposit
        ) :|
        [ "type" .=
            encodeText "stakeDelegation"
       <> "credential" `Shelley.encodeCredential` credential
       <> Conway.encodeDelegatee delegatee
        ]

encodeTxCert
    :: Dp.DijkstraTxCert DijkstraEra
    -> NonEmpty Series
encodeTxCert = \case
    Dp.DijkstraTxCertDeleg dCert ->
        encodeDelegCert dCert
    Dp.DijkstraTxCertPool pCert ->
        Shelley.encodePoolCert pCert :| []
    Dp.DijkstraTxCertGov cCert ->
        Conway.encodeConwayGovCert cCert :| []

encodeScriptPurposeIndex
    :: Dp.DijkstraPlutusPurpose AsIx DijkstraEra
    -> Json
encodeScriptPurposeIndex = \case
    Dp.DijkstraSpending ix ->
        translate (Al.AlonzoSpending ix)
    Dp.DijkstraMinting ix ->
        translate (Al.AlonzoMinting ix)
    Dp.DijkstraCertifying (AsIx (AsIx -> ix)) ->
        translate (Al.AlonzoCertifying ix)
    Dp.DijkstraRewarding ix ->
        translate (Al.AlonzoRewarding ix)
    Dp.DijkstraVoting (AsIx ix) ->
        encodeObject
            ( "index" .=
                encodeWord32 ix
           <> "purpose" .=
                encodeText "vote"
            )
    Dp.DijkstraProposing (AsIx ix) ->
        encodeObject
            ( "index" .=
                encodeWord32 ix
           <> "purpose" .=
                encodeText "propose"
            )
    Dp.DijkstraGuarding (AsIx ix) ->
        encodeObject
            ( "index" .=
                encodeWord32 ix
           <> "purpose" .=
                encodeText "guard"
            )
  where
    translate = Alonzo.encodeScriptPurposeIndex @AlonzoEra

encodeScriptPurposeItem
    :: Dp.DijkstraPlutusPurpose AsItem DijkstraEra
    -> Json
encodeScriptPurposeItem = encodeObject . \case
    Dp.DijkstraSpending (AsItem txIn) ->
        "purpose" .= encodeText "spend" <>
        "outputReference" .= encodeObject (Shelley.encodeTxIn txIn)
    Dp.DijkstraMinting (AsItem policyId) ->
        "purpose" .= encodeText "mint" <>
        "policy" .= Mary.encodePolicyId policyId
    Dp.DijkstraRewarding (AsItem acct) ->
        "purpose" .= encodeText "withdraw" <>
        "rewardAccount" .= Shelley.encodeRewardAcnt acct
    Dp.DijkstraVoting (AsItem voter) ->
        "purpose" .= encodeText "vote" <>
        "issuer" .= Conway.encodeVoter voter
    Dp.DijkstraProposing (AsItem proposal) ->
        "purpose" .= encodeText "propose" <>
        "proposal" .= encodeObject (encodeProposalProcedure proposal)
    Dp.DijkstraCertifying (AsItem cert) ->
        case encodeTxCert cert of
            c :| [] ->
                "purpose" .= encodeText "publish" <>
                "certificate" .= encodeObject c
            _ :| (c : _) ->
                "purpose" .= encodeText "publish" <>
                "certificate" .= encodeObject c
    Dp.DijkstraGuarding (AsItem scriptHash) ->
        "purpose" .= encodeText "guard" <>
        "script" .= Shelley.encodeScriptHash scriptHash

encodeProposalProcedure
    :: Cn.ProposalProcedure DijkstraEra
    -> Series
encodeProposalProcedure x =
      "deposit" .=
        encodeCoin (Cn.pProcDeposit x)
   <> "returnAccount" .=
        Shelley.encodeRewardAcnt (Cn.pProcReturnAddr x)
   <> "metadata" .=
        Conway.encodeAnchor (Cn.pProcAnchor x)
   <> "action" .=
        encodeGovAction (Cn.pProcGovAction x)

encodeGovAction
    :: Cn.GovAction DijkstraEra
    -> Json
encodeGovAction = \case
    Cn.ParameterChange ancestor pparamsUpdate guardrails ->
        encodeObject
            ( "type" .=
                encodeText "protocolParametersUpdate"
           <> "ancestor" .=? OmitWhenNothing
                (Conway.encodeGovActionId . Cn.unGovPurposeId) ancestor
           <> "parameters" .=
                encodePParamsUpdate pparamsUpdate
           <> "guardrails" .=
                encodeStrictMaybe (\s -> encodeObject ("hash" .= Shelley.encodeScriptHash s)) guardrails
            )
    Cn.HardForkInitiation ancestor version ->
        encodeObject
            ( "type" .=
                encodeText "hardForkInitiation"
           <> "ancestor" .=? OmitWhenNothing
               (Conway.encodeGovActionId . Cn.unGovPurposeId) ancestor
           <> "version" .=
                Shelley.encodeProtVer version
            )
    Cn.TreasuryWithdrawals withdrawals guardrails ->
        encodeObject
            ( "type" .=
                encodeText "treasuryWithdrawals"
           <> "withdrawals" .=
                encodeMap Shelley.stringifyRewardAcnt encodeCoin withdrawals
           <> "guardrails" .=
                encodeStrictMaybe (\s -> encodeObject ("hash" .= Shelley.encodeScriptHash s)) guardrails
            )
    Cn.UpdateCommittee ancestor removed added quorum ->
        encodeObject
            ( "type" .=
                encodeText "constitutionalCommittee"
           <> "ancestor" .=? OmitWhenNothing
                (Conway.encodeGovActionId . Cn.unGovPurposeId) ancestor
           <> "members" .= encodeObject
                ( "added" .=
                    encodeMapAsList (\k -> Conway.encodeConstitutionalCommitteeMember k . SJust) added
               <> "removed" .=
                    encodeFoldable (`Conway.encodeConstitutionalCommitteeMember` SNothing) removed
                )
           <> "quorum" .=
                encodeUnitInterval quorum
            )
    Cn.NewConstitution ancestor constitution ->
        encodeObject
            ( "type" .=
                encodeText "constitution"
           <> "ancestor" .=? OmitWhenNothing
               (Conway.encodeGovActionId . Cn.unGovPurposeId) ancestor
           <> Conway.encodeConstitution constitution
            )
    Cn.NoConfidence ancestor ->
        encodeObject
            ( "type" .=
                encodeText "noConfidence"
           <> "ancestor" .=? OmitWhenNothing
                (Conway.encodeGovActionId . Cn.unGovPurposeId) ancestor
            )
    Cn.InfoAction ->
        encodeObject
            ( "type" .=
                encodeText "information"
            )

encodePParamsUpdate
    :: Ledger.PParamsUpdate DijkstraEra
    -> Json
encodePParamsUpdate (Ledger.PParamsUpdate x) =
    encodePParamsHKD (\k enc v -> k .=? OmitWhenNothing enc v) (const SNothing) x

encodePParamsHKD
    :: forall f era. (HKDFunctor f)
    => (forall a. Text -> (a -> Json) -> Ledger.HKD f a -> Series)
    -> (forall a. a -> Ledger.HKD f a)
    -> Dp.DijkstraPParams f era
    -> Json
encodePParamsHKD encode pure_ x =
    encode "minFeeCoefficient"
        (encodeInteger . unCoin . fromCompact . Ba.unCoinPerByte) (unTHKD (Dp.dppTxFeePerByte x)) <>
    encode "minFeeConstant"
        (encodeCoin . fromCompact) (unTHKD (Dp.dppTxFeeFixed x)) <>
    encode "minFeeReferenceScripts"
        (\(base :: NonNegativeInterval) -> encodeObject
            ( "base" .= encodeDouble (fromRational (unboundRational base))
            )
        )
        (unTHKD (Dp.dppMinFeeRefScriptCostPerByte x)) <>
    encode "refScriptCostStride"
        (encodeNonZero encodeWord32)
        (unTHKD (Dp.dppRefScriptCostStride x)) <>
    encode "refScriptCostMultiplier"
        (\(v :: PositiveInterval) -> encodeRational (unboundRational v))
        (unTHKD (Dp.dppRefScriptCostMultiplier x)) <>
    encode "maxBlockBodySize"
        (encodeSingleton "bytes" . encodeWord32) (unTHKD (Dp.dppMaxBBSize x)) <>
    encode "maxBlockHeaderSize"
        (encodeSingleton "bytes" . encodeWord16) (unTHKD (Dp.dppMaxBHSize x)) <>
    encode "maxTransactionSize"
        (encodeSingleton "bytes" . encodeWord32) (unTHKD (Dp.dppMaxTxSize x)) <>
    encode "maxReferenceScriptSizePerBlock"
        (encodeSingleton "bytes" . encodeWord32) (unTHKD (Dp.dppMaxRefScriptSizePerBlock x)) <>
    encode "maxReferenceScriptSizePerTx"
        (encodeSingleton "bytes" . encodeWord32) (unTHKD (Dp.dppMaxRefScriptSizePerTx x)) <>
    encode "stakeCredentialDeposit"
        (encodeCoin . fromCompact) (unTHKD (Dp.dppKeyDeposit x)) <>
    encode "stakePoolDeposit"
        (encodeCoin . fromCompact) (unTHKD (Dp.dppPoolDeposit x)) <>
    encode "stakePoolRetirementEpochBound"
        encodeEpochInterval (unTHKD (Dp.dppEMax x)) <>
    encode "desiredNumberOfStakePools"
        encodeWord16 (unTHKD (Dp.dppNOpt x)) <>
    encode "stakePoolPledgeInfluence"
        encodeNonNegativeInterval (unTHKD (Dp.dppA0 x)) <>
    encode "monetaryExpansion"
        encodeUnitInterval (unTHKD (Dp.dppRho x)) <>
    encode "treasuryExpansion"
        encodeUnitInterval (unTHKD (Dp.dppTau x)) <>
    encode "minStakePoolCost"
        (encodeCoin . fromCompact) (unTHKD (Dp.dppMinPoolCost x)) <>
    encode "minUtxoDepositConstant"
        encodeCoin (pure_ (Coin 0)) <>
    encode "minUtxoDepositCoefficient"
        (encodeInteger . unCoin . fromCompact . Ba.unCoinPerByte) (unTHKD (Dp.dppCoinsPerUTxOByte x)) <>
    encode "plutusCostModels"
        Alonzo.encodeCostModels (unTHKD (Dp.dppCostModels x)) <>
    encode "scriptExecutionPrices"
        Alonzo.encodePrices (unTHKD (Dp.dppPrices x)) <>
    encode "maxExecutionUnitsPerTransaction"
        (Alonzo.encodeExUnits . Al.unOrdExUnits) (unTHKD (Dp.dppMaxTxExUnits x)) <>
    encode "maxExecutionUnitsPerBlock"
        (Alonzo.encodeExUnits . Al.unOrdExUnits) (unTHKD (Dp.dppMaxBlockExUnits x)) <>
    encode "maxValueSize"
        (encodeSingleton "bytes" . encodeWord32) (unTHKD (Dp.dppMaxValSize x)) <>
    encode "collateralPercentage"
        encodeWord16 (unTHKD (Dp.dppCollateralPercentage x)) <>
    encode "maxCollateralInputs"
        encodeWord16 (unTHKD (Dp.dppMaxCollateralInputs x)) <>
    encode "version"
        Shelley.encodeProtVer (fromNoUpdate @f @ProtVer (Dp.dppProtocolVersion x)) <>
    encode "stakePoolVotingThresholds"
        Conway.encodePoolVotingThresholds (unTHKD (Dp.dppPoolVotingThresholds x)) <>
    encode "delegateRepresentativeVotingThresholds"
        Conway.encodeDRepVotingThresholds (unTHKD (Dp.dppDRepVotingThresholds x)) <>
    encode "constitutionalCommitteeMinSize"
        encodeWord16 (unTHKD (Dp.dppCommitteeMinSize x)) <>
    encode "constitutionalCommitteeMaxTermLength"
        encodeEpochInterval (unTHKD (Dp.dppCommitteeMaxTermLength x)) <>
    encode "governanceActionLifetime"
        encodeEpochInterval (unTHKD (Dp.dppGovActionLifetime x)) <>
    encode "governanceActionDeposit"
        (encodeCoin . fromCompact) (unTHKD (Dp.dppGovActionDeposit x)) <>
    encode "delegateRepresentativeDeposit"
        (encodeCoin . fromCompact) (unTHKD (Dp.dppDRepDeposit x)) <>
    encode "delegateRepresentativeMaxIdleTime"
        encodeEpochInterval (unTHKD (Dp.dppDRepActivity x))
    & encodeObject

encodeConwayContextError
    :: Cn.ConwayContextError DijkstraEra
    -> Json
encodeConwayContextError err = encodeText $ case err of
    Cn.CertificateNotSupported{} ->
        "A certificate in the transaction isn't supported in neither plutus:v1 nor plutus:v2. Use plutus:v3 or higher."
    Cn.PlutusPurposeNotSupported{} ->
        "A script purpose in the transaction isn't supported in neither plutus:v1 nor plutus:v2. Use plutus:v3 or higher."
    Cn.CurrentTreasuryFieldNotSupported{} ->
        "Unsupported field in transaction: 'treasury'. Use plutus:v3 or higher, or remove this field."
    Cn.VotingProceduresFieldNotSupported{} ->
        "Unsupported field in transaction: 'votes'. Use plutus:v3 or higher, or remove this field."
    Cn.ProposalProceduresFieldNotSupported{} ->
        "Unsupported field in transaction: 'proposals'. Use plutus:v3 or higher, or remove this field."
    Cn.TreasuryDonationFieldNotSupported{} ->
        "Unsupported field in transaction: 'donation'. Use plutus:v3 or higher, or remove this field."
    Cn.BabbageContextError (Ba.ByronTxOutInContext TxOutFromInput{}) ->
        "Found inputs locked by a (legacy) Byron/Bootstrap address. Don't use those."
    Cn.BabbageContextError (Ba.ByronTxOutInContext TxOutFromOutput{}) ->
        "Found outputs to a (legacy) Byron/Bootstrap address. Don't use those."
    Cn.BabbageContextError (Ba.InlineDatumsNotSupported{}) ->
       "Inline datums not supported in plutus:v1. Use plutus:v2 or higher."
    Cn.BabbageContextError (Ba.ReferenceScriptsNotSupported{}) ->
       "Reference scripts not supported in plutus:v1. Use plutus:v2 or higher."
    Cn.BabbageContextError (Ba.ReferenceInputsNotSupported{}) ->
       "Reference inputs not supported in plutus:v1. Use plutus:v2 or higher."
    Cn.BabbageContextError (Ba.RedeemerPointerPointsToNothing purpose) ->
        let (title, ptr) =
                case purpose of
                    Dp.DijkstraSpending (AsIx ix) -> ("spending input", ix)
                    Dp.DijkstraMinting (AsIx ix) -> ("minting policy", ix)
                    Dp.DijkstraCertifying (AsIx ix) -> ("publishing certificate", ix)
                    Dp.DijkstraRewarding (AsIx ix) -> ("withdrawing from account", ix)
                    Dp.DijkstraVoting (AsIx ix) -> ("voting as voter", ix)
                    Dp.DijkstraProposing (AsIx ix) -> ("proposing governance proposal", ix)
                    Dp.DijkstraGuarding (AsIx ix) -> ("guarding script", ix)
          in "Couldn't find corresponding redeemer for " <> title <> " #" <> show ptr <> ". Verify your transaction's construction."
    Cn.BabbageContextError (Ba.AlonzoContextError (Al.TimeTranslationPastHorizon e)) ->
        "Uncomputable slot arithmetic; transaction's validity bounds go beyond the foreseeable end of the current era: " <> e
    Cn.BabbageContextError (Ba.AlonzoContextError (Al.TranslationLogicMissingInput i)) ->
        "Unknown transaction input (missing from UTxO set): " <> Shelley.stringifyTxIn i
    Cn.ReferenceInputsNotDisjointFromInputs{} ->
        "Reference inputs overlap with regular inputs. They must be disjoint."

encodeContextError
    :: Dp.DijkstraContextError DijkstraEra
    -> Json
encodeContextError = \case
    Dp.ConwayContextError err ->
        encodeConwayContextError err
    Dp.PointerPresentInOutput{} ->
        encodeText "Found outputs with pointer addresses. Those are no longer supported."
