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
import qualified Cardano.Ledger.Core as Ledger.Core
import qualified Cardano.Ledger.HKD as Ledger

import qualified Cardano.Ledger.Alonzo.PParams as Al
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Al
import qualified Cardano.Ledger.Alonzo.Scripts as Al

import qualified Cardano.Ledger.Babbage.Core as Ba
import qualified Cardano.Ledger.Babbage.TxBody as Ba
import qualified Cardano.Ledger.Babbage.TxInfo as Ba

import qualified Cardano.Ledger.Conway.Governance as Cn
import qualified Cardano.Ledger.Conway.TxInfo as Cn

import qualified Cardano.Ledger.Dijkstra.Governance ()
import qualified Cardano.Ledger.Dijkstra.PParams as Di
import qualified Cardano.Ledger.Dijkstra.Scripts as Di
import qualified Cardano.Ledger.Dijkstra.TxCert as Di
import qualified Cardano.Ledger.Dijkstra.TxInfo as Di

import qualified Cardano.Ledger.Shelley.UTxO as Sh
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap

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
          "transactions" .= encodeFoldable (encodeTx opts) (txs ^. Ledger.Core.txSeqBlockBodyL)
        )

-- | Encode a Dijkstra transaction by converting to Conway via CBOR roundtrip.
-- Dijkstra and Conway share the same CBOR wire format, so this is safe.
encodeTx
    :: (MetadataFormat, IncludeCbor)
    -> Ledger.Core.Tx Ledger.Core.TopTx DijkstraEra
    -> Json
encodeTx opts x =
    case dijkstraToConwayTx x of
        Left err -> error $ "encodeTx/Dijkstra: CBOR roundtrip failed: " <> show err
        Right conwayTx -> Conway.encodeTx opts conwayTx

dijkstraToConwayTx
    :: Ledger.Core.Tx Ledger.Core.TopTx DijkstraEra
    -> Either Binary.DecoderError (Ledger.Core.Tx Ledger.Core.TopTx ConwayEra)
dijkstraToConwayTx tx =
    let bytes = Binary.serialize (Ledger.Core.eraProtVerHigh @DijkstraEra) tx
     in Binary.decodeFullAnnotator
            (Ledger.Core.eraProtVerHigh @ConwayEra)
            "Tx"
            Binary.decCBOR
            bytes

encodeNativeScript
    :: Di.DijkstraNativeScript DijkstraEra
    -> Json
encodeNativeScript ns = encodeObject (go (getMemoRawType ns))
  where
    go = \case
        Di.DijkstraRequireSignature sig ->
            "clause" .= encodeText "signature" <>
            "from" .= Shelley.encodeKeyHash sig
        Di.DijkstraRequireAllOf xs ->
            "clause" .= encodeText "all" <>
            "from" .= encodeFoldable encodeNativeScript xs
        Di.DijkstraRequireAnyOf xs ->
            "clause" .= encodeText "any" <>
            "from" .= encodeFoldable encodeNativeScript xs
        Di.DijkstraRequireMOf n xs ->
            "clause" .= encodeText "some" <>
            "atLeast" .= encodeInteger (toInteger n) <>
            "from" .= encodeFoldable encodeNativeScript xs
        Di.DijkstraTimeStart s ->
            "clause" .= encodeText "after" <>
            "slot" .= encodeSlotNo s
        Di.DijkstraTimeExpire s ->
            "clause" .= encodeText "before" <>
            "slot" .= encodeSlotNo s
        Di.DijkstraRequireGuard cred ->
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
                encodeByteStringBase16 (Ledger.Core.originalBytes nativeScript)
        else
            mempty
    Al.PlutusScript script ->
        "language" .=
            encodeText (Alonzo.stringifyLanguage (Al.plutusScriptLanguage script)) <>
        "cbor" .=
            encodeByteStringBase16 (Ledger.Core.originalBytes (Al.plutusScriptBinary script))

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
    :: Di.DijkstraDelegCert
    -> NonEmpty Series
encodeDelegCert = \case
    Di.DijkstraRegCert credential deposit ->
        ( "type" .=
            encodeText "stakeCredentialRegistration"
       <> "credential" `Shelley.encodeCredential` credential
       <> "deposit" .=
            encodeCoin deposit
        ) :| []
    Di.DijkstraUnRegCert credential deposit ->
        ( "type" .=
            encodeText "stakeCredentialDeregistration"
        <> "credential" `Shelley.encodeCredential` credential
        <> "deposit" .=
            encodeCoin deposit
        ) :| []
    Di.DijkstraDelegCert credential delegatee ->
        ( "type" .=
            encodeText "stakeDelegation"
        <> "credential" `Shelley.encodeCredential` credential
        <> Conway.encodeDelegatee delegatee
        ) :| []
    Di.DijkstraRegDelegCert credential delegatee deposit ->
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
    :: Di.DijkstraTxCert DijkstraEra
    -> NonEmpty Series
encodeTxCert = \case
    Di.DijkstraTxCertDeleg dCert ->
        encodeDelegCert dCert
    Di.DijkstraTxCertPool pCert ->
        Shelley.encodePoolCert pCert :| []
    Di.DijkstraTxCertGov cCert ->
        Conway.encodeConwayGovCert cCert :| []

encodeScriptPurposeIndex
    :: Di.DijkstraPlutusPurpose AsIx DijkstraEra
    -> Json
encodeScriptPurposeIndex = \case
    Di.DijkstraSpending ix ->
        translate (Al.AlonzoSpending ix)
    Di.DijkstraMinting ix ->
        translate (Al.AlonzoMinting ix)
    Di.DijkstraCertifying (AsIx (AsIx -> ix)) ->
        translate (Al.AlonzoCertifying ix)
    Di.DijkstraRewarding ix ->
        translate (Al.AlonzoRewarding ix)
    Di.DijkstraVoting (AsIx ix) ->
        encodeObject
            ( "index" .=
                encodeWord32 ix
           <> "purpose" .=
                encodeText "vote"
            )
    Di.DijkstraProposing (AsIx ix) ->
        encodeObject
            ( "index" .=
                encodeWord32 ix
           <> "purpose" .=
                encodeText "propose"
            )
    Di.DijkstraGuarding (AsIx ix) ->
        encodeObject
            ( "index" .=
                encodeWord32 ix
           <> "purpose" .=
                encodeText "guard"
            )
  where
    translate = Alonzo.encodeScriptPurposeIndex @AlonzoEra

encodeScriptPurposeItem
    :: Di.DijkstraPlutusPurpose AsItem DijkstraEra
    -> Json
encodeScriptPurposeItem = encodeObject . \case
    Di.DijkstraSpending (AsItem txIn) ->
        "purpose" .= encodeText "spend" <>
        "outputReference" .= encodeObject (Shelley.encodeTxIn txIn)
    Di.DijkstraMinting (AsItem policyId) ->
        "purpose" .= encodeText "mint" <>
        "policy" .= Mary.encodePolicyId policyId
    Di.DijkstraRewarding (AsItem acct) ->
        "purpose" .= encodeText "withdraw" <>
        "rewardAccount" .= Shelley.encodeRewardAcnt acct
    Di.DijkstraVoting (AsItem voter) ->
        "purpose" .= encodeText "vote" <>
        "issuer" .= Conway.encodeVoter voter
    Di.DijkstraProposing (AsItem proposal) ->
        "purpose" .= encodeText "propose" <>
        "proposal" .= encodeObject (encodeProposalProcedure proposal)
    Di.DijkstraCertifying (AsItem cert) ->
        case encodeTxCert cert of
            c :| [] ->
                "purpose" .= encodeText "publish" <>
                "certificate" .= encodeObject c
            _ :| (c : _) ->
                "purpose" .= encodeText "publish" <>
                "certificate" .= encodeObject c
    Di.DijkstraGuarding (AsItem scriptHash) ->
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
    :: Ledger.Core.PParamsUpdate DijkstraEra
    -> Json
encodePParamsUpdate (Ledger.Core.PParamsUpdate x) =
    encodePParamsHKD (\k enc v -> k .=? OmitWhenNothing enc v) (const SNothing) x

encodePParamsHKD
    :: forall f era. (HKDFunctor f)
    => (forall a. Text -> (a -> Json) -> Ledger.HKD f a -> Series)
    -> (forall a. a -> Ledger.HKD f a)
    -> Di.DijkstraPParams f era
    -> Json
encodePParamsHKD encode pure_ x =
    encode "minFeeCoefficient"
        (encodeInteger . unCoin . fromCompact . Ba.unCoinPerByte) (unTHKD (Di.dppTxFeePerByte x)) <>
    encode "minFeeConstant"
        (encodeCoin . fromCompact) (unTHKD (Di.dppTxFeeFixed x)) <>
    encode "minFeeReferenceScripts"
        (\(base :: NonNegativeInterval) -> encodeObject
            ( "base" .= encodeDouble (fromRational (unboundRational base))
            )
        )
        (unTHKD (Di.dppMinFeeRefScriptCostPerByte x)) <>
    encode "refScriptCostStride"
        (encodeNonZero encodeWord32)
        (unTHKD (Di.dppRefScriptCostStride x)) <>
    encode "refScriptCostMultiplier"
        (\(v :: PositiveInterval) -> encodeRational (unboundRational v))
        (unTHKD (Di.dppRefScriptCostMultiplier x)) <>
    encode "maxBlockBodySize"
        (encodeSingleton "bytes" . encodeWord32) (unTHKD (Di.dppMaxBBSize x)) <>
    encode "maxBlockHeaderSize"
        (encodeSingleton "bytes" . encodeWord16) (unTHKD (Di.dppMaxBHSize x)) <>
    encode "maxTransactionSize"
        (encodeSingleton "bytes" . encodeWord32) (unTHKD (Di.dppMaxTxSize x)) <>
    encode "maxReferenceScriptSizePerBlock"
        (encodeSingleton "bytes" . encodeWord32) (unTHKD (Di.dppMaxRefScriptSizePerBlock x)) <>
    encode "maxReferenceScriptSizePerTx"
        (encodeSingleton "bytes" . encodeWord32) (unTHKD (Di.dppMaxRefScriptSizePerTx x)) <>
    encode "stakeCredentialDeposit"
        (encodeCoin . fromCompact) (unTHKD (Di.dppKeyDeposit x)) <>
    encode "stakePoolDeposit"
        (encodeCoin . fromCompact) (unTHKD (Di.dppPoolDeposit x)) <>
    encode "stakePoolRetirementEpochBound"
        encodeEpochInterval (unTHKD (Di.dppEMax x)) <>
    encode "desiredNumberOfStakePools"
        encodeWord16 (unTHKD (Di.dppNOpt x)) <>
    encode "stakePoolPledgeInfluence"
        encodeNonNegativeInterval (unTHKD (Di.dppA0 x)) <>
    encode "monetaryExpansion"
        encodeUnitInterval (unTHKD (Di.dppRho x)) <>
    encode "treasuryExpansion"
        encodeUnitInterval (unTHKD (Di.dppTau x)) <>
    encode "minStakePoolCost"
        (encodeCoin . fromCompact) (unTHKD (Di.dppMinPoolCost x)) <>
    encode "minUtxoDepositConstant"
        encodeCoin (pure_ (Coin 0)) <>
    encode "minUtxoDepositCoefficient"
        (encodeInteger . unCoin . fromCompact . Ba.unCoinPerByte) (unTHKD (Di.dppCoinsPerUTxOByte x)) <>
    encode "plutusCostModels"
        Alonzo.encodeCostModels (unTHKD (Di.dppCostModels x)) <>
    encode "scriptExecutionPrices"
        Alonzo.encodePrices (unTHKD (Di.dppPrices x)) <>
    encode "maxExecutionUnitsPerTransaction"
        (Alonzo.encodeExUnits . Al.unOrdExUnits) (unTHKD (Di.dppMaxTxExUnits x)) <>
    encode "maxExecutionUnitsPerBlock"
        (Alonzo.encodeExUnits . Al.unOrdExUnits) (unTHKD (Di.dppMaxBlockExUnits x)) <>
    encode "maxValueSize"
        (encodeSingleton "bytes" . encodeWord32) (unTHKD (Di.dppMaxValSize x)) <>
    encode "collateralPercentage"
        encodeWord16 (unTHKD (Di.dppCollateralPercentage x)) <>
    encode "maxCollateralInputs"
        encodeWord16 (unTHKD (Di.dppMaxCollateralInputs x)) <>
    encode "version"
        Shelley.encodeProtVer (fromNoUpdate @f @ProtVer (Di.dppProtocolVersion x)) <>
    encode "stakePoolVotingThresholds"
        Conway.encodePoolVotingThresholds (unTHKD (Di.dppPoolVotingThresholds x)) <>
    encode "delegateRepresentativeVotingThresholds"
        Conway.encodeDRepVotingThresholds (unTHKD (Di.dppDRepVotingThresholds x)) <>
    encode "constitutionalCommitteeMinSize"
        encodeWord16 (unTHKD (Di.dppCommitteeMinSize x)) <>
    encode "constitutionalCommitteeMaxTermLength"
        encodeEpochInterval (unTHKD (Di.dppCommitteeMaxTermLength x)) <>
    encode "governanceActionLifetime"
        encodeEpochInterval (unTHKD (Di.dppGovActionLifetime x)) <>
    encode "governanceActionDeposit"
        (encodeCoin . fromCompact) (unTHKD (Di.dppGovActionDeposit x)) <>
    encode "delegateRepresentativeDeposit"
        (encodeCoin . fromCompact) (unTHKD (Di.dppDRepDeposit x)) <>
    encode "delegateRepresentativeMaxIdleTime"
        encodeEpochInterval (unTHKD (Di.dppDRepActivity x))
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
                    Di.DijkstraSpending (AsIx ix) -> ("spending input", ix)
                    Di.DijkstraMinting (AsIx ix) -> ("minting policy", ix)
                    Di.DijkstraCertifying (AsIx ix) -> ("publishing certificate", ix)
                    Di.DijkstraRewarding (AsIx ix) -> ("withdrawing from account", ix)
                    Di.DijkstraVoting (AsIx ix) -> ("voting as voter", ix)
                    Di.DijkstraProposing (AsIx ix) -> ("proposing governance proposal", ix)
                    Di.DijkstraGuarding (AsIx ix) -> ("guarding script", ix)
          in "Couldn't find corresponding redeemer for " <> title <> " #" <> show ptr <> ". Verify your transaction's construction."
    Cn.BabbageContextError (Ba.AlonzoContextError (Al.TimeTranslationPastHorizon e)) ->
        "Uncomputable slot arithmetic; transaction's validity bounds go beyond the foreseeable end of the current era: " <> e
    Cn.BabbageContextError (Ba.AlonzoContextError (Al.TranslationLogicMissingInput i)) ->
        "Unknown transaction input (missing from UTxO set): " <> Shelley.stringifyTxIn i
    Cn.ReferenceInputsNotDisjointFromInputs{} ->
        "Reference inputs overlap with regular inputs. They must be disjoint."

encodeContextError
    :: Di.DijkstraContextError DijkstraEra
    -> Json
encodeContextError = \case
    Di.ConwayContextError err ->
        encodeConwayContextError err
    Di.PointerPresentInOutput{} ->
        encodeText "Found outputs with pointer addresses. Those are no longer supported."

encodePParams :: Ledger.Core.PParams DijkstraEra -> Json
encodePParams (Ledger.Core.PParams x) =
    encodePParamsHKD (\k enc v -> k .= enc v) identity x

encodeUtxo :: Sh.UTxO DijkstraEra -> Json
encodeUtxo =
    encodeList id
    . Map.foldrWithKey (\i o -> (:) (encodeObject (Shelley.encodeTxIn i <> encodeTxOut includeAllCbor o))) []
    . Sh.unUTxO

encodeGovActionState :: Cn.GovActionState DijkstraEra -> Json
encodeGovActionState st =
      "proposal" .= Conway.encodeGovActionId (Cn.gasId st)
   <> encodeProposalProcedure (Cn.gasProposalProcedure st)
   <> "since" .= (encodeSingleton "epoch" . encodeEpochNo) (Cn.gasProposedIn st)
   <> "until" .= (encodeSingleton "epoch" . encodeEpochNo) (Cn.gasExpiresAfter st)
   <> "votes" .= encodeMapAsList
        (\issuer vote -> encodeObject
            ( "issuer" .= Conway.encodeVoter issuer
           <> "vote" .= Conway.encodeVote vote
            )
        )
        (mconcat
            [ Map.mapKeys Cn.CommitteeVoter (Cn.gasCommitteeVotes st)
            , Map.mapKeys Cn.DRepVoter (Cn.gasDRepVotes st)
            , Map.mapKeys Cn.StakePoolVoter (Cn.gasStakePoolVotes st)
            ]
        )
    & encodeObject

govProposals
    :: Ledger.GovState DijkstraEra
    -> OMap.OMap Cn.GovActionId (Cn.GovActionState DijkstraEra)
govProposals = view Cn.pPropsL . Cn.cgsProposals
