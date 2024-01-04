--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
{-# LANGUAGE TypeOperators #-}

module Ogmios.Data.Json.Conway where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.BaseTypes
    ( EpochNo
    , ProtVer
    )
import Cardano.Ledger.Binary
    ( sizedValue
    )
import Cardano.Ledger.HKD
    ( HKDFunctor (..)
    )
import Cardano.Ledger.Keys
    ( KeyRole (..)
    )
import Cardano.Ledger.SafeHash
    ( extractHash
    )
import Data.Maybe.Strict
    ( strictMaybe
    )
import Ouroboros.Consensus.Protocol.Praos
    ( Praos
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )
import Ouroboros.Consensus.Shelley.Protocol.Praos
    ()

import qualified Data.Map as Map

import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Binary as Binary
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.DRep as Ledger
import qualified Cardano.Ledger.HKD as Ledger

import qualified Cardano.Ledger.Shelley.PParams as Sh

import qualified Cardano.Ledger.Alonzo.PParams as Al
import qualified Cardano.Ledger.Alonzo.TxSeq as Al

import qualified Cardano.Ledger.Babbage.Core as Ba
import qualified Cardano.Ledger.Babbage.Tx as Ba

import qualified Cardano.Ledger.Conway.Genesis as Cn
import qualified Cardano.Ledger.Conway.Governance as Cn
import qualified Cardano.Ledger.Conway.PParams as Cn
import qualified Cardano.Ledger.Conway.Tx as Cn
import qualified Cardano.Ledger.Conway.TxBody as Cn
import qualified Cardano.Ledger.Conway.TxCert as Cn

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Babbage as Babbage
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

encodeAnchor
    :: Crypto crypto
    => Cn.Anchor crypto
    -> Json
encodeAnchor x = encodeObject
    ( "url" .=
        encodeUrl (Cn.anchorUrl x)
   <> "hash" .=
        Shelley.encodeHash (extractHash (Cn.anchorDataHash x))
    )

encodeBlock
    :: ( Crypto crypto
       )
    => IncludeCbor
    -> ShelleyBlock (Praos crypto) (ConwayEra crypto)
    -> Json
encodeBlock opts (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "type" .= encodeText "praos"
        <>
          "era" .= encodeText "babbage"
        <>
          "id" .= Shelley.encodeShelleyHash headerHash
        <>
          Babbage.encodeHeader blkHeader
        <>
          "transactions" .= encodeFoldable (encodeTx opts) (Al.txSeqTxns txs)
        )

encodeCommittee
    :: Crypto crypto
    => Cn.Committee (ConwayEra crypto)
    -> Json
encodeCommittee x = encodeObject
    ( "members" .=
        encodeMapAsList (\k -> encodeConstitutionalCommitteeMember k . SJust) (Cn.committeeMembers x)
   <> "quorum" .=
        encodeUnitInterval (Cn.committeeQuorum x)
    )

encodeConstitution
    :: Crypto crypto
    => Cn.Constitution (ConwayEra crypto)
    -> Series
encodeConstitution x =
    "anchor" .=
        encodeAnchor (Cn.constitutionAnchor x) <>
    "hash" .=? OmitWhenNothing
        Shelley.encodeScriptHash (Cn.constitutionScript x)

encodeConstitutionalCommitteeMember
    :: Crypto crypto
    => Ledger.Credential 'ColdCommitteeRole crypto
    -> StrictMaybe EpochNo
    -> Json
encodeConstitutionalCommitteeMember memberId mandate =
    encodeObject
        ( "id" .= Shelley.encodeCredential memberId
       <> "mandate" .=? OmitWhenNothing
            (encodeSingleton "epoch" . encodeEpochNo) mandate
        )

encodeConwayGovCert
    :: Crypto crypto
    => Cn.ConwayGovCert crypto
    -> Series
encodeConwayGovCert = \case
    Cn.ConwayRegDRep credential deposit anchor ->
        "type" .=
            encodeText "delegateRepresentativeRegistration"
       <>
        "delegateRepresentative" .=
            encodeDRep (Ledger.DRepCredential credential)
       <>
        "deposit" .=
            encodeCoin deposit
       <>
        "anchor" .=? OmitWhenNothing
            encodeAnchor anchor
    Cn.ConwayUnRegDRep credential deposit ->
        "type" .=
            encodeText "delegateRepresentativeRetirement"
       <>
        "delegateRepresentative" .=
            encodeDRep (Ledger.DRepCredential credential)
       <>
        "deposit" .=
            encodeCoin deposit
    Cn.ConwayUpdateDRep credential anchor ->
        "type" .=
            encodeText "delegateRepresentativeUpdate"
       <>
        "delegateRepresentative" .=
            encodeDRep (Ledger.DRepCredential credential)
       <>
        "anchor" .=
            encodeStrictMaybe encodeAnchor anchor
    Cn.ConwayAuthCommitteeHotKey cold hot ->
        "type" .=
            encodeText "constitutionalCommitteeHotKeyRegistration"
       <>
        "member" .=
            encodeConstitutionalCommitteeMember cold SNothing
       <>
        "hotKey" .=
            Shelley.encodeCredential hot
    Cn.ConwayResignCommitteeColdKey cold anchor ->
        "type" .=
            encodeText "constitutionalCommitteeRetirement"
       <>
        "member" .=
            encodeConstitutionalCommitteeMember cold SNothing
       <>
        "anchor" .=
            encodeStrictMaybe encodeAnchor anchor

encodeDelegCert
    :: Crypto crypto
    => Cn.ConwayDelegCert crypto
    -> [Series]
encodeDelegCert = \case
    Cn.ConwayRegCert credential deposit ->
        [ "type" .=
            encodeText "stakeCredentialRegistration"
       <> "credential" .=
            Shelley.encodeCredential credential
       <> "deposit" .=? OmitWhenNothing
            encodeCoin deposit
        ]
    Cn.ConwayUnRegCert credential deposit ->
        [ "type" .=
            encodeText "stakeCredentialDeregistration"
        <> "credential" .=
            Shelley.encodeCredential credential
        <> "deposit" .=? OmitWhenNothing
            encodeCoin deposit
        ]
    Cn.ConwayDelegCert credential delegatee ->
        [ "type" .=
            encodeText "stakeDelegation"
        <> "credential" .=
            Shelley.encodeCredential credential
        <> encodeDelegatee delegatee
        ]
    Cn.ConwayRegDelegCert credential delegatee deposit ->
        [ "type" .=
            encodeText "stakeCredentialRegistration"
       <> "credential" .=
            Shelley.encodeCredential credential
       <> "deposit" .=
            encodeCoin deposit
        , "type" .=
            encodeText "stakeDelegation"
       <> "credential" .=
            Shelley.encodeCredential credential
       <> encodeDelegatee delegatee
        ]

encodeDelegatee
    :: Crypto crypto
    => Cn.Delegatee crypto
    -> Series
encodeDelegatee = \case
    Cn.DelegStake poolId ->
        "stakePool" .= encodeSingleton "id" (Shelley.encodePoolId poolId)
    Cn.DelegVote drep ->
        "delegateRepresentative" .= encodeDRep drep
    Cn.DelegStakeVote poolId drep ->
        "stakePool" .= encodeSingleton "id" (Shelley.encodePoolId poolId)
        <>
        "delegateRepresentative" .= encodeDRep drep

encodeDRep
    :: Crypto crypto
    => Ledger.DRep crypto
    -> Json
encodeDRep = encodeObject . \case
    Ledger.DRepCredential credential ->
        "type" .= encodeText "registered" <>
        "id" .= Shelley.encodeCredential credential
    Ledger.DRepAlwaysAbstain ->
        "type" .= encodeText "abstain"
    Ledger.DRepAlwaysNoConfidence ->
        "type" .= encodeText "noConfidence"

encodeGenesis
    :: Crypto crypto
    => Cn.ConwayGenesis crypto
    -> Json
encodeGenesis x =
    encodeObject
        ( "era" .= encodeText "conway"
       <> "constitution" .= encodeObject (encodeConstitution (Cn.cgConstitution x))
       <> "constitutionalCommittee" .= encodeCommittee (Cn.cgCommittee x)
       <> "updatableParameters" .= encodeObject
            ( "stakePoolVotingThresholds" .=
                encodePoolVotingThresholds (Cn.ucppPoolVotingThresholds (Cn.cgUpgradePParams x))
           <> "delegateRepresentativeVotingThresholds" .=
                encodeDRepVotingThresholds (Cn.ucppDRepVotingThresholds (Cn.cgUpgradePParams x))
           <> "constitutionalCommitteeMinSize" .=
                encodeNatural (Cn.ucppCommitteeMinSize (Cn.cgUpgradePParams x))
           <> "constitutionalCommitteeMaxTermLength" .=
                encodeNatural (Cn.ucppCommitteeMaxTermLength (Cn.cgUpgradePParams x))
           <> "governanceActionLifetime" .=
                encodeEpochNo (Cn.ucppGovActionLifetime (Cn.cgUpgradePParams x))
           <> "governanceActionDeposit" .=
                encodeCoin (Cn.ucppGovActionDeposit (Cn.cgUpgradePParams x))
           <> "delegateRepresentativeDeposit" .=
                encodeCoin (Cn.ucppDRepDeposit (Cn.cgUpgradePParams x))
           <> "delegateRepresentativeMaxIdleTime" .=
                encodeEpochNo (Cn.ucppDRepActivity (Cn.cgUpgradePParams x))
            )
        )

encodeGovAction
    :: Crypto crypto
    => Cn.GovAction (ConwayEra crypto)
    -> Json
encodeGovAction = \case
    Cn.ParameterChange _prevGovActionId pparamsUpdate ->
        encodeObject
            ( "type" .=
                encodeText "protocolParametersUpdate"
           <> "parameters" .=
                encodePParamsUpdate pparamsUpdate
            )
    Cn.HardForkInitiation _prevGovActionId version ->
        encodeObject
            ( "type" .=
                encodeText "hardForkInitiation"
           <> "version" .=
                Shelley.encodeProtVer version
            )
    Cn.TreasuryWithdrawals withdrawals ->
        encodeObject
            ( "type" .=
                encodeText "treasuryWithdrawals"
           <> "withdrawals" .=
                encodeMap Shelley.stringifyRewardAcnt encodeCoin withdrawals
            )
    Cn.UpdateCommittee _prevGovActionId removed added quorum ->
        encodeObject
            ( "type" .=
                encodeText "constitutionalCommittee"
           <> "members" .= encodeObject
                ( "added" .=
                    encodeMapAsList (\k -> encodeConstitutionalCommitteeMember k . SJust) added
               <> "removed" .=
                    encodeFoldable (`encodeConstitutionalCommitteeMember` SNothing) removed
                )
           <> "quorum" .=
                encodeUnitInterval quorum
            )
    Cn.NewConstitution _prevGovActionId constitution  ->
        encodeObject
            ( "type" .=
                encodeText "constitution"
           <> encodeConstitution constitution
            )
    Cn.NoConfidence  _prevGovActionId ->
        encodeObject
            ( "type" .=
                encodeText "noConfidence"
            )
    Cn.InfoAction ->
        encodeObject
            ( "type" .=
                encodeText "information"
            )

encodeGovActionId
    :: Crypto crypto
    => Cn.GovActionId crypto
    -> Json
encodeGovActionId x =
    encodeObject
        ( "transaction" .=
            encodeObject (Shelley.encodeTxId (Cn.gaidTxId x))
        <> "index" .=
            encodeGovActionIx (Cn.gaidGovActionIx x)
        )

encodeGovActionIx
    :: Cn.GovActionIx
    -> Json
encodeGovActionIx (Cn.GovActionIx ix) =
    encodeWord32 ix

encodeGovActionPurpose
    :: Cn.GovActionPurpose
    -> Json
encodeGovActionPurpose = \case
  Cn.PParamUpdatePurpose ->
      encodeText "protocolParametersUpdate"
  Cn.HardForkPurpose ->
      encodeText "hardForkInitiation"
  Cn.CommitteePurpose ->
      encodeText "constitutionalCommittee"
  Cn.ConstitutionPurpose ->
      encodeText "constitution"

encodePParams
    :: (Ledger.PParamsHKD Identity era ~ Cn.ConwayPParams Identity era)
    => Ledger.PParams era
    -> Json
encodePParams (Ledger.PParams x) =
    encodePParamsHKD (\k encode v -> k .= encode v) identity x

encodePParamsUpdate
    :: (Ledger.PParamsHKD StrictMaybe era ~ Cn.ConwayPParams StrictMaybe era)
    => Ledger.PParamsUpdate era
    -> Json
encodePParamsUpdate (Ledger.PParamsUpdate x) =
    encodePParamsHKD (\k encode v -> k .=? OmitWhenNothing encode v) (const SNothing) x

encodeProposedPPUpdates
    :: forall era.
        ( Ledger.PParamsHKD StrictMaybe era ~ Cn.ConwayPParams StrictMaybe era
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
    :: forall f era. (HKDFunctor f)
    => (forall a. Text -> (a -> Json) -> Ledger.HKD f a -> Series)
    -> (forall a. a -> Ledger.HKD f a)
    -> Cn.ConwayPParams f era
    -> Json
encodePParamsHKD encode pure_ x =
    encode "minFeeCoefficient"
        (encodeInteger . unCoin) (Cn.cppMinFeeA x) <>
    encode "minFeeConstant"
        encodeCoin (Cn.cppMinFeeB x) <>
    encode "maxBlockBodySize"
        (encodeSingleton "bytes" . encodeNatural) (Cn.cppMaxBBSize x) <>
    encode "maxBlockHeaderSize"
        (encodeSingleton "bytes" . encodeNatural) (Cn.cppMaxBHSize x) <>
    encode "maxTransactionSize"
        (encodeSingleton "bytes" . encodeNatural) (Cn.cppMaxTxSize x) <>
    encode "stakeCredentialDeposit"
        encodeCoin (Cn.cppKeyDeposit x) <>
    encode "stakePoolDeposit"
        encodeCoin (Cn.cppPoolDeposit x) <>
    encode "stakePoolRetirementEpochBound"
        encodeEpochNo (Cn.cppEMax x) <>
    encode "desiredNumberOfStakePools"
        encodeNatural (Cn.cppNOpt x) <>
    encode "stakePoolPledgeInfluence"
        encodeNonNegativeInterval (Cn.cppA0 x) <>
    encode "monetaryExpansion"
        encodeUnitInterval (Cn.cppRho x) <>
    encode "treasuryExpansion"
        encodeUnitInterval (Cn.cppTau x) <>
    encode "minStakePoolCost"
        encodeCoin (Cn.cppMinPoolCost x) <>
    encode "minUtxoDepositConstant"
        encodeCoin (pure_ (Coin 0)) <>
    encode "minUtxoDepositCoefficient"
        (encodeInteger . unCoin . Ba.unCoinPerByte) (Cn.cppCoinsPerUTxOByte x) <>
    encode "plutusCostModels"
        Alonzo.encodeCostModels (Cn.cppCostModels x) <>
    encode "scriptExecutionPrices"
        Alonzo.encodePrices (Cn.cppPrices x) <>
    encode "maxExecutionUnitsPerTransaction"
        (Alonzo.encodeExUnits . Al.unOrdExUnits) (Cn.cppMaxTxExUnits x) <>
    encode "maxExecutionUnitsPerBlock"
        (Alonzo.encodeExUnits . Al.unOrdExUnits) (Cn.cppMaxBlockExUnits x) <>
    encode "maxValueSize"
        (encodeSingleton "bytes" . encodeNatural) (Cn.cppMaxValSize x) <>
    encode "collateralPercentage"
        encodeNatural (Cn.cppCollateralPercentage x) <>
    encode "maxCollateralInputs"
        encodeNatural (Cn.cppMaxCollateralInputs x) <>
    encode "version"
        Shelley.encodeProtVer (fromNoUpdate @f @ProtVer (Cn.cppProtocolVersion x)) <>
    encode "stakePoolVotingThresholds"
        encodePoolVotingThresholds (Cn.cppPoolVotingThresholds x) <>
    encode "delegateRepresentativeVotingThresholds"
        encodeDRepVotingThresholds (Cn.cppDRepVotingThresholds x) <>
    encode "constitutionalCommitteeMinSize"
        encodeNatural (Cn.cppCommitteeMinSize x) <>
    encode "constitutionalCommitteeMaxTermLength"
        encodeNatural (Cn.cppCommitteeMaxTermLength x) <>
    encode "governanceActionLifetime"
        encodeEpochNo (Cn.cppGovActionLifetime x) <>
    encode "governanceActionDeposit"
        encodeCoin (Cn.cppGovActionDeposit x) <>
    encode "delegateRepresentativeDeposit"
        encodeCoin (Cn.cppDRepDeposit x) <>
    encode "delegateRepresentativeMaxIdleTime"
        encodeEpochNo (Cn.cppDRepActivity x)
    & encodeObject

encodeDRepVotingThresholds :: Cn.DRepVotingThresholds -> Json
encodeDRepVotingThresholds x =
    "noConfidence" .= encodeUnitInterval (Cn.dvtMotionNoConfidence x) <>
    "constitutionalCommittee" .= encodeObject
        ( "default" .= encodeUnitInterval (Cn.dvtCommitteeNormal x)
       <> "stateOfNoConfidence" .= encodeUnitInterval (Cn.dvtCommitteeNoConfidence x)
        ) <>
    "constitution" .= encodeUnitInterval (Cn.dvtUpdateToConstitution x) <>
    "hardForkInitiation" .= encodeUnitInterval (Cn.dvtHardForkInitiation x) <>
    "protocolParametersUpdate" .= encodeObject
        ( "network" .= encodeUnitInterval (Cn.dvtPPNetworkGroup x)
       <> "economic" .= encodeUnitInterval (Cn.dvtPPEconomicGroup x)
       <> "technical" .= encodeUnitInterval (Cn.dvtPPTechnicalGroup x)
       <> "governance" .= encodeUnitInterval (Cn.dvtPPGovGroup x)
        ) <>
    "treasuryWithdrawals" .= encodeUnitInterval (Cn.dvtTreasuryWithdrawal x)
    & encodeObject

encodePoolVotingThresholds :: Cn.PoolVotingThresholds -> Json
encodePoolVotingThresholds x =
    "noConfidence" .= encodeUnitInterval (Cn.pvtMotionNoConfidence x) <>
    "constitutionalCommittee" .= encodeObject
        ( "default" .= encodeUnitInterval (Cn.pvtCommitteeNormal x)
       <> "stateOfNoConfidence" .= encodeUnitInterval (Cn.pvtCommitteeNoConfidence x)
        ) <>
    "hardForkInitiation" .= encodeUnitInterval (Cn.pvtHardForkInitiation x)
    & encodeObject

encodeProposalProcedure
    :: Crypto crypto
    => Cn.ProposalProcedure (ConwayEra crypto)
    -> Json
encodeProposalProcedure x = encodeObject
    ( "deposit" .=
        encodeCoin (Cn.pProcDeposit x)
   <> "returnAccount" .=
        Shelley.encodeRewardAcnt (Cn.pProcReturnAddr x)
   <> "anchor" .=
       encodeAnchor (Cn.pProcAnchor x)
   <> "action" .=
        encodeGovAction (Cn.pProcGovAction x)
    )

encodeTx
    :: forall era crypto.
        ( Crypto crypto
        , era ~ ConwayEra crypto
        )
   => IncludeCbor
    -> Ba.AlonzoTx era
    -> Json
encodeTx opts x =
    encodeObject
        ( Shelley.encodeTxId (Ledger.txid @(ConwayEra crypto) (Cn.body x))
       <>
        "spends" .= Alonzo.encodeIsValid (Cn.isValid x)
       <>
        encodeTxBody opts (Cn.body x) (strictMaybe mempty (Map.keys . snd) auxiliary)
       <>
        "metadata" .=? OmitWhenNothing fst auxiliary
       <>
        Alonzo.encodeWitnessSet opts (snd <$> auxiliary) (Cn.wits x)
       <>
        if includeTransactionCbor opts then
           "cbor" .= encodeByteStringBase16 (Binary.serialize' (Ledger.eraProtVerLow @era) x)
        else
           mempty
        )
  where
    auxiliary = do
        hash <- Shelley.encodeAuxiliaryDataHash <$> Cn.ctbAdHash (Cn.body x)
        (labels, scripts) <- Alonzo.encodeAuxiliaryData opts <$> Ba.auxiliaryData x
        pure
            ( encodeObject ("hash" .= hash <> "labels" .= labels)
            , scripts
            )

encodeTxBody
    :: Crypto crypto
    => IncludeCbor
    -> Cn.ConwayTxBody (ConwayEra crypto)
    -> [Ledger.ScriptHash crypto]
    -> Series
encodeTxBody opts x scripts =
    "inputs" .=
        encodeFoldable (encodeObject . Shelley.encodeTxIn) (Cn.ctbSpendInputs x) <>
    "references" .=? OmitWhen null
        (encodeFoldable (encodeObject . Shelley.encodeTxIn)) (Cn.ctbReferenceInputs x) <>
    "outputs" .=
        encodeFoldable (encodeObject . Babbage.encodeTxOut opts . sizedValue) (Cn.ctbOutputs x) <>
    "collaterals" .=? OmitWhen null
        (encodeFoldable (encodeObject . Shelley.encodeTxIn)) (Cn.ctbCollateralInputs x) <>
    "collateralReturn" .=? OmitWhenNothing
        (encodeObject . Babbage.encodeTxOut opts . sizedValue) (Cn.ctbCollateralReturn x) <>
    "totalCollateral" .=? OmitWhenNothing
        encodeCoin (Cn.ctbTotalCollateral x) <>
    "certificates" .=? OmitWhen null
        (encodeConcatFoldable (fmap encodeObject . encodeTxCert)) (Cn.ctbCerts x) <>
    "withdrawals" .=? OmitWhen (null . Ledger.unWithdrawals)
        Shelley.encodeWdrl (Cn.ctbWithdrawals x) <>
    "mint" .=? OmitWhen (== mempty)
        (encodeObject . Mary.encodeMultiAsset) (Cn.ctbMint x) <>
    "requiredExtraSignatories" .=? OmitWhen null
        (encodeFoldable Shelley.encodeKeyHash) (Cn.ctbReqSignerHashes x) <>
    "requiredExtraScripts" .=? OmitWhen null
        (encodeFoldable Shelley.encodeScriptHash) scripts <>
    "network" .=? OmitWhenNothing
        Shelley.encodeNetwork (Cn.ctbTxNetworkId x) <>
    "scriptIntegrityHash" .=? OmitWhenNothing
        Alonzo.encodeScriptIntegrityHash (Cn.ctbScriptIntegrityHash x) <>
    "fee" .=
        encodeCoin (Cn.ctbTxfee x) <>
    "validityInterval" .=
        Allegra.encodeValidityInterval (Cn.ctbVldt x) <>
    "proposals" .=? OmitWhen null
        (encodeFoldable encodeProposalProcedure)
        (Cn.ctbProposalProcedures x) <>
    "votes" .=? OmitWhen (null . Cn.unVotingProcedures)
        encodeVotingProcedures
        (Cn.ctbVotingProcedures x)

encodeTxCert
    :: forall era crypto.
        ( EraCrypto (era crypto) ~ crypto
        , Crypto crypto
        )
    => Cn.ConwayTxCert (era crypto)
    -> [Series]
encodeTxCert = \case
    Cn.ConwayTxCertDeleg dCert ->
        encodeDelegCert dCert
    Cn.ConwayTxCertPool pCert ->
        [Shelley.encodePoolCert pCert]
    Cn.ConwayTxCertGov cCert ->
        [encodeConwayGovCert cCert]

encodeVotingProcedures
    :: Crypto crypto
    => Cn.VotingProcedures (ConwayEra crypto)
    -> Json
encodeVotingProcedures =
    encodeList identity . Map.foldrWithKey encodeProcedures mempty . Cn.unVotingProcedures
  where
    encodeProcedures issuer =
        flip $ Map.foldrWithKey
            (\govActionId  -> (:) . encodeVotingProcedure issuer govActionId)

encodeVotingProcedure
    :: Crypto crypto
    => Cn.Voter crypto
    -> Cn.GovActionId crypto
    -> Cn.VotingProcedure (ConwayEra crypto)
    -> Json
encodeVotingProcedure issuer govActionId x =
    encodeObject
        ( "proposal" .=
            encodeGovActionId govActionId
       <> "issuer" .=
           encodeVoter issuer
       <> "vote" .=
           encodeVote (Cn.vProcVote x)
       <> "anchor" .=? OmitWhenNothing
            encodeAnchor (Cn.vProcAnchor x)
        )

encodeVote
    :: Cn.Vote
    -> Json
encodeVote = \case
    Cn.VoteYes -> encodeText "yes"
    Cn.VoteNo -> encodeText "no"
    Cn.Abstain -> encodeText "abstain"

encodeVoter
    :: Crypto crypto
    => Cn.Voter crypto
    -> Json
encodeVoter = encodeObject . \case
    Cn.CommitteeVoter credential ->
        "role" .= encodeText "constitutionalCommittee" <>
        "id" .= Shelley.encodeCredential credential
    Cn.DRepVoter credential ->
        "role" .= encodeText "delegateRepresentative" <>
        "id" .= Shelley.encodeCredential credential
    Cn.StakePoolVoter poolId ->
        "role" .= encodeText "stakePoolOperator" <>
        "id" .= Shelley.encodePoolId poolId
