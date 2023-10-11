--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
{-# LANGUAGE TypeOperators #-}

module Ogmios.Data.Json.Conway where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Binary
    ( sizedValue
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
import qualified Cardano.Ledger.Keys as Ledger

import qualified Cardano.Ledger.Alonzo.TxSeq as Al

import qualified Cardano.Ledger.Babbage.PParams as Ba
import qualified Cardano.Ledger.Babbage.Tx as Ba

import qualified Cardano.Ledger.Conway.Genesis as Cn
import qualified Cardano.Ledger.Conway.Governance as Cn
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

-- TODO: Force the purpose to 'ColdCommitteeRole in the next version of the
-- cardano-ledger. In the current one, there's a discrepancy between the phantom
-- type given to 'GovernanceAction' ('Voting) and the one given to the tx
-- certificate ('CommitteeColdKey).
--
-- Although they ought to both refer to the same thing.
encodeConstitutionalCommitteeMember
    :: (Crypto crypto, purpose :\: 'Ledger.StakePool)
    => Ledger.KeyHash purpose crypto
    -> Json
encodeConstitutionalCommitteeMember memberId =
    encodeObject
        ( "id" .= Shelley.encodeKeyHash memberId
        )

encodeConwayCommitteeCert
    :: Crypto crypto
    => Cn.ConwayCommitteeCert crypto
    -> Series
encodeConwayCommitteeCert = \case
    Cn.ConwayRegDRep credential deposit ->
        "type" .=
            encodeText "delegateRepresentativeRegistration"
       <>
        "delegateRepresentative" .=
            encodeDRep (Ledger.DRepCredential credential)
       <>
        "deposit" .=
            encodeCoin deposit
    Cn.ConwayUnRegDRep credential deposit ->
        "type" .=
            encodeText "delegateRepresentativeRetirement"
       <>
        "delegateRepresentative" .=
            encodeDRep (Ledger.DRepCredential credential)
       <>
        "deposit" .=
            encodeCoin deposit
    Cn.ConwayAuthCommitteeHotKey coldKey hotKey ->
        "type" .=
            encodeText "constitutionalCommitteeHotKeyRegistration"
       <>
        "member" .=
            encodeConstitutionalCommitteeMember coldKey
       <>
        "hotKey" .=
            Shelley.encodeKeyHash hotKey
    Cn.ConwayResignCommitteeColdKey coldKey ->
        "type" .=
            encodeText "constitutionalCommitteeRetirement"
       <>
        "member" .=
            encodeConstitutionalCommitteeMember coldKey

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
        ( "era" .=
            encodeText "conway"
       <> "initialDelegates" .=
           Shelley.encodeInitialDelegates (Ledger.unGenDelegs (Cn.cgGenDelegs x))
        )

encodeGovernanceAction
    :: Crypto crypto
    => Cn.GovernanceAction (ConwayEra crypto)
    -> Json
encodeGovernanceAction = \case
    Cn.ParameterChange pparamsUpdate ->
        encodeObject
            ( "type" .=
                encodeText "protocolParametersUpdate"
           <> "parameters" .=
                encodePParamsUpdate pparamsUpdate
            )
    Cn.HardForkInitiation version ->
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
                encodeMap Shelley.stringifyCredential encodeCoin withdrawals
            )
    Cn.NewCommittee members quorum ->
        encodeObject
            ( "type" .=
                encodeText "constitutionalCommittee"
           <> "members" .=
                encodeFoldable encodeConstitutionalCommitteeMember members
           <> "quorum" .=
                encodeRational quorum
            )
    Cn.NewConstitution hash ->
        encodeObject
            ( "type" .=
                encodeText "constitution"
           <> "hash" .=
                Shelley.encodeHash (extractHash hash)
            )
    Cn.NoConfidence ->
        encodeObject
            ( "type" .=
                encodeText "noConfidence"
            )
    Cn.InfoAction ->
        encodeObject
            ( "type" .=
                encodeText "information"
            )

encodeGovernanceActionId
    :: Crypto crypto
    => Cn.GovernanceActionId crypto
    -> Json
encodeGovernanceActionId x =
    encodeObject
        ( "transaction" .=
            encodeObject (Shelley.encodeTxId (Cn.gaidTxId x))
        <> "index" .=
            encodeGovernanceActionIx (Cn.gaidGovActionIx x)
        )

encodeGovernanceActionIx
    :: Cn.GovernanceActionIx
    -> Json
encodeGovernanceActionIx (Cn.GovernanceActionIx ix) =
    encodeWord64 ix

encodePParams
    :: (Ledger.PParamsHKD Identity era ~ Ba.BabbagePParams Identity era)
    => Ledger.PParams era
    -> Json
encodePParams (Ledger.PParams x) =
    Babbage.encodePParamsHKD (\k encode v -> k .= encode v) identity x

encodePParamsUpdate
    :: (Ledger.PParamsHKD StrictMaybe era ~ Ba.BabbagePParams StrictMaybe era)
    => Ledger.PParamsUpdate era
    -> Json
encodePParamsUpdate (Ledger.PParamsUpdate x) =
    Babbage.encodePParamsHKD (\k encode v -> k .=? OmitWhenNothing encode v) (const SNothing) x

encodeProposalProcedure
    :: Crypto crypto
    => Cn.ProposalProcedure (ConwayEra crypto)
    -> Json
encodeProposalProcedure x = encodeObject
    ( "deposit" .=
        encodeCoin (Cn.pProcDeposit x)
   <> "returnAccount" .=
        Shelley.encodeKeyHash (Cn.pProcReturnAddr x)
   <> "anchor" .=? OmitWhenNothing
        encodeAnchor
        (Cn.pProcAnchor x)
   <> "action" .=
        encodeGovernanceAction (Cn.pProcGovernanceAction x)
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
    "votes" .=? OmitWhen null
        (encodeFoldable encodeVotingProcedure)
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
    Cn.ConwayTxCertCommittee cCert ->
        [encodeConwayCommitteeCert cCert]

encodeVotingProcedure
    :: Crypto crypto
    => Cn.VotingProcedure (ConwayEra crypto)
    -> Json
encodeVotingProcedure x = encodeObject
    ( "proposal" .=
        encodeGovernanceActionId (Cn.vProcGovActionId x)
   <> "issuer" .=
       encodeVoter (Cn.vProcVoter x)
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
