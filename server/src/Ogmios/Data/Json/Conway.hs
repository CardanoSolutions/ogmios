--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

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

import qualified Cardano.Ledger.Conway.Delegation.Certificates as Cn
import qualified Cardano.Ledger.Conway.Genesis as Cn
import qualified Cardano.Ledger.Conway.Governance as Cn
import qualified Cardano.Ledger.Conway.Tx as Cn
import qualified Cardano.Ledger.Conway.TxBody as Cn

import qualified Cardano.Ledger.Credential as Ledger
import Cardano.Ledger.Keys
    ( KeyRole (..)
    )
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

encodeDCert
    :: Crypto crypto
    => Cn.ConwayDCert crypto
    -> [Json]
encodeDCert = \case
    Cn.ConwayDCertDeleg dCert ->
        encodeObject <$> encodeDelegCert dCert
    Cn.ConwayDCertPool pCert ->
        [encodeObject (Shelley.encodePoolCert pCert)]
    Cn.ConwayDCertConstitutional cCert ->
        [encodeObject (Shelley.encodeConstitutionalDelegCert cCert)]

-- TODO: This has heavily changed in the recent ledger versions.
-- In this version of the ledger, registration and delegation certificates have
-- been merged into one, so we manually unwind them into two certificates.
--
-- This is changed in later version of the ledger which we can't rely on just
-- yet because the entire Cardano stack ain't ready for it.
encodeDelegCert
    :: Crypto crypto
    => Cn.ConwayDelegCert crypto
    -> [Series]
encodeDelegCert = \case
    Cn.ConwayDeleg credential delegatee deposit ->
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
    Cn.ConwayReDeleg credential delegatee ->
        [ "type" .=
            encodeText "stakeDelegation"
        <> "credential" .=
            Shelley.encodeCredential credential
        <> encodeDelegatee delegatee
        ]
    Cn.ConwayUnDeleg credential deposit ->
        [ "type" .=
            encodeText "stakeCredentialDeregistration"
        <> "credential" .=
            Shelley.encodeCredential credential
        <> "deposit" .=
            encodeCoin deposit
        ]

encodeDelegatee
    :: Crypto crypto
    => Cn.Delegatee crypto
    -> Series
encodeDelegatee = \case
    Cn.DelegStake poolId ->
        "stakePool" .= encodeSingleton "id" (Shelley.encodePoolId poolId)
    Cn.DelegVote drep ->
        "delegateRepresentative" .= encodeSingleton "id" (Shelley.encodeCredential drep)
    Cn.DelegStakeVote poolId drep ->
        "stakePool" .= encodeSingleton "id" (Shelley.encodePoolId poolId)
        <>
        "delegateRepresentative" .= encodeSingleton "id" (Shelley.encodeCredential drep)

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
                encodeFoldable Shelley.encodeKeyHash members
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
        (encodeConcatFoldable encodeDCert) (Cn.ctbCerts x) <>
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

encodeVoterRole
    :: Cn.VoterRole
    -> Json
encodeVoterRole = encodeText . \case
    Cn.ConstitutionalCommittee ->
        "constitutionalCommittee"
    Cn.DRep ->
        "delegateRepresentative"
    Cn.SPO ->
        "stakePoolOperator"

encodeVotingProcedure
    :: Crypto crypto
    => Cn.VotingProcedure (ConwayEra crypto)
    -> Json
encodeVotingProcedure x = encodeObject
    ( "proposal" .=
        encodeGovernanceActionId (Cn.vProcGovActionId x)
   <> "issuer" .=
       encodeVoter (Cn.vProcRoleKeyHash x) (Cn.vProcRole x)
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

-- TODO: Think about making 'stakePool' a required field in the corresponding
-- JSON schema once bumping cardano-ledger to >= 1.6.0.0
encodeVoter
    :: Crypto crypto
    => Ledger.Credential 'Voting crypto
    -> Cn.VoterRole
    -> Json
encodeVoter credential = encodeObject . \case
    role@Cn.ConstitutionalCommittee ->
        "role" .= encodeVoterRole role <>
        "id" .= Shelley.encodeCredential credential
    role@Cn.DRep ->
        "role" .= encodeVoterRole role <>
        "id" .= Shelley.encodeCredential credential
    role@Cn.SPO ->
        "role" .= encodeVoterRole role
