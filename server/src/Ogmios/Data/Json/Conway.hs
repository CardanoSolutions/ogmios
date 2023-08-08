--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Conway where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Binary
    ( sizedValue
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

import qualified Ogmios.Data.Json.Babbage as Babbage
import qualified Ogmios.Data.Json.Shelley as Shelley


encodeBlock
    :: ( Crypto crypto
       )
    => ShelleyBlock (Praos crypto) (ConwayEra crypto)
    -> Json
encodeBlock (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "type" .= encodeText "praos"
        <>
          "era" .= encodeText "babbage"
        <>
          "id" .= Shelley.encodeShelleyHash headerHash
        <>
          Babbage.encodeHeader blkHeader
        <>
          "transactions" .= encodeFoldable encodeTx (Al.txSeqTxns txs)
        )

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

encodeGovernanceActionId
    :: Crypto crypto
    => Cn.GovernanceActionId crypto
    -> Json
encodeGovernanceActionId x =
    encodeObject
        ( "transaction" .=
            encodeObject (Shelley.encodeTxId (Cn.gaidTxId x))
        <>
          "governanceAction" .=
            encodeGovernanceActionIx (Cn.gaidGovActionIx x)
        )

encodeGovernanceActionIx
    :: Cn.GovernanceActionIx
    -> Json
encodeGovernanceActionIx (Cn.GovernanceActionIx ix) =
    encodeObject ("index" .= encodeWord64 ix)

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

-- TODO: Finish ConwayEra transaction model.
encodeTx
    :: forall crypto.
        ( Crypto crypto
        )
    => Ba.AlonzoTx (ConwayEra crypto)
    -> Json
encodeTx x =
    encodeObject
        ( Shelley.encodeTxId (Ledger.txid @(ConwayEra crypto) (Ba.body x))
        )

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
