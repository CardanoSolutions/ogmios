--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- TODO(dijkstra): warnings disabled while encodeTx is stubbed (needs lens rewrite).
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches -Wno-unused-top-binds -Wno-incomplete-patterns #-}

module Ogmios.Data.Json.Allegra where

import Ogmios.Data.Json.Prelude

import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()

import qualified Data.Map as Map

import qualified Cardano.Protocol.TPraos.BHeader as TPraos

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger

import qualified Cardano.Ledger.Shelley.BlockBody as Sh
import qualified Cardano.Ledger.Shelley.Scripts as Sh
import qualified Cardano.Ledger.Shelley.Tx as Sh
import qualified Cardano.Ledger.Shelley.TxWits as Sh
import qualified Cardano.Ledger.Shelley.UTxO as Sh

import qualified Cardano.Ledger.Allegra.Scripts as Al
import qualified Cardano.Ledger.Allegra.TxAuxData as Al
import qualified Cardano.Ledger.Allegra.TxBody as Al

import qualified Ogmios.Data.Json.Shelley as Shelley

type AuxiliaryScripts =
    Map Ledger.ScriptHash (Ledger.Script AllegraEra)

--
-- Encoders
--
encodeAuxiliaryData
    :: (MetadataFormat, IncludeCbor)
    -> Al.AllegraTxAuxData AllegraEra
    -> (Json, AuxiliaryScripts)
encodeAuxiliaryData opts (Al.AllegraTxAuxData blob scripts) =
    ( Shelley.encodeMetadataBlob @AllegraEra opts blob
    , foldr
        (\script -> Map.insert (Ledger.hashScript @AllegraEra script) script)
        mempty
        scripts
    )

encodeBlock
    :: (MetadataFormat, IncludeCbor)
    -> ShelleyBlock (TPraos StandardCrypto) AllegraEra
    -> Json
encodeBlock opts (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "type" .= encodeText "praos"
        <>
          "era" .= encodeText "allegra"
        <>
          "id" .= Shelley.encodeShelleyHash headerHash
        <>
          Shelley.encodeBHeader blkHeader
        <>
          "size" .= encodeSingleton "bytes" (encodeWord32 (TPraos.bsize hBody))
        <>
          "transactions" .= encodeFoldable (encodeTx opts) (Sh.shelleyBlockBodyTxs txs)
        )
  where
    TPraos.BHeader hBody _ = blkHeader

encodeScript
    :: ( Al.AllegraEraScript era
       , Ledger.NativeScript era ~ Al.Timelock era
       )
    => IncludeCbor
    -> Al.Timelock era
    -> Json
encodeScript opts = encodeObject . \case
    timelock ->
        "language" .=
            encodeText "native" <>
        "json" .=
            encodeTimelock timelock <>
        if includeScriptCbor opts then
            "cbor" .=
                encodeByteStringBase16 (Ledger.originalBytes timelock)
        else
            mempty

encodeTimelock
    :: ( Al.AllegraEraScript era
       , Ledger.NativeScript era ~ Al.Timelock era
       )
    => Al.Timelock era
    -> Json
encodeTimelock = encodeObject . \case
    Sh.RequireSignature sig ->
        "clause" .= encodeText "signature" <>
        "from" .= Shelley.encodeKeyHash sig
    Sh.RequireAllOf xs ->
        "clause" .= encodeText "all" <>
        "from" .= encodeFoldable encodeTimelock xs
    Sh.RequireAnyOf xs ->
        "clause" .= encodeText "any" <>
        "from" .= encodeFoldable encodeTimelock xs
    Sh.RequireMOf n xs ->
        "clause" .= encodeText "some" <>
        "atLeast" .= encodeInteger (toInteger n) <>
        "from" .= encodeFoldable encodeTimelock xs
    Al.RequireTimeExpire s ->
        "clause" .= encodeText "before" <>
        "slot" .= encodeSlotNo s
    Al.RequireTimeStart s ->
        "clause" .= encodeText "after" <>
        "slot" .= encodeSlotNo s

encodeTx
    :: (MetadataFormat, IncludeCbor)
    -> Sh.Tx Ledger.TopTx AllegraEra
    -> Json
encodeTx _ _ =
    -- TODO(dijkstra): rewrite using lenses (bodyTxL/witsTxL/auxDataTxL) instead of Sh.MkShelleyTx pattern. The `Sh.MkShelleyTx` syn doesn't unify with non-Shelley eras in cardano-ledger 1.20. PR #461 uses `x ^. Ledger.bodyTxL` etc.
    error "TODO(dijkstra): encodeTx Allegra"

encodeTxBody
    :: Ledger.TxBody Ledger.TopTx AllegraEra
    -> [Ledger.ScriptHash]
    -> Series
encodeTxBody (Al.AllegraTxBody inps outs dCerts wdrls fee validity updates _) requiredScripts =
    "inputs" .=
        encodeFoldable (encodeObject . Shelley.encodeTxIn) inps <>
    "outputs" .=
        encodeFoldable (encodeObject . Shelley.encodeTxOut) outs <>
    "withdrawals" .=? OmitWhen (null . Ledger.unWithdrawals)
        Shelley.encodeWdrl wdrls <>
    "certificates" .=? OmitWhen null
        (encodeList encodeObject) certs <>
    "requiredExtraScripts" .=? OmitWhen null
        (encodeFoldable Shelley.encodeScriptHash) requiredScripts <>
    "fee" .=
        encodeCoin fee <>
    "validityInterval" .=
        encodeValidityInterval validity <>
    "proposals" .=? OmitWhen null
        (encodeList (encodeSingleton "action")) actions <>
    "votes" .=? OmitWhen null
        (encodeList Shelley.encodeGenesisVote) votes
  where
    (certs, mirs) =
        Shelley.encodeTxCerts dCerts

    (votes, actions) = fromSMaybe ([], mirs) $
        Shelley.encodeUpdate Shelley.encodePParamsUpdate mirs <$> updates

encodeUtxo
    :: Sh.UTxO AllegraEra
    -> Json
encodeUtxo =
    Shelley.encodeUtxo

encodeValidityInterval
    :: Al.ValidityInterval
    -> Json
encodeValidityInterval x =
    "invalidBefore" .=? OmitWhenNothing
        encodeSlotNo (Al.invalidBefore x) <>
    "invalidAfter" .=? OmitWhenNothing
        encodeSlotNo (Al.invalidHereafter x)
    & encodeObject

encodeWitnessSet
    :: IncludeCbor
    -> StrictMaybe AuxiliaryScripts
    -> Sh.ShelleyTxWits AllegraEra
    -> Series
encodeWitnessSet opts (fromSMaybe mempty -> auxScripts) x =
    "signatories" .=
        encodeFoldable2
            Shelley.encodeBootstrapWitness
            Shelley.encodeWitVKey
            (Sh.bootWits x)
            (Sh.addrWits x) <>
    "scripts" .=? OmitWhen null
        (encodeMap Shelley.stringifyScriptHash (encodeScript opts))
        (Sh.scriptWits x <> auxScripts)
