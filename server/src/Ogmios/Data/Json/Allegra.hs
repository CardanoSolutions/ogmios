--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
import qualified Cardano.Ledger.SafeHash as Ledger

import qualified Cardano.Ledger.Shelley.BlockChain as Sh
import qualified Cardano.Ledger.Shelley.Scripts as Sh
import qualified Cardano.Ledger.Shelley.Tx as Sh
import qualified Cardano.Ledger.Shelley.TxWits as Sh
import qualified Cardano.Ledger.Shelley.UTxO as Sh

import qualified Cardano.Ledger.Allegra.Scripts as Al
import qualified Cardano.Ledger.Allegra.TxAuxData as Al
import qualified Cardano.Ledger.Allegra.TxBody as Al

import qualified Ogmios.Data.Json.Shelley as Shelley

type AuxiliaryScripts crypto =
    Map (Ledger.ScriptHash crypto) (Ledger.Script (AllegraEra crypto))

--
-- Encoders
--
encodeAuxiliaryData
    :: forall crypto era. (Era era, era ~ AllegraEra crypto)
    => (MetadataFormat, IncludeCbor)
    -> Al.AllegraTxAuxData era
    -> (Json, AuxiliaryScripts crypto)
encodeAuxiliaryData opts (Al.AllegraTxAuxData blob scripts) =
    ( Shelley.encodeMetadataBlob @era opts blob
    , foldr
        (\script -> Map.insert (Ledger.hashScript @(AllegraEra crypto) script) script)
        mempty
        scripts
    )

encodeBlock
    :: Crypto crypto
    => (MetadataFormat, IncludeCbor)
    -> ShelleyBlock (TPraos crypto) (AllegraEra crypto)
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
          "transactions" .= encodeFoldable (encodeTx opts) (Sh.txSeqTxns' txs)
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
    :: forall era crypto.
        ( Crypto crypto
        , era ~ AllegraEra crypto
        )
    => (MetadataFormat, IncludeCbor)
    -> Sh.ShelleyTx era
    -> Json
encodeTx (fmt, opts) x =
    encodeObject
        ( Shelley.encodeTxId (Ledger.txIdTxBody @(AllegraEra crypto) (Sh.body x))
       <>
        "spends" .= encodeText "inputs"
       <>
        encodeTxBody (Sh.body x) (strictMaybe mempty (Map.keys . snd) auxiliary)
       <>
        "metadata" .=? OmitWhenNothing fst auxiliary
       <>
        encodeWitnessSet opts (snd <$> auxiliary) (Sh.wits x)
       <>
        if includeTransactionCbor opts then
           "cbor" .= encodeByteStringBase16 (encodeCbor @era x)
        else
           mempty
       )
  where
    auxiliary = do
        hash <- Shelley.encodeAuxiliaryDataHash <$> Al.atbAuxDataHash (Sh.body x)
        (labels, scripts) <- encodeAuxiliaryData (fmt, opts) <$> Sh.auxiliaryData x
        pure
            ( encodeObject ("hash" .= hash <> "labels" .= labels)
            , scripts
            )

encodeTxBody
    :: Crypto crypto
    => Al.AllegraTxBody (AllegraEra crypto)
    -> [Ledger.ScriptHash crypto]
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
    :: Crypto crypto
    => Sh.UTxO (AllegraEra crypto)
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
    :: Crypto crypto
    => IncludeCbor
    -> StrictMaybe (AuxiliaryScripts crypto)
    -> Sh.ShelleyTxWits (AllegraEra crypto)
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
