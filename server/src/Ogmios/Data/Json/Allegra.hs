--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Allegra where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( serialize'
    )
import Data.Maybe.Strict
    ( fromSMaybe
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )
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
    => Al.AllegraTxAuxData era
    -> (Json, AuxiliaryScripts crypto)
encodeAuxiliaryData (Al.AllegraTxAuxData blob scripts) =
    ( Shelley.encodeMetadataBlob @era blob
    , foldr
        (\script -> Map.insert (Ledger.hashScript @(AllegraEra crypto) script) script)
        mempty
        scripts
    )

encodeBlock
    :: Crypto crypto
    => ShelleyBlock (TPraos crypto) (AllegraEra crypto)
    -> Json
encodeBlock (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "type" .= encodeText "praos"
        <>
          "era" .= encodeText "allegra"
        <>
          "id" .= Shelley.encodeShelleyHash headerHash
        <>
          Shelley.encodeBHeader blkHeader
        <>
          "size" .= encodeSingleton "bytes" (encodeNatural (TPraos.bsize hBody))
        <>
          "transactions" .= encodeFoldable encodeTx (Sh.txSeqTxns' txs)
        )
  where
    TPraos.BHeader hBody _ = blkHeader

encodeScript
    :: Era era
    => Al.Timelock era
    -> Json
encodeScript = encodeObject . \case
    timelock ->
        "language" .=
            encodeText "native" <>
        "json" .=
            encodeTimelock timelock <>
        "cbor" .=
            encodeByteStringBase16 (Ledger.originalBytes timelock)

encodeTimelock
    :: Era era
    => Al.Timelock era
    -> Json
encodeTimelock = encodeObject . \case
    Al.RequireSignature sig ->
        "clause" .= encodeText "signature" <>
        "from" .= Shelley.encodeKeyHash sig
    Al.RequireAllOf xs ->
        "clause" .= encodeText "all" <>
        "from" .= encodeFoldable encodeTimelock xs
    Al.RequireAnyOf xs ->
        "clause" .= encodeText "any" <>
        "from" .= encodeFoldable encodeTimelock xs
    Al.RequireMOf n xs ->
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
    :: forall crypto. (Crypto crypto)
    => Sh.ShelleyTx (AllegraEra crypto)
    -> Json
encodeTx x =
    Shelley.encodeTxId (Ledger.txid @(AllegraEra crypto) (Sh.body x))
        <>
    "inputSource" .= encodeText "inputs"
        <>
    encodeTxBody (Sh.body x)
        <>
    "metadata" .=? OmitWhenNothing fst auxiliary
        <>
    encodeWitnessSet (snd <$> auxiliary) (Sh.wits x)
        <>
    "cbor" .= encodeByteStringBase16 (serialize' x)
        & encodeObject
  where
    auxiliary = do
        hash <- Shelley.encodeAuxiliaryDataHash <$> Al.atbAuxDataHash (Sh.body x)
        (labels, scripts) <- encodeAuxiliaryData <$> Sh.auxiliaryData x
        pure
            ( encodeObject ("hash" .= hash <> "labels" .= labels)
            , scripts
            )

encodeTxBody
    :: Crypto crypto
    => Al.AllegraTxBody (AllegraEra crypto)
    -> Series
encodeTxBody (Al.AllegraTxBody inps outs certs wdrls fee validity updates _) =
    "inputs" .=
        encodeFoldable Shelley.encodeTxIn inps <>
    "outputs" .=
        encodeFoldable Shelley.encodeTxOut outs <>
    "withdrawals" .=? OmitWhen (null . Ledger.unWithdrawals)
        Shelley.encodeWdrl wdrls <>
    "certificates" .=? OmitWhen null
        (encodeFoldable Shelley.encodeDCert) certs <>
    "fee" .=
        encodeCoin fee <>
    "validityInterval" .=
        encodeValidityInterval validity <>
    "governanceActions" .=? OmitWhenNothing
        (Shelley.encodeUpdate Shelley.encodePParamsUpdate)
        updates

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
    => StrictMaybe (AuxiliaryScripts crypto)
    -> Sh.ShelleyTxWits (AllegraEra crypto)
    -> Series
encodeWitnessSet (fromSMaybe mempty -> auxScripts) x =
    "signatories" .=
        encodeFoldable2
            Shelley.encodeBootstrapWitness
            Shelley.encodeWitVKey
            (Sh.bootWits x)
            (Sh.addrWits x) <>
    "scripts" .=? OmitWhen null
        (encodeMap Shelley.stringifyScriptHash encodeScript)
        (Sh.scriptWits x <> auxScripts)
