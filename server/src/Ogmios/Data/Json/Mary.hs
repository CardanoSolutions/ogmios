--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Mary where

import Ogmios.Data.Json.Prelude

import Data.Maybe.Strict
    ( fromSMaybe
    , strictMaybe
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()

import qualified Data.Map.Strict as Map

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Shelley as Shelley

import qualified Cardano.Protocol.TPraos.BHeader as TPraos

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Binary as Binary
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger

import qualified Cardano.Ledger.Shelley.BlockChain as Sh
import qualified Cardano.Ledger.Shelley.Tx as Sh
import qualified Cardano.Ledger.Shelley.TxBody as Sh
import qualified Cardano.Ledger.Shelley.TxWits as Sh
import qualified Cardano.Ledger.Shelley.UTxO as Sh

import qualified Cardano.Ledger.Allegra.TxAuxData as Al

import qualified Cardano.Ledger.Mary.TxBody as Ma
import qualified Cardano.Ledger.Mary.Value as Ma

type AuxiliaryScripts crypto =
    Map (Ledger.ScriptHash crypto) (Ledger.Script (MaryEra crypto))

--
-- Encoders
--

encodeAuxiliaryData
    :: forall crypto era. (Era era, era ~ MaryEra crypto)
    => (MetadataFormat, IncludeCbor)
    -> Al.AllegraTxAuxData era
    -> (Json, AuxiliaryScripts crypto)
encodeAuxiliaryData opts (Al.AllegraTxAuxData blob scripts) =
    ( Shelley.encodeMetadataBlob @era opts blob
    , foldr
        (\script -> Map.insert (Ledger.hashScript @(MaryEra crypto) script) script)
        mempty
        scripts
    )

encodeBlock
    :: ( Crypto crypto
       )
    => (MetadataFormat, IncludeCbor)
    -> ShelleyBlock (TPraos crypto) (MaryEra crypto)
    -> Json
encodeBlock opts (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "type" .= encodeText "praos"
        <>
          "era" .= encodeText "mary"
        <>
          "id" .= Shelley.encodeShelleyHash headerHash
        <>
          Shelley.encodeBHeader blkHeader
        <>
          "size" .= encodeSingleton "bytes" (encodeNatural (TPraos.bsize hBody))
        <>
          "transactions" .= encodeFoldable (encodeTx opts) (Sh.txSeqTxns' txs)
        )
  where
    TPraos.BHeader hBody _ = blkHeader

encodeMultiAsset
    :: Crypto crypto
    => Ma.MultiAsset crypto
    -> Series
encodeMultiAsset (Ma.MultiAsset assets) =
    encodeMapSeries
        stringifyPolicyId
        (const (encodeMap stringifyAssetName encodeInteger))
        assets

encodePolicyId
    :: Crypto crypto
    => Ma.PolicyID crypto
    -> Json
encodePolicyId (Ma.PolicyID hash) =
    Shelley.encodeScriptHash hash

encodeTx
    :: forall era crypto.
       ( Crypto crypto
       , era ~ MaryEra crypto
       )
    => (MetadataFormat, IncludeCbor)
    -> Sh.ShelleyTx era
    -> Json
encodeTx (fmt, opts) x =
    encodeObject
        ( Shelley.encodeTxId (Ledger.txid @(MaryEra crypto) (Sh.body x))
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
           "cbor" .= encodeByteStringBase16 (Binary.serialize' (Ledger.eraProtVerLow @era) x)
        else
           mempty
       )
  where
    auxiliary = do
        hash <- Shelley.encodeAuxiliaryDataHash <$> Ma.mtbAuxDataHash (Sh.body x)
        (labels, scripts) <- encodeAuxiliaryData (fmt, opts) <$> Sh.auxiliaryData x
        pure
            ( encodeObject ("hash" .= hash <> "labels" .= labels)
            , scripts
            )

encodeTxBody
    :: Crypto crypto
    => Ma.MaryTxBody (MaryEra crypto)
    -> [Ledger.ScriptHash crypto]
    -> Series
encodeTxBody (Ma.MaryTxBody inps outs dCerts wdrls fee validity updates _ mint) scripts =
    "inputs" .=
        encodeFoldable (encodeObject . Shelley.encodeTxIn) inps <>
    "outputs" .=
        encodeFoldable (encodeObject . encodeTxOut) outs <>
    "withdrawals" .=? OmitWhen (null . Ledger.unWithdrawals)
        Shelley.encodeWdrl wdrls <>
    "certificates" .=? OmitWhen null
        (encodeList encodeObject) certs <>
    "requiredExtraScripts" .=? OmitWhen null
        (encodeFoldable Shelley.encodeScriptHash) scripts <>
    "mint" .=? OmitWhen (== mempty)
        (encodeObject . encodeMultiAsset) mint <>
    "fee" .=
        encodeCoin fee <>
    "validityInterval" .=
        Allegra.encodeValidityInterval validity <>
    "proposals" .=? OmitWhen null
        (encodeList (encodeSingleton "action")) actions <>
    "votes" .=? OmitWhen null
        (encodeList Shelley.encodeGenesisVote) votes
  where
    (certs, mirs) =
        Shelley.encodeTxCerts dCerts

    (votes, actions) = fromSMaybe ([], mirs) $
        Shelley.encodeUpdate Shelley.encodePParamsUpdate mirs <$> updates

encodeTxOut
    :: Crypto crypto
    => Sh.TxOut (MaryEra crypto)
    -> Series
encodeTxOut (Sh.ShelleyTxOut addr value) =
    "address" .=
        Shelley.encodeAddress addr <>
    "value" .=
        encodeValue value

encodeUtxo
    :: Crypto crypto
    => Sh.UTxO (MaryEra crypto)
    -> Json
encodeUtxo =
    encodeList id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO i o = encodeObject (Shelley.encodeTxIn i <> encodeTxOut o)

encodeValue
    :: Crypto crypto
    => Ma.MaryValue crypto
    -> Json
encodeValue (Ma.MaryValue lovelace assets) =
    encodeObject
        ( "ada" .= encodeSingleton "lovelace" (encodeInteger (unCoin lovelace))
       <> encodeMultiAsset assets
        )

encodeWitnessSet
    :: Crypto crypto
    => IncludeCbor
    -> StrictMaybe (AuxiliaryScripts crypto)
    -> Sh.ShelleyTxWits (MaryEra crypto)
    -> Series
encodeWitnessSet opts (fromSMaybe mempty -> auxScripts) x =
    "signatories" .=
        encodeFoldable2
            Shelley.encodeBootstrapWitness
            Shelley.encodeWitVKey
            (Sh.bootWits x)
            (Sh.addrWits x) <>
    "scripts" .=? OmitWhen null
        (encodeMap Shelley.stringifyScriptHash (Allegra.encodeScript opts))
        (Sh.scriptWits x <> auxScripts)

--
-- Conversion To Text
--

stringifyPolicyId :: Crypto crypto => Ma.PolicyID crypto -> Text
stringifyPolicyId (Ma.PolicyID pid) =
    Shelley.stringifyScriptHash pid

stringifyAssetName :: Ma.AssetName -> Text
stringifyAssetName (Ma.AssetName bytes) =
    encodeBase16 (fromShort bytes)
