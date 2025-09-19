--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Mary where

import Ogmios.Data.Json.Prelude

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
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger

import qualified Cardano.Ledger.Shelley.BlockChain as Sh
import qualified Cardano.Ledger.Shelley.Tx as Sh
import qualified Cardano.Ledger.Shelley.TxOut as Sh
import qualified Cardano.Ledger.Shelley.TxWits as Sh
import qualified Cardano.Ledger.Shelley.UTxO as Sh

import qualified Cardano.Ledger.Allegra.TxAuxData as Al

import qualified Cardano.Ledger.Mary.TxBody as Ma
import qualified Cardano.Ledger.Mary.Value as Ma

type AuxiliaryScripts =
    Map Ledger.ScriptHash (Ledger.Script MaryEra)

--
-- Encoders
--

encodeAuxiliaryData
    :: (MetadataFormat, IncludeCbor)
    -> Al.AllegraTxAuxData MaryEra
    -> (Json, AuxiliaryScripts)
encodeAuxiliaryData opts (Al.AllegraTxAuxData blob scripts) =
    ( Shelley.encodeMetadataBlob @MaryEra opts blob
    , foldr
        (\script -> Map.insert (Ledger.hashScript @MaryEra script) script)
        mempty
        scripts
    )

encodeBlock
    :: (MetadataFormat, IncludeCbor)
    -> ShelleyBlock (TPraos StandardCrypto) MaryEra
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
          "size" .= encodeSingleton "bytes" (encodeWord32 (TPraos.bsize hBody))
        <>
          "transactions" .= encodeFoldable (encodeTx opts) (Sh.txSeqTxns' txs)
        )
  where
    TPraos.BHeader hBody _ = blkHeader

encodeMultiAsset
    :: Ma.MultiAsset
    -> Series
encodeMultiAsset (Ma.MultiAsset assets) =
    encodeMapSeries
        stringifyPolicyId
        (const (encodeMap stringifyAssetName encodeInteger))
        assets

encodePolicyId
    :: Ma.PolicyID
    -> Json
encodePolicyId (Ma.PolicyID hash) =
    Shelley.encodeScriptHash hash

encodeTx
    :: (MetadataFormat, IncludeCbor)
    -> Sh.ShelleyTx MaryEra
    -> Json
encodeTx (fmt, opts) x =
    encodeObject
        ( Shelley.encodeTxId (Ledger.txIdTxBody @MaryEra (Sh.body x))
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
           "cbor" .= encodeByteStringBase16 (encodeCbor @MaryEra x)
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
    :: Ma.MaryTxBody MaryEra
    -> [Ledger.ScriptHash]
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
    :: Sh.ShelleyTxOut MaryEra
    -> Series
encodeTxOut (Sh.ShelleyTxOut addr value) =
    "address" .=
        Shelley.encodeAddress addr <>
    "value" .=
        encodeValue value

encodeUtxo
    :: Sh.UTxO MaryEra
    -> Json
encodeUtxo =
    encodeList id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO i o = encodeObject (Shelley.encodeTxIn i <> encodeTxOut o)

encodeValue
    :: Ma.MaryValue
    -> Json
encodeValue (Ma.MaryValue lovelace assets) =
    encodeObject
        ( "ada" .= encodeSingleton "lovelace" (encodeInteger (unCoin lovelace))
       <> encodeMultiAsset assets
        )

encodeWitnessSet
    :: IncludeCbor
    -> StrictMaybe AuxiliaryScripts
    -> Sh.ShelleyTxWits MaryEra
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

stringifyPolicyId :: Ma.PolicyID -> Text
stringifyPolicyId (Ma.PolicyID pid) =
    Shelley.stringifyScriptHash pid

stringifyAssetName :: Ma.AssetName -> Text
stringifyAssetName (Ma.AssetName bytes) =
    encodeBase16 (fromShort bytes)
