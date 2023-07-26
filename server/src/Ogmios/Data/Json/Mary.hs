--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Mary where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( serialize'
    )
import Data.ByteString.Base16
    ( encodeBase16
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

import qualified Data.ByteString.Short as BS
import qualified Data.Map.Strict as Map

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Shelley as Shelley

import qualified Cardano.Protocol.TPraos.BHeader as TPraos

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger

import qualified Cardano.Ledger.Shelley.BlockChain as Sh
import qualified Cardano.Ledger.Shelley.PParams as Sh
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
    :: forall crypto. Crypto crypto
    => Al.AllegraTxAuxData (MaryEra crypto)
    -> (Json, AuxiliaryScripts crypto)
encodeAuxiliaryData (Al.AllegraTxAuxData blob scripts) =
    ( Shelley.encodeMetadataBlob blob
    , foldr
        (\script -> Map.insert (Ledger.hashScript @(MaryEra crypto) script) script)
        mempty
        scripts
    )

encodeBlock
    :: ( Crypto crypto
       )
    => ShelleyBlock (TPraos crypto) (MaryEra crypto)
    -> Json
encodeBlock (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "era" .= encodeText "mary"
        <>
          "header" .= encodeObject
            ( "hash" .= Shelley.encodeShelleyHash headerHash
            )
        <>
        Shelley.encodeBHeader blkHeader
        <>
        "size" .= encodeNatural (TPraos.bsize hBody)
        <>
        "transactions" .= encodeFoldable encodeTx (Sh.txSeqTxns' txs)
        )
  where
    TPraos.BHeader hBody _ = blkHeader

encodeMultiAsset
    :: Crypto crypto
    => Ma.MultiAsset crypto
    -> Json
encodeMultiAsset (Ma.MultiAsset assets) =
    encodeMap stringifyAssetId encodeInteger (flatten assets)
  where
    flatten :: (Ord k1, Ord k2) => Map k1 (Map k2 a) -> Map (k1, k2) a
    flatten = Map.foldrWithKey
        (\k inner -> Map.union (Map.mapKeys (k,) inner))
        mempty

encodePolicyId
    :: Crypto crypto
    => Ma.PolicyID crypto
    -> Json
encodePolicyId (Ma.PolicyID hash) =
    Shelley.encodeScriptHash hash

encodeProposedPPUpdates
    :: forall era.
        ( Era era
        , Ledger.PParamsHKD StrictMaybe era ~ Sh.ShelleyPParams StrictMaybe era
        )
    => Sh.ProposedPPUpdates era
    -> Json
encodeProposedPPUpdates =
    Shelley.encodeProposedPPUpdates

encodeTx
    :: forall crypto.
       ( Crypto crypto
       )
    => Sh.ShelleyTx (MaryEra crypto)
    -> Json
encodeTx x =
    "id" .= Shelley.encodeTxId (Ledger.txid @(MaryEra crypto) (Sh.body x))
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
        hash <- Shelley.encodeAuxiliaryDataHash <$> Ma.mtbAuxDataHash (Sh.body x)
        (labels, scripts) <- encodeAuxiliaryData <$> Sh.auxiliaryData x
        pure
            ( encodeObject ("hash" .= hash <> "labels" .= labels)
            , scripts
            )

encodeTxBody
    :: Crypto crypto
    => Ma.MaryTxBody (MaryEra crypto)
    -> Series
encodeTxBody (Ma.MaryTxBody inps outs certs wdrls fee validity updates _ mint) =
    "inputs" .=
        encodeFoldable Shelley.encodeTxIn inps <>
    "outputs" .=
        encodeFoldable encodeTxOut outs <>
    "withdrawals" .=? OmitWhen (null . Ledger.unWithdrawals)
        Shelley.encodeWdrl wdrls <>
    "certificates" .=? OmitWhen null
        (encodeFoldable Shelley.encodeDCert) certs <>
    "mint" .=? OmitWhen (== mempty)
        encodeMultiAsset mint <>
    "fee" .=
        encodeCoin fee <>
    "validityInterval" .=
        Allegra.encodeValidityInterval validity <>
    "governanceActions" .=? OmitWhenNothing
        (encodeFoldable identity . pure @[] . Shelley.encodeUpdate)
        updates

encodeTxOut
    :: Crypto crypto
    => Sh.TxOut (MaryEra crypto)
    -> Json
encodeTxOut (Sh.ShelleyTxOut addr value) =
    "address" .=
        Shelley.encodeAddress addr <>
    "value" .=
        encodeValue value
    & encodeObject

encodeUtxo
    :: Crypto crypto
    => Sh.UTxO (MaryEra crypto)
    -> Json
encodeUtxo =
    encodeList id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO = curry (encode2Tuple Shelley.encodeTxIn encodeTxOut)

encodeValue
    :: Crypto crypto
    => Ma.MaryValue crypto
    -> Json
encodeValue (Ma.MaryValue coins assets) =
    "coins" .=
        encodeInteger coins <>
    "assets" .=
        encodeMultiAsset assets
    & encodeObject

encodeWitnessSet
    :: Crypto crypto
    => StrictMaybe (AuxiliaryScripts crypto)
    -> Sh.ShelleyTxWits (MaryEra crypto)
    -> Series
encodeWitnessSet (fromSMaybe mempty -> auxScripts) x =
    "signatories" .=
        encodeFoldable2
            Shelley.encodeBootstrapWitness
            Shelley.encodeWitVKey
            (Sh.bootWits x)
            (Sh.addrWits x) <>
    "scripts" .=? OmitWhen null
        (encodeMap Shelley.stringifyScriptHash Allegra.encodeScript)
        (Sh.scriptWits x <> auxScripts)

--
-- Conversion To Text
--

stringifyAssetId :: Crypto crypto => (Ma.PolicyID crypto, Ma.AssetName) -> Text
stringifyAssetId (Ma.PolicyID pid, Ma.AssetName bytes)
    | BS.null bytes = Shelley.stringifyScriptHash pid
    | otherwise     = Shelley.stringifyScriptHash pid <> "." <> encodeBase16 (fromShort bytes)
