--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- | Dijkstra is a new era following Conway (protocol version 12).
-- The ledger types share the same CBOR format as Conway's but are nominally
-- distinct (different data family instances). We bridge via CBOR serialization
-- for safe type conversion.
module Ogmios.Data.Json.Dijkstra where

import Ogmios.Data.Json.Prelude

import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )
import Ouroboros.Consensus.Shelley.Protocol.Praos
    ()

import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Core as Ledger.Core
import qualified Cardano.Ledger.Binary as Binary
import qualified Cardano.Ledger.Block as Ledger

import qualified Ogmios.Data.Json.Babbage as Babbage
import qualified Ogmios.Data.Json.Conway as Conway
import qualified Ogmios.Data.Json.Shelley as Shelley

encodeBlock
    :: (MetadataFormat, IncludeCbor)
    -> ShelleyBlock (Praos StandardCrypto) DijkstraEra
    -> Json
encodeBlock opts (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "type" .= encodeText "praos"
        <>
          "era" .= encodeText "dijkstra"
        <>
          "id" .= Shelley.encodeShelleyHash headerHash
        <>
          Babbage.encodeHeader blkHeader
        <>
          "transactions" .= encodeFoldable (encodeTx opts) (txs ^. Ledger.Core.txSeqBlockBodyL)
        )

-- | Encode a Dijkstra transaction by converting to Conway via CBOR roundtrip.
-- Dijkstra and Conway share the same CBOR wire format, so this is safe.
encodeTx
    :: (MetadataFormat, IncludeCbor)
    -> Ledger.Tx Ledger.Core.TopTx DijkstraEra
    -> Json
encodeTx opts x =
    case dijkstraToConwayTx x of
        Left err -> error $ "encodeTx/Dijkstra: CBOR roundtrip failed: " <> show err
        Right conwayTx -> Conway.encodeTx opts conwayTx

dijkstraToConwayTx
    :: Ledger.Tx Ledger.Core.TopTx DijkstraEra
    -> Either Binary.DecoderError (Ledger.Tx Ledger.Core.TopTx ConwayEra)
dijkstraToConwayTx tx =
    let bytes = Binary.serialize (Ledger.Core.eraProtVerHigh @DijkstraEra) tx
     in Binary.decodeFullAnnotator
            (Ledger.Core.eraProtVerHigh @ConwayEra)
            "Tx"
            Binary.decCBOR
            bytes
