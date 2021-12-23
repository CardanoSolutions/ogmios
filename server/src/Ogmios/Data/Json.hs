--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.Data.Json
    ( Json
    , SerializationMode (..)
    , ViaEncoding (..)
    , jsonToByteString
    , FromJSON
    , ToJSON
    , decodeWith
    , inefficientEncodingToValue

      -- * Encoders
    , encodeAcquireFailure
    , encodeBlock
    , encodePoint
    , encodeSubmitTxError
    , encodeSubmitTxPayload
    , encodeTip

      -- * Decoders
    , decodeOneEraHash
    , decodePoint
    , decodeSubmitTxPayload
    , decodeTip
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( FromCBOR (..), ToCBOR (..) )
import Cardano.Crypto.Hashing
    ( decodeHash, hashToBytes )
import Cardano.Ledger.Crypto
    ( Crypto )
import Cardano.Ledger.Shelley.API
    ( ApplyTxError (..), PraosCrypto )
import Cardano.Network.Protocol.NodeToClient
    ( SubmitTxError, SubmitTxPayload )
import Cardano.Slotting.Block
    ( BlockNo (..) )
import Cardano.Slotting.Slot
    ( SlotNo (..), WithOrigin (..) )
import Ogmios.Data.Json.Query
    ( encodeEraMismatch, encodeOneEraHash, encodePoint )
import Ouroboros.Consensus.Byron.Ledger.Block
    ( ByronBlock (..) )
import Ouroboros.Consensus.Byron.Ledger.Mempool
    ( encodeByronGenTx )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    , CardanoEras
    , GenTx (..)
    , HardForkApplyTxErr (..)
    , HardForkBlock (..)
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( OneEraHash (..) )
import Ouroboros.Consensus.Shelley.Eras
    ( MaryEra )
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyBlock )
import Ouroboros.Network.Block
    ( Point (..), Tip (..), genesisPoint, wrapCBORinCBOR )
import Ouroboros.Network.Point
    ( Block (..) )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure (..) )

import qualified Codec.CBOR.Encoding as Cbor
import qualified Codec.CBOR.Read as Cbor
import qualified Codec.CBOR.Write as Cbor

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Byron as Byron
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

--
-- Encoders
--

encodeAcquireFailure
    :: AcquireFailure
    -> Json
encodeAcquireFailure = \case
    AcquireFailurePointTooOld ->
        encodeText "pointTooOld"
    AcquireFailurePointNotOnChain ->
        encodeText "pointNotOnChain"

encodeBlock
    :: Crypto crypto
    => SerializationMode
    -> CardanoBlock crypto
    -> Json
encodeBlock mode = \case
    BlockByron blk -> encodeObject
        [ ( "byron"
          , Byron.encodeABlockOrBoundary mode (byronBlockRaw blk)
          )
        ]
    BlockShelley blk -> encodeObject
        [ ( "shelley"
          , Shelley.encodeBlock mode blk
          )
        ]
    BlockAllegra blk -> encodeObject
        [ ( "allegra"
          , Allegra.encodeBlock mode blk
          )
        ]
    BlockMary blk -> encodeObject
        [ ( "mary"
          , Mary.encodeBlock mode blk
          )
        ]
    BlockAlonzo blk -> encodeObject
        [ ( "alonzo"
          , Alonzo.encodeBlock mode blk
          )
        ]

encodeSubmitTxError
    :: Crypto crypto
    => SubmitTxError (CardanoBlock crypto)
    -> Json
encodeSubmitTxError = \case
    ApplyTxErrByron e ->
        Byron.encodeApplyMempoolPayloadErr e
    ApplyTxErrShelley (ApplyTxError xs) ->
        encodeList Shelley.encodeLedgerFailure xs
    ApplyTxErrAllegra (ApplyTxError xs) ->
        encodeList Allegra.encodeLedgerFailure xs
    ApplyTxErrMary (ApplyTxError xs) ->
        encodeList Mary.encodeLedgerFailure xs
    ApplyTxErrAlonzo (ApplyTxError xs) ->
        encodeList Alonzo.encodeLedgerFailure xs
    ApplyTxErrWrongEra e ->
        encodeList encodeEraMismatch [ e ]

encodeSubmitTxPayload
    :: PraosCrypto crypto
    => SubmitTxPayload (CardanoBlock crypto)
    -> Json
encodeSubmitTxPayload = \case
    GenTxByron tx ->
        encodeByteStringBase16 $ Cbor.toStrictByteString $ encodeByronGenTx tx
    GenTxShelley tx ->
        encodeByteStringBase16 $ Cbor.toStrictByteString $ toCBOR tx
    GenTxAllegra tx ->
        encodeByteStringBase16 $ Cbor.toStrictByteString $ toCBOR tx
    GenTxMary tx ->
        encodeByteStringBase16 $ Cbor.toStrictByteString $ toCBOR tx
    GenTxAlonzo tx ->
        encodeByteStringBase16 $ Cbor.toStrictByteString $ toCBOR tx

encodeTip
    :: Tip (CardanoBlock crypto)
    -> Json
encodeTip = \case
    TipGenesis -> encodeText "origin"
    Tip slot hash blockNo -> encodeObject
        [ ( "slot"
          , encodeSlotNo slot
          )
        , ( "hash"
          , encodeOneEraHash hash
          )
        , ( "blockNo"
          , encodeBlockNo blockNo
          )
        ]

--
-- Decoders
--

decodeOneEraHash
    :: Text
    -> Json.Parser (OneEraHash (CardanoEras crypto))
decodeOneEraHash =
    either (const mempty) (pure . OneEraHash . toShort . hashToBytes) . decodeHash

decodePoint
    :: Json.Value
    -> Json.Parser (Point (CardanoBlock crypto))
decodePoint json =
    parseOrigin json <|> parsePoint json
  where
    parseOrigin = Json.withText "Point" $ \case
        txt | txt == "origin" -> pure genesisPoint
        _ -> empty

    parsePoint = Json.withObject "Point" $ \obj -> do
        slot <- obj .: "slot"
        hash <- obj .: "hash" >>= decodeOneEraHash
        pure $ Point $ At $ Block (SlotNo slot) hash

decodeSubmitTxPayload
    :: forall crypto. PraosCrypto crypto
    => Json.Value
    -> Json.Parser (SubmitTxPayload (CardanoBlock crypto))
decodeSubmitTxPayload = Json.withText "Tx" $ \(encodeUtf8 -> utf8) -> do
    bytes <- decodeBase16 utf8 <|> decodeBase64 utf8
    asum $
        (deserialiseCBOR GenTxMary <$> [fromStrict bytes, wrap bytes])
        ++
        (deserialiseCBOR GenTxAlonzo <$> [fromStrict bytes, wrap bytes])
  where
    -- Cardano tools have a tendency to wrap cbor in cbor (e.g cardano-cli).
    -- In particular, a `GenTx` is expected to be prefixed with a cbor tag
    -- `24` and serialized as CBOR bytes `58xx`.
    wrap :: ByteString -> LByteString
    wrap = Cbor.toLazyByteString . wrapCBORinCBOR Cbor.encodePreEncoded

    deserialiseCBOR
        :: forall era.
            ( Or
                (era ~ LastElem (CardanoEras crypto))
                (era ~ ShelleyBlock (MaryEra crypto))
            , FromCBOR (GenTx era)
            )
        => (GenTx era -> GenTx (CardanoBlock crypto))
        -> LByteString
        -> Json.Parser (GenTx (CardanoBlock crypto))
    deserialiseCBOR mk =
        either (fail . show) (pure . mk . snd)
        .
        Cbor.deserialiseFromBytes fromCBOR
      where
        _compilerWarning = keepRedundantConstraint
            (Proxy @(Or
                (era ~ LastElem (CardanoEras crypto))
                (era ~ ShelleyBlock (MaryEra crypto))
            ))

decodeTip
    :: Json.Value
    -> Json.Parser (Tip (CardanoBlock crypto))
decodeTip json =
    parseOrigin json <|> parseTip json
  where
    parseOrigin = Json.withText "Tip" $ \case
        txt | txt == "origin" -> pure TipGenesis
        _ -> empty

    parseTip = Json.withObject "Tip" $ \obj -> do
        slot <- obj .: "slot"
        hash <- obj .: "hash" >>= decodeOneEraHash
        blockNo <- obj .: "blockNo"
        pure $ Tip (SlotNo slot) hash (BlockNo blockNo)
