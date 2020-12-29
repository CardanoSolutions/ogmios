--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.Data.Json
    ( Json
    , jsonToByteString
    , FromJSON
    , decodeWith

      -- * Encoders
    , encodeBlock
    , encodePoint
    , encodeTip
    , encodeAcquireFailure
    , encodeHardForkApplyTxErr

      -- * Parsers
    , SomeQuery (..)
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( FromCBOR (..) )
import Cardano.Crypto.Hashing
    ( decodeHash, hashToBytes )
import Cardano.Ledger.Crypto
    ( Crypto )
import Cardano.Ledger.Era
    ( Era )
import Cardano.Slotting.Block
    ( BlockNo (..) )
import Cardano.Slotting.Slot
    ( EpochNo (..), SlotNo (..), WithOrigin (..) )
import Data.Aeson
    ( (.:) )
import Data.ByteString.Base16
    ( decodeBase16 )
import Data.ByteString.Base64
    ( decodeBase64 )
import Data.ByteString.Short
    ( toShort )
import Data.Maybe
    ( fromJust )
import Ouroboros.Consensus.Byron.Ledger.Block
    ( ByronBlock (..) )
import Ouroboros.Consensus.Cardano.Block
    ( AllegraEra
    , CardanoBlock
    , CardanoEras
    , CardanoQueryResult
    , GenTx (..)
    , HardForkApplyTxErr (..)
    , HardForkBlock (..)
    , Query (..)
    , ShelleyEra
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( MismatchEraInfo (..), OneEraHash (..) )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( EraMismatch (..), mkEraMismatch )
import Ouroboros.Network.Block
    ( Point (..), Tip (..), genesisPoint, wrapCBORinCBOR )
import Ouroboros.Network.Point
    ( Block (..), WithOrigin (..) )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure (..) )
import Shelley.Spec.Ledger.API
    ( PraosCrypto )

import qualified Cardano.Ledger.Era as Era
import qualified Codec.CBOR.Encoding as Cbor
import qualified Codec.CBOR.Read as Cbor
import qualified Codec.CBOR.Write as Cbor
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Ogmios.Data.Json.Byron as Byron
import qualified Ogmios.Data.Json.MaryAllegra as MaryAllegra
import qualified Ogmios.Data.Json.Shelley as Shelley

--
-- Orphans
--

instance Json.ToJSON (Tip (CardanoBlock crypto)) where
    toJSON = fromJust . Json.decodeStrict . jsonToByteString . encodeTip

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

encodeMismatchEraInfo
    :: MismatchEraInfo (CardanoEras crypto)
    -> Json
encodeMismatchEraInfo =
    encodeEraMismatch . mkEraMismatch

encodeBlock
    :: Crypto crypto
    => CardanoBlock crypto
    -> Json
encodeBlock = \case
    BlockByron blk -> encodeObject
        [ ("byron"
          , Byron.encodeABlockOrBoundary (byronBlockRaw blk)
          )
        ]
    BlockShelley _blk -> undefined
    BlockAllegra _blk -> undefined
    BlockMary _blk -> undefined

encodeBlockNo
    :: BlockNo
    -> Json
encodeBlockNo =
    encodeWord64 . unBlockNo

encodeHardForkApplyTxErr
    :: Crypto crypto
    => HardForkApplyTxErr (CardanoEras crypto)
    -> Json
encodeHardForkApplyTxErr = \case
    ApplyTxErrByron e ->
        Byron.encodeApplyMempoolPayloadErr e
    ApplyTxErrShelley _e ->
        undefined
    ApplyTxErrAllegra _e ->
        undefined
    ApplyTxErrMary _e ->
        undefined
    ApplyTxErrWrongEra e ->
        encodeEraMismatch e

encodeEraMismatch
    :: EraMismatch
    -> Json
encodeEraMismatch x = encodeObject
    [ ( "eraMismatch", encodeObject
        [ ( "ledgerEra"
          , encodeText (ledgerEraName x)
          )
        , ( "queryEra"
          , encodeText (otherEraName x)
          )
        ]
      )
    ]

encodeOneEraHash
    :: OneEraHash eras
    -> Json
encodeOneEraHash =
    encodeShortByteString encodeByteStringBase16 . getOneEraHash

encodePoint
    :: Point (CardanoBlock crypto)
    -> Json
encodePoint = \case
    Point Origin -> encodeText "origin"
    Point (At x) -> encodeObject
        [ ( "slot"
          , encodeSlotNo (blockPointSlot x)
          )
        , ( "hash"
          , encodeOneEraHash (blockPointHash x)
          )
        ]

encodeSlotNo
    :: SlotNo
    -> Json
encodeSlotNo =
    encodeWord64 . unSlotNo

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
-- Parsers
--

instance PraosCrypto crypto => FromJSON (GenTx (CardanoBlock crypto))
  where
    parseJSON = Json.withText "Tx" $ \(encodeUtf8 -> utf8) -> do
        bytes <- parseBase16 utf8 <|> parseBase64 utf8
        deserialiseCBOR (fromStrict bytes) <|> deserialiseCBOR (wrap bytes)
      where
        deserialiseCBOR =
            either (fail . show) (pure . GenTxShelley . snd)
            .
            Cbor.deserialiseFromBytes fromCBOR

        -- Cardano tools have a tendency to wrap cbor in cbor (e.g cardano-cli).
        -- In particular, a `GenTx` is expected to be prefixed with a cbor tag
        -- `24` and serialized as CBOR bytes `58xx`.
        wrap = Cbor.toLazyByteString . wrapCBORinCBOR Cbor.encodePreEncoded

        parseBase16 :: ByteString -> Json.Parser ByteString
        parseBase16 = either (fail . toString) pure . decodeBase16

        parseBase64 :: ByteString -> Json.Parser ByteString
        parseBase64 = either (fail . toString) pure . decodeBase64

instance Crypto crypto => FromJSON (Point (CardanoBlock crypto)) where
    parseJSON json = parseOrigin json <|> parsePoint json
      where
        parseOrigin = Json.withText "Point" $ \case
            txt | txt == "origin" -> pure genesisPoint
            _ -> empty

        parsePoint = Json.withObject "Point" $ \obj -> do
            slot <- obj .: "slot"
            hash <- obj .: "hash" >>= decodeOneEraHash
            pure $ Point $ At $ Block (SlotNo slot) hash

instance Crypto crypto => FromJSON (SomeQuery Maybe (CardanoBlock crypto)) where
    parseJSON = choice "query"
        [ Shelley.parseGetLedgerTip _void encodeMismatchEraInfo encodePoint
        -- , parseGetEpochNo _void
        -- , parseGetNonMyopicMemberRewards _void
        -- , parseGetCurrentPParams _void
        -- , parseGetProposedPParamsUpdates _void
        -- , parseGetStakeDistribution _void
        -- , parseGetUTxO _void
        -- , parseGetFilteredUTxO _void
        ]
      where
        _void :: forall result. Proxy result -> Maybe result
        _void = const Nothing

instance Crypto crypto => FromJSON (Tip (CardanoBlock crypto)) where
    parseJSON json = parseOrigin json <|> parseTip json
      where
        parseOrigin = Json.withText "Tip" $ \case
            txt | txt == "origin" -> pure TipGenesis
            _ -> empty

        parseTip = Json.withObject "Tip" $ \obj -> do
            slot <- obj .: "slot"
            hash <- obj .: "hash" >>= decodeOneEraHash
            blockNo <- obj .: "blockNo"
            pure $ Tip (SlotNo slot) hash (BlockNo blockNo)

decodeOneEraHash
    :: Text
    -> Json.Parser (OneEraHash (CardanoEras crypto))
decodeOneEraHash =
    either (const mempty) (pure . OneEraHash . toShort . hashToBytes) . decodeHash

--
-- Helpers
--

choice :: (Alternative f, MonadFail f) => String -> [a -> f b] -> a -> f b
choice entity xs a =
    asum (xs <*> pure a) <|> fail ("invalid " <> entity)
