--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Ogmios.Data.Json.Prelude
    ( -- * Prelude
      module Ogmios.Prelude
    , Json
    , FromJSON
    , ToJSON
    , SerializationMode(..)
    , ViaEncoding (..)
    , jsonToByteString
    , decodeWith
    , choice
    , inefficientEncodingToValue
    , (.:)
    , (.:?)
    , at

      -- * Re-Exports
    , Coin (..)
    , StrictMaybe (..)

      -- * Decoder
    , decodeBase16
    , decodeBase58
    , decodeBase64

      -- * Basic Types
    , encodeBlockNo
    , encodeBool
    , encodeByteArray
    , encodeByteStringBase16
    , encodeByteStringBase64
    , encodeByteStringBech32
    , encodeCoin
    , encodeDnsName
    , encodeDouble
    , encodeEpochNo
    , encodeEpochSize
    , encodeIPv4
    , encodeIPv6
    , encodeInteger
    , encodeNatural
    , encodeNominalDiffTime
    , encodeNonNegativeInterval
    , encodeNull
    , encodePort
    , encodePositiveUnitInterval
    , encodeRational
    , encodeRelativeTime
    , encodeScientific
    , encodeShortByteString
    , encodeSlotNo
    , encodeSlotLength
    , encodeString
    , encodeSystemStart
    , encodeText
    , encodeUnitInterval
    , encodeUrl
    , encodeUtcTime
    , encodeWithOrigin
    , encodeWord
    , encodeWord16
    , encodeWord32
    , encodeWord64
    , encodeWord8

      -- * Data-Structures
    , encodeAnnotated
    , encodeIdentity
    , encodeFoldable
    , encodeFoldable'
    , encodeList
    , encodeListWithMode
    , encodeMap
    , encodeMapWithMode
    , encodeMaybe
    , encodeObject
    , encodeObjectWithMode
    , encode2Tuple
    , encode3Tuple
    , encode4Tuple
    , encodeStrictMaybe
    ) where

import Ogmios.Prelude

import Cardano.Binary
    ( Annotated (..)
    )
import Cardano.Ledger.BaseTypes
    ( DnsName
    , NonNegativeInterval
    , Port
    , PositiveUnitInterval
    , StrictMaybe (..)
    , UnitInterval
    , Url
    , dnsToText
    , portToWord16
    , unboundRational
    , urlToText
    )
import Cardano.Ledger.Coin
    ( Coin (..)
    )
import Cardano.Slotting.Block
    ( BlockNo (..)
    )
import Cardano.Slotting.Slot
    ( EpochNo (..)
    , EpochSize (..)
    , SlotNo (..)
    , WithOrigin (..)
    )
import Cardano.Slotting.Time
    ( SlotLength (..)
    , SystemStart (..)
    )
import Data.Aeson
    ( FromJSON
    , ToJSON
    , (.:)
    , (.:?)
    )
import Data.ByteArray
    ( ByteArrayAccess
    )
import Data.ByteString.Base16
    ( encodeBase16
    )
import Data.ByteString.Base64
    ( encodeBase64
    )
import Data.ByteString.Bech32
    ( HumanReadablePart
    , encodeBech32
    )
import Data.IP
    ( IPv4
    , IPv6
    )
import Data.Ratio
    ( (%)
    )
import Data.Scientific
    ( Scientific
    )
import Data.Sequence.Strict
    ( StrictSeq
    )
import Data.Time.Clock
    ( NominalDiffTime
    , UTCTime
    )
import Data.Vector
    ( Vector
    )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime (..)
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Key as Json
import qualified Data.Aeson.KeyMap as Json
import qualified Data.Aeson.Parser.Internal as Json hiding
    ( scientific
    )
import qualified Data.Aeson.Types as Json
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base58 as B58
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Ouroboros.Consensus.Util.Counting as Consensus

--
-- Prelude
--

type Json = Json.Encoding

jsonToByteString :: Json -> ByteString
jsonToByteString = toStrict . Json.encodingToLazyByteString

decodeWith :: (Json.Value -> Json.Parser a) -> ByteString -> Maybe a
decodeWith decoder =
    Json.decodeStrictWith Json.jsonEOF (Json.parse decoder)

choice :: (Alternative f, MonadFail f) => String -> [a -> f b] -> a -> f b
choice entity xs a =
    asum (xs <*> pure a) <|> fail ("invalid " <> entity)

at :: Json.Key -> Json.Value -> Maybe Json.Value
at key = \case
    Json.Object m -> Json.lookup key m
    _ -> Nothing

-- | Converts a 'Json.Encoding' to a 'Json.Value'. This is inefficient because
-- the conversion is done by serializing the encoding to bytestring, and parsing
-- the bytestring back into a value.
--
-- This is to use with care in areas where performances do not matter and where
-- one cannot simply rely on 'Json.Encoding' solely.
--
-- The more 'correct' way to do this would be to define both a `toJSON` and
-- `toEncoding` wherever this function is needed which usually means duplicating
-- code.
inefficientEncodingToValue :: Json.Encoding -> Json.Value
inefficientEncodingToValue = unsafeDecodeValue . Json.encodingToLazyByteString
  where
    unsafeDecodeValue :: BL.ByteString -> Json.Value
    unsafeDecodeValue =
        let
            justification =
                "impossible: couldn't decode JSON value that was just encoded."
        in
            fromMaybe (error justification) . Json.decode

newtype ViaEncoding = ViaEncoding { unViaEncoding :: Json }
    deriving (Show, Generic)

instance ToJSON ViaEncoding where
    toJSON = inefficientEncodingToValue . unViaEncoding
    toEncoding = unViaEncoding

--
-- Serialization Mode
--

-- | The 'SerializationMode' allows for selectively run different JSON
-- serializers. The 'CompactSerialization' mode will omit some fields deemed
-- non-necessary in a trustless setup (for example, when clients fully trust the
-- node / ogmios server they're connecting to).
data SerializationMode
    = FullSerialization
    | CompactSerialization
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

--
-- Basic Types
--

encodeBlockNo :: BlockNo -> Json
encodeBlockNo =
    encodeWord64 . unBlockNo
{-# INLINABLE encodeBlockNo #-}

encodeBool :: Bool -> Json
encodeBool =
    Json.bool
{-# INLINABLE encodeBool #-}

encodeByteArray :: ByteArrayAccess ba => (ByteString -> Json) -> ba -> Json
encodeByteArray encodeByteString =
    encodeByteString . BA.convert
{-# INLINABLE encodeByteArray #-}

encodeByteStringBase16 :: ByteString -> Json
encodeByteStringBase16 =
    encodeText . encodeBase16
{-# INLINABLE encodeByteStringBase16 #-}

encodeByteStringBech32 :: HumanReadablePart -> ByteString -> Json
encodeByteStringBech32 hrp =
    encodeText . encodeBech32 hrp
{-# INLINABLE encodeByteStringBech32 #-}

encodeByteStringBase64 :: ByteString -> Json
encodeByteStringBase64 =
    encodeText . encodeBase64
{-# INLINABLE encodeByteStringBase64 #-}

encodeCoin :: Coin -> Json
encodeCoin =
    encodeInteger . unCoin
{-# INLINEABLE encodeCoin #-}

encodeDnsName :: DnsName -> Json
encodeDnsName =
    encodeText . dnsToText
{-# INLINABLE encodeDnsName #-}

encodeDouble :: Double -> Json
encodeDouble =
    Json.toEncoding
{-# INLINABLE encodeDouble #-}

encodeEpochNo :: EpochNo -> Json
encodeEpochNo =
    encodeWord64 . unEpochNo
{-# INLINABLE encodeEpochNo #-}

encodeEpochSize :: EpochSize -> Json
encodeEpochSize =
    encodeWord64 . unEpochSize
{-# INLINABLE encodeEpochSize #-}

encodeIPv4 :: IPv4 -> Json
encodeIPv4 =
    encodeString . show
{-# INLINABLE encodeIPv4 #-}

encodeIPv6 :: IPv6 -> Json
encodeIPv6 =
    encodeString . show
{-# INLINABLE encodeIPv6 #-}

encodeInteger :: Integer -> Json
encodeInteger =
    Json.integer
{-# INLINABLE encodeInteger #-}

encodeNatural :: Natural -> Json
encodeNatural =
    encodeInteger . toInteger
{-# INLINABLE encodeNatural #-}

encodeNominalDiffTime :: NominalDiffTime -> Json
encodeNominalDiffTime t =
    -- TODO / NOTE: Backward-compatibility prior to v5.5.4. Should encode only
    -- as Double in v6+
    if i % 1 == r then Json.integer i else Json.double (fromRational r)
  where
    r = toRational t
    i = round t
{-# INLINABLE encodeNominalDiffTime #-}

encodeNonNegativeInterval :: NonNegativeInterval -> Json
encodeNonNegativeInterval =
    encodeRational . unboundRational
{-# INLINABLE encodeNonNegativeInterval #-}

encodeNull :: Json
encodeNull =
    Json.null_
{-# INLINABLE encodeNull #-}

encodePort :: Port -> Json
encodePort =
    encodeWord16 . portToWord16
{-# INLINABLE encodePort #-}

encodePositiveUnitInterval :: PositiveUnitInterval -> Json
encodePositiveUnitInterval =
    encodeRational . unboundRational
{-# INLINABLE encodePositiveUnitInterval #-}

encodeRational :: Rational -> Json
encodeRational r =
    encodeText (show (numerator r) <> "/" <> show (denominator r))
{-# INLINABLE encodeRational #-}

encodeRelativeTime :: RelativeTime -> Json
encodeRelativeTime =
    encodeNominalDiffTime . getRelativeTime
{-# INLINABLE encodeRelativeTime #-}

encodeScientific :: Scientific -> Json
encodeScientific =
    Json.scientific
{-# INLINABLE encodeScientific #-}

encodeShortByteString :: (ByteString -> Json) -> ShortByteString -> Json
encodeShortByteString encodeByteString =
    encodeByteString . fromShort
{-# INLINABLE encodeShortByteString #-}

encodeSlotLength :: SlotLength -> Json
encodeSlotLength =
    encodeNominalDiffTime . getSlotLength
{-# INLINABLE encodeSlotLength #-}

encodeSlotNo :: SlotNo -> Json
encodeSlotNo =
    encodeWord64 . unSlotNo
{-# INLINABLE encodeSlotNo #-}

encodeString :: String -> Json
encodeString =
    Json.string
{-# INLINABLE encodeString #-}

encodeSystemStart
    :: SystemStart
    -> Json
encodeSystemStart =
    encodeUtcTime . getSystemStart
{-# INLINABLE encodeSystemStart #-}

encodeText :: Text -> Json
encodeText =
    Json.text
{-# INLINABLE encodeText #-}

encodeUnitInterval :: UnitInterval -> Json
encodeUnitInterval =
    encodeRational . unboundRational
{-# INLINABLE encodeUnitInterval #-}

encodeUrl :: Url -> Json
encodeUrl =
    encodeText . urlToText
{-# INLINABLE encodeUrl #-}

encodeUtcTime :: UTCTime -> Json
encodeUtcTime =
    Json.utcTime
{-# INLINABLE encodeUtcTime #-}

encodeWithOrigin :: (a -> Json) -> WithOrigin a -> Json
encodeWithOrigin encodeA = \case
    Origin -> encodeText "origin"
    At a -> encodeA a
{-# INLINABLE encodeWithOrigin #-}

encodeWord :: Word -> Json
encodeWord =
    Json.word
{-# INLINABLE encodeWord #-}

encodeWord8 :: Word8 -> Json
encodeWord8 =
    Json.word8
{-# INLINABLE encodeWord8 #-}

encodeWord16 :: Word16 -> Json
encodeWord16 =
    Json.word16
{-# INLINABLE encodeWord16 #-}

encodeWord32 :: Word32 -> Json
encodeWord32 =
    Json.word32
{-# INLINABLE encodeWord32 #-}

encodeWord64 :: Word64 -> Json
encodeWord64 =
    Json.word64
{-# INLINABLE encodeWord64 #-}

--
-- Data-Structures
--

encodeAnnotated :: (a -> Json) -> Annotated a any -> Json
encodeAnnotated encodeElem =
    encodeElem . unAnnotated
{-# INLINABLE encodeAnnotated #-}

encodeIdentity :: (a -> Json) -> Identity a -> Json
encodeIdentity encodeElem =
    encodeElem . runIdentity
{-# INLINABLE encodeIdentity #-}

encodeFoldable :: Foldable f => (a -> Json) -> f a -> Json
encodeFoldable encodeElem =
    Json.list id . foldr ((:) . encodeElem) []
{-# SPECIALIZE encodeFoldable :: (a -> Json) -> [a] -> Json #-}
{-# SPECIALIZE encodeFoldable :: (a -> Json) -> NonEmpty a -> Json #-}
{-# SPECIALIZE encodeFoldable :: (a -> Json) -> Consensus.NonEmpty xs a -> Json #-}
{-# SPECIALIZE encodeFoldable :: (a -> Json) -> Vector a -> Json #-}
{-# SPECIALIZE encodeFoldable :: (a -> Json) -> Set a -> Json #-}
{-# SPECIALIZE encodeFoldable :: (a -> Json) -> StrictSeq a -> Json #-}
{-# INLINABLE encodeFoldable #-}

encodeFoldable' :: Foldable f => (a -> Text) -> (a -> Json) -> f a -> Json
encodeFoldable' encodeKey encodeValue =
    Json.pairs . foldr (\a -> (<>) (Json.pair (Json.fromText (encodeKey a)) (encodeValue a))) mempty
{-# SPECIALIZE encodeFoldable' :: (a -> Text) -> (a -> Json) -> [a] -> Json #-}
{-# SPECIALIZE encodeFoldable' :: (a -> Text) -> (a -> Json) -> NonEmpty a -> Json #-}
{-# SPECIALIZE encodeFoldable' :: (a -> Text) -> (a -> Json) -> Vector a -> Json #-}
{-# SPECIALIZE encodeFoldable' :: (a -> Text) -> (a -> Json) -> Set a -> Json #-}
{-# SPECIALIZE encodeFoldable' :: (a -> Text) -> (a -> Json) -> StrictSeq a -> Json #-}
{-# INLINABLE encodeFoldable' #-}

encodeList :: (a -> Json) -> [a] -> Json
encodeList =
    Json.list
{-# INLINABLE encodeList #-}

encodeListWithMode :: SerializationMode -> (a -> Json) -> [a] -> Json
encodeListWithMode mode =
    case mode of
        FullSerialization ->
            Json.list
        CompactSerialization -> \encodeVal xs ->
            let n = 5
                r = length xs - n
             in
            Json.list id
                ( (encodeVal <$> take n xs)
                  ++
                  [ encodeText ("..." <> show r <> " more element(s)") | r > 0 ]
                )
{-# INLINABLE encodeListWithMode #-}

encodeMap :: (k -> Text) -> (v -> Json) -> Map k v -> Json
encodeMap encodeKey encodeValue =
    encodeObject . Map.foldrWithKey (\k v -> (:) (encodeKey k, encodeValue v)) []
{-# INLINABLE encodeMap #-}

encodeMapWithMode :: SerializationMode -> (k -> Text) -> (v -> Json) -> Map k v -> Json
encodeMapWithMode mode encodeKey encodeValue m =
    case mode of
        FullSerialization ->
            encodeMap encodeKey encodeValue m
        CompactSerialization ->
            let reducer k v = (:) (Json.pairs $ Json.pair (Json.fromText (encodeKey k)) (encodeValue v))
                zero = [ encodeText ("..." <> show r <> " more element(s)") | r > 0 ]
                r = Map.size m - n
                n = 5
             in (Json.list id . Map.foldrWithKey reducer zero . Map.take n) m
{-# INLINABLE encodeMapWithMode #-}

encodeMaybe :: (a -> Json) -> Maybe a -> Json
encodeMaybe =
    maybe encodeNull
{-# INLINABLE encodeMaybe #-}

encodeObject :: [(Text, Json)] -> Json
encodeObject =
    Json.pairs . foldr (\(Json.fromText -> k, v) -> (<>) (Json.pair k v)) mempty
{-# INLINABLE encodeObject #-}

encodeObjectWithMode
    :: SerializationMode
    -> [(Text, Json)]
        -- ^ Common fields
    -> [(Text, Json)]
        -- ^ Field when mode = full
    -> Json
encodeObjectWithMode mode common whenFull =
    let
        rest = case mode of
            CompactSerialization -> []
            FullSerialization -> whenFull
    in
        encodeObject (rest ++ common)
{-# INLINABLE encodeObjectWithMode #-}

encode2Tuple
    :: (a -> Json)
    -> (b -> Json)
    -> (a, b)
    -> Json
encode2Tuple encodeA encodeB (a,b) =
    Json.list id [encodeA a, encodeB b]
{-# INLINABLE encode2Tuple #-}

encode3Tuple
    :: (a -> Json)
    -> (b -> Json)
    -> (c -> Json)
    -> (a, b, c)
    -> Json
encode3Tuple encodeA encodeB encodeC (a, b, c) =
    Json.list id [encodeA a, encodeB b, encodeC c]
{-# INLINABLE encode3Tuple #-}

encode4Tuple
    :: (a -> Json)
    -> (b -> Json)
    -> (c -> Json)
    -> (d -> Json)
    -> (a, b, c, d)
    -> Json
encode4Tuple encodeA encodeB encodeC encodeD (a, b, c, d) =
    Json.list id [encodeA a, encodeB b, encodeC c, encodeD d]
{-# INLINABLE encode4Tuple #-}

encodeStrictMaybe :: (a -> Json) -> StrictMaybe a -> Json
encodeStrictMaybe encodeElem = \case
    SNothing -> encodeNull
    SJust a  -> encodeElem a
{-# INLINABLE encodeStrictMaybe #-}

--
-- Decoder
--

decodeBase16 :: ByteString -> Json.Parser ByteString
decodeBase16 = either (fail . toString) pure . B16.decodeBase16

decodeBase58 :: ByteString -> Json.Parser ByteString
decodeBase58 = maybe mempty pure . B58.decodeBase58 B58.bitcoinAlphabet

decodeBase64 :: ByteString -> Json.Parser ByteString
decodeBase64 = either (fail . toString) pure . B64.decodeBase64
