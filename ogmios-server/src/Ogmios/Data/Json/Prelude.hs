--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Prelude
    ( -- * Prelude
      module Relude
    , Json
    , jsonToByteString
    , FromJSON
    , decodeWith

      -- * Basic Types
    , encodeBool
    , encodeByteArray
    , encodeByteStringBase16
    , encodeByteStringBase64
    , encodeDouble
    , encodeIPv4
    , encodeIPv6
    , encodeInteger
    , encodeNatural
    , encodeNull
    , encodeRational
    , encodeScientific
    , encodeShortByteString
    , encodeString
    , encodeText
    , encodeWord
    , encodeWord16
    , encodeWord32
    , encodeWord64
    , encodeWord8

      -- * Data-Structures
    , encodeAnnotated
    , encodeFoldable
    , encodeList
    , encodeMap
    , encodeMaybe
    , encodeObject
    , encode2Tuple
    , encode3Tuple
    , encode4Tuple
    , encodeStrictMaybe

      -- * Queries
    , SomeQuery (..)
    ) where

import Relude

import Cardano.Binary
    ( Annotated (..) )
import Codec.Binary.Bech32
    ( HumanReadablePart )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Data.Aeson
    ( FromJSON )
import Data.ByteArray
    ( ByteArrayAccess )
import Data.ByteString.Base16
    ( encodeBase16 )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Data.ByteString.Base64
    ( encodeBase64 )
import Data.ByteString.Short
    ( ShortByteString, fromShort )
import Data.IP
    ( IPv4, IPv6 )
import Data.Scientific
    ( Scientific )
import Data.Sequence.Strict
    ( StrictSeq )
import Data.Vector
    ( Vector )
import Jsonifier
    ( Json )
import Ouroboros.Consensus.Shelley.Ledger.Query
    ( Query (..) )
import Shelley.Spec.Ledger.BaseTypes
    ( StrictMaybe (..) )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteArray as BA
import qualified Data.Map.Strict as Map
import qualified Jsonifier as Json

import qualified Data.Aeson.Parser.Internal as Aeson
import qualified Data.Aeson.Types as Aeson

--
-- Prelude
--

jsonToByteString :: Json -> ByteString
jsonToByteString = Json.toByteString

decodeWith :: (Aeson.Value -> Aeson.Parser a) -> ByteString -> Maybe a
decodeWith decoder =
    Aeson.decodeStrictWith Aeson.jsonEOF (Aeson.parse decoder)

--
-- Basic Types
--

encodeBool :: Bool -> Json
encodeBool = Json.bool

encodeByteArray :: ByteArrayAccess ba => (ByteString -> Json) -> ba -> Json
encodeByteArray encodeByteString =
    encodeByteString . BA.convert

encodeByteStringBase16 :: ByteString -> Json
encodeByteStringBase16 =
    encodeText . encodeBase16

encodeByteStringBase64 :: ByteString -> Json
encodeByteStringBase64 =
    encodeText . encodeBase64

encodeDouble :: Double -> Json
encodeDouble =
    Json.doubleNumber

encodeIPv4 :: IPv4 -> Json
encodeIPv4 =
    encodeString . show

encodeIPv6 :: IPv6 -> Json
encodeIPv6 =
    encodeString . show

encodeInteger :: Integer -> Json
encodeInteger =
    Json.scientificNumber . fromInteger

encodeNatural :: Natural -> Json
encodeNatural =
    encodeInteger . toInteger

encodeNull :: Json
encodeNull =
    Json.null

encodeRational :: Rational -> Json
encodeRational =
    encodeDouble . fromRational

encodeScientific :: Scientific -> Json
encodeScientific =
    Json.scientificNumber

encodeShortByteString :: (ByteString -> Json) -> ShortByteString -> Json
encodeShortByteString encodeByteString =
    encodeByteString . fromShort

encodeString :: String -> Json
encodeString =
    encodeText . toText

encodeText :: Text -> Json
encodeText =
    Json.textString

encodeWord :: Word -> Json
encodeWord =
    Json.wordNumber

encodeWord8 :: Word8 -> Json
encodeWord8 =
    encodeWord . fromIntegral

encodeWord16 :: Word16 -> Json
encodeWord16 =
    encodeWord . fromIntegral

encodeWord32 :: Word32 -> Json
encodeWord32 =
    encodeWord . fromIntegral

encodeWord64 :: Word64 -> Json
encodeWord64 =
    encodeInteger . toInteger

--
-- Data-Structures
--

encodeAnnotated :: (a -> Json) -> Annotated a any -> Json
encodeAnnotated encodeElem =
    encodeElem . unAnnotated

encodeFoldable :: Foldable f => (a -> Json) -> f a -> Json
encodeFoldable encodeElem =
    Json.array . foldr ((:) . encodeElem) []
{-# SPECIALIZE encodeFoldable :: (a -> Json) -> [a] -> Json #-}
{-# SPECIALIZE encodeFoldable :: (a -> Json) -> NonEmpty a -> Json #-}
{-# SPECIALIZE encodeFoldable :: (a -> Json) -> Vector a -> Json #-}
{-# SPECIALIZE encodeFoldable :: (a -> Json) -> Set a -> Json #-}
{-# SPECIALIZE encodeFoldable :: (a -> Json) -> StrictSeq a -> Json #-}

encodeList :: (a -> Json) -> [a] -> Json
encodeList encodeElem =
    Json.array . fmap encodeElem

encodeMap :: (k -> Text) -> (v -> Json) -> Map k v -> Json
encodeMap encodeKey encodeValue =
    Json.object . Map.foldrWithKey (\k v -> (:) (encodeKey k, encodeValue v)) []

encodeMaybe :: (a -> Json) -> Maybe a -> Json
encodeMaybe =
    maybe Json.null

encodeObject :: Foldable f => f (Text, Json) -> Json
encodeObject =
    Json.object
{-# SPECIALIZE encodeObject :: [(Text, Json)] -> Json #-}

encode2Tuple
    :: (a -> Json)
    -> (b -> Json)
    -> (a, b)
    -> Json
encode2Tuple encodeA encodeB (a,b) =
    Json.array [encodeA a, encodeB b]

encode3Tuple
    :: (a -> Json)
    -> (b -> Json)
    -> (c -> Json)
    -> (a, b, c)
    -> Json
encode3Tuple encodeA encodeB encodeC (a, b, c) =
    Json.array [encodeA a, encodeB b, encodeC c]

encode4Tuple
    :: (a -> Json)
    -> (b -> Json)
    -> (c -> Json)
    -> (d -> Json)
    -> (a, b, c, d)
    -> Json
encode4Tuple encodeA encodeB encodeC encodeD (a, b, c, d) =
    Json.array [encodeA a, encodeB b, encodeC c, encodeD d]

encodeStrictMaybe :: (a -> Json) -> StrictMaybe a -> Json
encodeStrictMaybe encodeElem = \case
    SNothing -> Json.null
    SJust a  -> encodeElem a

--
-- Queries
--

data SomeQuery (f :: * -> *) block = forall result. SomeQuery
    { query :: Query block result
    , encodeResult :: result -> Json
    , genResult :: Proxy result -> f result
    }
