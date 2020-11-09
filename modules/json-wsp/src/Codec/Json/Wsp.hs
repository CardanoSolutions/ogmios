--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK hide #-}

module Codec.Json.Wsp
    (
    -- * Types
      Request
    , Response
    , mkResponse
    , ServiceName

    -- * ToJSON / FromJSON
    , Options (..)
    , defaultOptions
    , genericFromJSON
    , genericToJSON

    -- * Generic
    , gWSPFromJSON
    , gWSPToJSON
    , gWSPMethodName
    ) where

import Prelude

import Codec.Json.Wsp.Handler
    ( Request (..), Response (..) )
import Control.Applicative
    ( Alternative (..), empty )
import Control.Arrow
    ( second )
import Control.Monad
    ( guard )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), (.:), (.:?), (.=) )
import Data.Char
    ( toLower )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import GHC.TypeLits
    ( KnownSymbol, Symbol, symbolVal )

import GHC.Generics

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Text as T

--
-- Types
--

type family ServiceName a :: Symbol

--
-- Public ToJSON / FromJSON interfaces
--

data Options = Options
    { fieldLabelModifier :: String -> String
    , constructorTagModifier :: String -> String
    }

-- | Default options for the generic parsing: do nothing.
defaultOptions :: Options
defaultOptions = Options id id

-- | Parse a given Json 'Value' as a JSON-WSP 'Request'.
genericFromJSON
    :: (Generic a, GWSPFromJSON (Rep a))
    => Options
    -> Json.Value
    -> Json.Parser (Request a)
genericFromJSON opts =
    fmap (\(refl, x) -> Request refl (to x)) . gWSPFromJSON opts

-- | Serialize a given response to JSON, provided that the result has a generic
-- JSON instance.
genericToJSON
    :: forall req res.
        ( Generic res
        , KnownSymbol (ServiceName (Response res))
        , GWSPMethodName (Rep req)
        , GWSPToJSON (Rep res)
        )
    => Options
    -> Proxy (Request req)
    -> Response res
    -> Json.Value
genericToJSON opts =
    mkResponse opts (gWSPToJSON opts . from)

-- | Serialize a given response to JSON
mkResponse
    :: forall req res.
        ( KnownSymbol (ServiceName (Response res))
        , GWSPMethodName (Rep req)
        )
    => Options
    -> (res -> Json.Value)
    -> Proxy (Request req)
    -> Response res
    -> Json.Value
mkResponse opts toResult _ (Response refl res) = Json.object
    [ "type" .= WspResponse
    , "version" .= V1_0
    , "servicename" .= symbolVal (Proxy @(ServiceName (Response res)))
    , "methodname" .= methodName
    , "result" .= toResult res
    , "reflection" .= refl
    ]
  where
    methodName = constructorTagModifier opts $ gWSPMethodName (Proxy :: Proxy (Rep req a))

data WspVersion
    = V1_0
    deriving (Show, Eq)

instance ToJSON WspVersion where
    toJSON = \case
        V1_0 -> Json.String "1.0"

instance FromJSON WspVersion where
    parseJSON bytes = do
        json <- parseJSON bytes
        guard (json == toJSON V1_0)
        pure V1_0

data WspType
    = WspDescription
    | WspRequest
    | WspResponse
    | WspFault
    deriving (Show, Eq)

instance ToJSON WspType where
    toJSON = toJSON . ("jsonwsp/" <>) . fmap toLower . drop 3 . show

instance FromJSON WspType where
    parseJSON bytes = do
        parseJSON bytes >>= \case
            json | json == toJSON WspDescription -> pure WspDescription
            json | json == toJSON WspRequest     -> pure WspRequest
            json | json == toJSON WspResponse    -> pure WspResponse
            json | json == toJSON WspFault       -> pure WspFault
            _ -> empty

class GWSPFromJSON (f :: * -> *) where
    gWSPFromJSON :: Options -> Json.Value -> Json.Parser (Maybe Json.Value, f a)

class GWSPToJSON (f :: * -> *) where
    gWSPToJSON :: Options -> f a -> Json.Value

class GWSPMethodName (f :: * -> *) where
    gWSPMethodName :: Proxy (f a) -> String

--
-- Generic Machinery for parsing 'Request'
--

instance GWSPFromJSON f => GWSPFromJSON (D1 c f) where
    gWSPFromJSON opts = fmap (second M1) . gWSPFromJSON opts

instance (Constructor c, GWSPFromJSON f) => GWSPFromJSON (C1 c f) where
    gWSPFromJSON opts value = flip (Json.withObject "U1") value $ \obj -> do
        _ <- parseKey obj "type" WspRequest
        _ <- parseKey obj "version" V1_0
        _ <- parseKey obj "methodname" methodName
        refl <- obj .:? "mirror"
        (_, f) <- gWSPFromJSON opts value
        pure (refl, M1 f)
      where
        methodName = T.pack $ constructorTagModifier opts $ conName (undefined :: C1 c U1 a)

instance (GWSPFromJSON f, GWSPFromJSON g) => GWSPFromJSON (f :*: g) where
    gWSPFromJSON opts value = do
        (_, f) <- gWSPFromJSON opts value
        (_, g) <- gWSPFromJSON opts value
        pure (Nothing, f :*: g)

-- Constructor *must* use the record-selector-syntax. This is necessary in order
-- to map the WSP arguments to their corresponding position in the constructor.
instance (Selector s, GWSPFromJSON f) => GWSPFromJSON (S1 s f) where
    gWSPFromJSON opts = Json.withObject "S1" $ \obj -> do
        let fieldName = T.pack $ fieldLabelModifier opts $ selName (undefined :: S1 s f a)
        (_, k1) <- obj .: "args" >>= (.: fieldName) >>= gWSPFromJSON opts
        pure (Nothing, M1 k1)

-- Unary constructor, nothing to do, the constructor name has been verified
-- with the 'C1 c f' instance.
instance GWSPFromJSON U1 where
    gWSPFromJSON _opts _value = pure (Nothing, U1)

-- Arguments are expected to have JSON instances.
instance (FromJSON c) => GWSPFromJSON (K1 i c) where
    gWSPFromJSON _opts = fmap ((Nothing,) . K1) . parseJSON

--
-- Generic Machinery for serializing 'Response'
--

instance GWSPMethodName f => GWSPMethodName (D1 c f) where
    gWSPMethodName _ = gWSPMethodName (Proxy :: Proxy (f a))

instance (Constructor c) => GWSPMethodName (C1 c f) where
    gWSPMethodName _ = conName (undefined :: C1 c f a)

instance GWSPToJSON f => GWSPToJSON (D1 c f) where
    gWSPToJSON opts = gWSPToJSON opts . unM1

instance (Constructor c, GWSPToJSON f) => GWSPToJSON (C1 c f) where
    gWSPToJSON opts (M1 f) =
        Json.object [ fieldName .= gWSPToJSON opts f ]
      where
        fieldName = T.pack $ constructorTagModifier opts $ conName (undefined :: C1 c f a)

instance (GWSPToJSON f, GWSPToJSON g) => GWSPToJSON (f :+: g) where
    gWSPToJSON opts = \case
        L1 f -> gWSPToJSON opts f
        R1 g -> gWSPToJSON opts g

instance (GWSPToJSON f, GWSPToJSON g) => GWSPToJSON (f :*: g) where
    gWSPToJSON opts (f :*: g) =
        case (gWSPToJSON opts f, gWSPToJSON opts g) of
            (Json.Object ff, Json.Object gg) ->
                Json.Object (ff <> gg)
            (_, _) ->
                error "gWSPToJSON (f :*: g): impossible"

-- Constructor *must* use the record-selector-syntax. This is necessary in order
-- to map the WSP arguments to their corresponding position in the constructor.
instance (Selector s, GWSPToJSON f) => GWSPToJSON (S1 s f) where
    gWSPToJSON opts (M1 f) =
        Json.object [ fieldName .= gWSPToJSON opts f ]
      where
        fieldName = T.pack $ fieldLabelModifier opts $ selName (undefined :: S1 s f a)

-- Arguments are expected to have JSON instances.
instance ToJSON c => GWSPToJSON (K1 i c) where
    gWSPToJSON _opts = toJSON . unK1

--
-- Validations (Internal)
--

parseKey :: (Eq a, FromJSON a, ToJSON a) => Json.Object -> Text -> a -> Json.Parser a
parseKey obj key expected =
    case Json.parseMaybe (.:? key) obj of
        Just Nothing -> fail $ prettyErrValidateKey
            $ ErrValidateKeyMissing $ ErrMissingKey key
        Nothing -> fail $ prettyErrValidateKey
            $ ErrValidateKeyInvalid $ ErrInvalidKey key (toJSON expected)
        Just (Just v) | v /= expected -> fail $ prettyErrValidateKey
            $ ErrValidateKeyInvalid $ ErrInvalidKey key (toJSON expected)
        Just (Just v) ->
            pure v

data ErrValidateKey
    = ErrValidateKeyMissing ErrMissingKey
    | ErrValidateKeyInvalid ErrInvalidKey
    deriving (Show, Eq)

data ErrMissingKey = ErrMissingKey
    { missingKey :: Text }
    deriving (Show, Eq)

data ErrInvalidKey = ErrInvalidKey
    { invalidKey :: Text, expectedValue :: Json.Value }
    deriving (Show, Eq)

prettyErrValidateKey :: ErrValidateKey -> String
prettyErrValidateKey = \case
    ErrValidateKeyMissing ErrMissingKey{missingKey} ->
        "missing required field '"<>T.unpack missingKey<>"'."

    ErrValidateKeyInvalid ErrInvalidKey{invalidKey,expectedValue} ->
        let
            expected = case expectedValue of
                Json.Number s ->
                    ": should be '"<>show s<>"'"
                Json.String t ->
                    ": should be '"<>T.unpack t<>"'"
                _ ->
                    "."
        in
            "invalid value for field '"<>T.unpack invalidKey<>"'"<>expected
