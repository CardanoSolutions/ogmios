--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2020 KtorZ <matthias.benkort@gmail.com>
-- License: MPL-2.0
-- Stability: Stable
-- Portability: Unix
module Codec.Json.Wsp
    (
    -- * Types
      Request
    , mkRequest
    , Response
    , mkResponse
    , ToResponse
    , Fault
    , FaultCode (..)
    , mkFault
    , ToFault
    , ServiceName
    , Mirror

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
    ( Fault (..), FaultCode (..), Mirror, Request (..), Response (..) )
import Control.Applicative
    ( Alternative (..), empty )
import Control.Arrow
    ( second )
import Control.Monad
    ( guard, when )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), (.:), (.:?), (.=) )
import Data.Char
    ( toLower )
import Data.Kind
    ( Type )
import Data.Maybe
    ( isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Void
    ( Void )
import GHC.TypeLits
    ( KnownSymbol, Symbol, symbolVal )

import GHC.Generics

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Key as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Text as T

--
-- Types
--

-- | Service name as a type-family to avoid over-complicating all JSON
-- instances.
--
-- @since 1.0.0
type family ServiceName a :: Symbol

-- | A type-alias to help readability in signatures
--
-- @since 2.0.0
type ToResponse a = a -> Response a

-- | A type-alias to help readability in signatures
--
-- @since 2.0.0
type ToFault = FaultCode -> String -> Fault
--
-- Public ToJSON / FromJSON interfaces
--

-- | Parsing options, in a similar fasion to aeson.
--
-- @since 1.0.0
data Options = Options
    { fieldLabelModifier :: String -> String
    , constructorTagModifier :: String -> String
    , onMissingField :: Json.Key -> Json.Parser Json.Value
    }

-- | Default options for the generic parsing: do nothing.
--
-- @since 1.0.0
defaultOptions :: Options
defaultOptions = Options id id (\k -> fail $ "key " ++ show k ++ " not found")

-- | Parse a given Json 'Value' as a JSON-WSP 'Request'.
--
-- since @1.0.0
genericFromJSON
    :: (Generic a, GWSPFromJSON (Rep a))
    => Options
    -> Json.Value
    -> Json.Parser (Request a)
genericFromJSON opts =
    fmap (\(refl, x) -> Request refl (to x)) . gWSPFromJSON opts

-- | Serialize a given response to JSON, provided that the result has a generic
-- JSON instance.
--
-- since @1.0.0
genericToJSON
    :: forall req res.
        ( Generic res
        , KnownSymbol (ServiceName (Request req))
        , GWSPMethodName (Rep req)
        , GWSPToJSON (Rep res)
        )
    => Options
    -> Proxy (Request req)
    -> Response res
    -> Json.Encoding
genericToJSON opts proxy =
    -- TODO: `gWSPToJSON` constructs a JSON 'Value', and only after converts it
    -- into an 'Encoding'. This is not the most efficient as the whole point of
    -- using an 'Encoding' is to construct it along the way and avoid
    -- constructing an intermediate 'Value' altogether.
    mkResponse opts proxy (Json.value . gWSPToJSON opts . from)

-- | Serialize a given response to JSON
--
-- since @1.0.0
mkResponse
    :: forall req res.
        ( KnownSymbol (ServiceName (Request req))
        , GWSPMethodName (Rep req)
        )
    => Options
    -> Proxy (Request req)
    -> (res -> Json.Encoding)
    -> Response res
    -> Json.Encoding
mkResponse opts _proxy toResult (Response refl res) = Json.pairs $
    ("type" .= WspResponse)
    <>
    ("version" .= V1_0)
    <>
    ("servicename" .= symbolVal (Proxy @(ServiceName (Request req))))
    <>
    ("methodname" .= methodName)
    <>
    (Json.pair "result" (toResult res))
    <>
    ("reflection" .= refl)
  where
    methodName = constructorTagModifier opts $ gWSPMethodName (Proxy :: Proxy (Rep req a))

-- | Serialize a given 'Fault' to JSON
--
-- since @1.0.0
mkFault
    :: KnownSymbol (ServiceName (Request Void))
    => Fault
    -> Json.Encoding
mkFault Fault{faultMirror,faultCode,faultString} = Json.pairs $
    ("type" .= WspFault)
    <>
    ("version" .= V1_0)
    <>
    ("servicename" .= symbolVal (Proxy @(ServiceName (Request Void))))
    <>
    ("fault" .= Json.object
        [ "code" .= faultCode
        , "string" .= faultString
        ]
    )
    <>
    ("reflection" .= faultMirror)

-- | Serialize a given request to JSON
--
-- since @1.1.0
mkRequest
    :: forall req.
        ( KnownSymbol (ServiceName (Request req))
        , GWSPMethodName (Rep req)
        )
    => Options
    -> (req -> Json.Encoding)
    -> Request req
    -> Json.Encoding
mkRequest opts toArgs (Request mirror req) = Json.pairs $
    ("type" .= WspRequest)
    <>
    ("version" .= V1_0)
    <>
    ("servicename" .= symbolVal (Proxy @(ServiceName (Request req))))
    <>
    ("methodname" .= methodName)
    <>
    (Json.pair "args" (toArgs req))
    <>
    ("mirror" .= mirror)
  where
    methodName = constructorTagModifier opts $ gWSPMethodName (Proxy :: Proxy (Rep req a))

-- | Supported JSON-WSP versions.
--
-- @since 1.0.0
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

-- | Supported types of messages as per JSON-WSP specification.
--
-- @since 1.0.0
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

class GWSPFromJSON (f :: Type -> Type) where
    gWSPFromJSON :: Options -> Json.Value -> Json.Parser (Maybe Json.Value, f a)

class GWSPToJSON (f :: Type -> Type) where
    gWSPToJSON :: Options -> f a -> Json.Value

class GWSPMethodName (f :: Type -> Type) where
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
        wrong <- obj .:? "reflection"
        when (isJust @(Maybe Json.Value) wrong)
            (fail "invalid key 'reflection'; should be 'mirror' on requests.")
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
        let fieldName = Json.fromString $ fieldLabelModifier opts $ selName (undefined :: S1 s f a)
        (_, k1) <- obj .: "args"
            >>= (.:? fieldName)
            >>= maybe
                (gWSPFromJSON opts =<< onMissingField opts fieldName)
                (gWSPFromJSON opts)
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
        fieldName = Json.fromString $ constructorTagModifier opts $ conName (undefined :: C1 c f a)

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
        fieldName = Json.fromString $ fieldLabelModifier opts $ selName (undefined :: S1 s f a)

-- Arguments are expected to have JSON instances.
instance ToJSON c => GWSPToJSON (K1 i c) where
    gWSPToJSON _opts = toJSON . unK1

--
-- Validations (Internal)
--

parseKey :: (Eq a, FromJSON a, ToJSON a) => Json.Object -> Json.Key -> a -> Json.Parser a
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
    { missingKey :: Json.Key }
    deriving (Show, Eq)

data ErrInvalidKey = ErrInvalidKey
    { invalidKey :: Json.Key, expectedValue :: Json.Value }
    deriving (Show, Eq)

prettyErrValidateKey :: ErrValidateKey -> String
prettyErrValidateKey = \case
    ErrValidateKeyMissing ErrMissingKey{missingKey} ->
        T.unpack ("missing required field '"<>Json.toText missingKey<>"'.")

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
            "invalid value for field '"<>T.unpack (Json.toText invalidKey)<>"'"<>expected
