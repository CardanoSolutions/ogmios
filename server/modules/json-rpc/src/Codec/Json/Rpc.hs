--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2020 KtorZ <matthias.benkort@gmail.com>
-- License: MPL-2.0
-- Stability: Stable
-- Portability: Unix
module Codec.Json.Rpc
    (
    -- * Types
      Request
    , mkRequest
    , mkRequestNoParams
    , Response
    , mkResponse
    , ok
    , ko
    , ToResponse
    , Fault
    , FaultCode (..)
    , ToFault
    , Mirror

    -- * ToJSON / FromJSON
    , Options (..)
    , defaultOptions
    , genericFromJSON
    , genericToJSON

    -- * Generic
    , gRpcFromJSON
    , gRpcToJSON
    , gRpcMethodName
    ) where

import Prelude

import Codec.Json.Rpc.Handler
    ( Fault (..)
    , FaultCode (..)
    , Mirror
    , Request (..)
    , Response (..)
    )
import Control.Arrow
    ( second
    )
import Control.Monad
    ( guard
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , (.:)
    , (.:?)
    , (.=)
    )
import Data.Char
    ( toLower
    )
import Data.Kind
    ( Type
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )

import GHC.Generics

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Key as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Text as T

--
-- Types
--

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
    , methodNamePredicate :: Text -> Text -> Bool
    }

-- | Default options for the generic parsing: do nothing.
--
-- @since 1.0.0
defaultOptions :: Options
defaultOptions = Options
    id
    (\case
        [] -> []
        x:xs -> toLower x : xs
    )
    (\k -> fail $ "key " ++ show k ++ " not found")
    (\v expected -> v == expected)

-- | Parse a given Json 'Value' as a JSON-Rpc 'Request'.
--
-- since @1.0.0
genericFromJSON
    :: (Generic a, GRpcFromJSON (Rep a))
    => Options
    -> Json.Value
    -> Json.Parser (Request a)
genericFromJSON opts =
    fmap (\(refl, x) -> Request refl (to x)) . gRpcFromJSON opts

-- | Serialize a given response to JSON, provided that the result has a generic
-- JSON instance.
--
-- since @1.0.0
genericToJSON
    :: forall res.
        ( Generic res
        , GRpcToJSON (Rep res)
        )
    => Options
    -> Response res
    -> Json.Encoding
genericToJSON opts =
    -- TODO: `gRpcToJSON` constructs a JSON 'Value', and only after converts it
    -- into an 'Encoding'. This is not the most efficient as the whole point of
    -- using an 'Encoding' is to construct it along the way and avoid
    -- constructing an intermediate 'Value' altogether.
    ok (Json.value . gRpcToJSON opts . from)

-- | Serialize a given response to JSON
--
-- since @1.0.0
mkResponse
    :: forall res. ()
    => (   (Json.Encoding -> Json.Encoding)
        -> (FaultCode -> String -> Maybe Json.Encoding -> Json.Encoding)
        -> res
        -> Json.Encoding
       )
    -> Response res
    -> Json.Encoding
mkResponse choose (Response refl res) =
    choose
        (\result -> Json.pairs $
            ("jsonrpc" .= V2_0)
            <>
            (Json.pair "result" result)
            <>
            ("id" .= refl)
        )
        (\faultCode faultMessage faultData -> Json.pairs $
            ("jsonrpc" .= V2_0)
            <>
            (Json.pair "error" $ Json.pairs $
                ("code" .= faultCode)
                <>
                ("message" .= faultMessage)
                <>
                (maybe mempty (Json.pair "data") faultData)
            )
            <>
            ("id" .= refl)
        )
        res

-- | Shorthand for returning success responses.
--
-- since @1.0.0
ok  :: forall res. ()
    => (res -> Json.Encoding)
    -> Response res
    -> Json.Encoding
ok toResult =
    mkResponse $ \resolve _reject -> resolve . toResult
{-# INLINEABLE ok #-}

-- | Shorthand for returning failure responses.
--
-- since @1.0.0
ko :: Fault -> Json.Encoding
ko Fault{faultCode,faultMessage,faultMirror} =
    mkResponse
        (\_resolve reject () -> reject faultCode faultMessage Nothing)
        (Response faultMirror ())
{-# INLINEABLE ko #-}

-- | Serialize a given request to JSON
--
-- since @1.1.0
mkRequest
    :: forall req.
        ( GRpcMethodName (Rep req)
        )
    => Options
    -> (req -> Json.Encoding)
    -> Request req
    -> Json.Encoding
mkRequest opts toArgs (Request mirror req) = Json.pairs $
    ("jsonrpc" .= V2_0)
    <>
    ("method" .= method)
    <>
    (Json.pair "params" (toArgs req))
    <>
    ("id" .= mirror)
  where
    method = gRpcMethodName opts (Proxy :: Proxy (Rep req a))

-- | Serialize a given request to JSON, without any parameter.
--
-- since @1.1.0
mkRequestNoParams
    :: forall req.
        ( GRpcMethodName (Rep req)
        )
    => Options
    -> Request req
    -> Json.Encoding
mkRequestNoParams opts (Request mirror _req) = Json.pairs $
    ("jsonrpc" .= V2_0)
    <>
    ("method" .= method)
    <>
    ("id" .= mirror)
  where
    method = gRpcMethodName opts (Proxy :: Proxy (Rep req a))

-- | Supported JSON-Rpc versions.
--
-- @since 1.0.0
data RpcVersion
    = V2_0
    deriving (Show, Eq)

instance ToJSON RpcVersion where
    toEncoding = \case
        V2_0 -> Json.string "2.0"
    toJSON = \case
        V2_0 -> Json.String "2.0"

instance FromJSON RpcVersion where
    parseJSON bytes = do
        json <- parseJSON bytes
        guard (json == toJSON V2_0)
        pure V2_0

class GRpcFromJSON (f :: Type -> Type) where
    gRpcFromJSON :: Options -> Json.Value -> Json.Parser (Maybe Json.Value, f a)

class GRpcToJSON (f :: Type -> Type) where
    gRpcToJSON :: Options -> f a -> Json.Value

class GRpcMethodName (f :: Type -> Type) where
    gRpcMethodName :: Options -> Proxy (f a) -> String

--
-- Generic Machinery for parsing 'Request'
--

instance GRpcFromJSON f => GRpcFromJSON (D1 c f) where
    gRpcFromJSON opts = fmap (second M1) . gRpcFromJSON opts

instance (Constructor c, GRpcFromJSON f) => GRpcFromJSON (C1 c f) where
    gRpcFromJSON opts value = flip (Json.withObject "U1") value $ \obj -> do
        _ <- parseKey obj "jsonrpc" "2.0"
        _ <- parseKeyWith (methodNamePredicate opts) obj "method" methodName
        refl <- obj .:? "id"
        (_, f) <- gRpcFromJSON opts value
        pure (refl, M1 f)
      where
        methodName = T.pack $ constructorTagModifier opts $ conName (undefined :: C1 c U1 a)

instance (GRpcFromJSON f, GRpcFromJSON g) => GRpcFromJSON (f :*: g) where
    gRpcFromJSON opts value = do
        (_, f) <- gRpcFromJSON opts value
        (_, g) <- gRpcFromJSON opts value
        pure (Nothing, f :*: g)

-- Constructor *must* use the record-selector-syntax. This is necessary in order
-- to map the Rpc arguments to their corresponding position in the constructor.
instance (Selector s, GRpcFromJSON f) => GRpcFromJSON (S1 s f) where
    gRpcFromJSON opts = Json.withObject "S1" $ \obj -> do
        let fieldName = Json.fromString $ fieldLabelModifier opts $ selName (undefined :: S1 s f a)
        (_, k1) <- obj .: "params"
            >>= (.:? fieldName)
            >>= maybe
                (gRpcFromJSON opts =<< onMissingField opts fieldName)
                (gRpcFromJSON opts)
        pure (Nothing, M1 k1)

-- Unary constructor, nothing to do, the constructor name has been verified
-- with the 'C1 c f' instance.
instance GRpcFromJSON U1 where
    gRpcFromJSON _opts _value = pure (Nothing, U1)

-- Arguments are expected to have JSON instances.
instance (FromJSON c) => GRpcFromJSON (K1 i c) where
    gRpcFromJSON _opts = fmap ((Nothing,) . K1) . parseJSON

--
-- Generic Machinery for serializing 'Response'
--

instance GRpcMethodName f => GRpcMethodName (D1 c f) where
    gRpcMethodName opts _ =
        gRpcMethodName opts (Proxy :: Proxy (f a))

instance (Constructor c) => GRpcMethodName (C1 c f) where
    gRpcMethodName opts _ =
        constructorTagModifier opts $ conName (undefined :: C1 c f a)

instance GRpcToJSON f => GRpcToJSON (D1 c f) where
    gRpcToJSON opts = gRpcToJSON opts . unM1

instance (Constructor c, GRpcToJSON f) => GRpcToJSON (C1 c f) where
    gRpcToJSON opts (M1 f) =
        Json.object [ fieldName .= gRpcToJSON opts f ]
      where
        fieldName = Json.fromString $ constructorTagModifier opts $ conName (undefined :: C1 c f a)

instance (GRpcToJSON f, GRpcToJSON g) => GRpcToJSON (f :+: g) where
    gRpcToJSON opts = \case
        L1 f -> gRpcToJSON opts f
        R1 g -> gRpcToJSON opts g

instance (GRpcToJSON f, GRpcToJSON g) => GRpcToJSON (f :*: g) where
    gRpcToJSON opts (f :*: g) =
        case (gRpcToJSON opts f, gRpcToJSON opts g) of
            (Json.Object ff, Json.Object gg) ->
                Json.Object (ff <> gg)
            (_, _) ->
                error "gRpcToJSON (f :*: g): impossible"

-- Constructor *must* use the record-selector-syntax. This is necessary in order
-- to map the Rpc arguments to their corresponding position in the constructor.
instance (Selector s, GRpcToJSON f) => GRpcToJSON (S1 s f) where
    gRpcToJSON opts (M1 f) =
        Json.object [ fieldName .= gRpcToJSON opts f ]
      where
        fieldName = Json.fromString $ fieldLabelModifier opts $ selName (undefined :: S1 s f a)

-- Arguments are expected to have JSON instances.
instance ToJSON c => GRpcToJSON (K1 i c) where
    gRpcToJSON _opts = toJSON . unK1

--
-- Validations (Internal)
--

parseKey
    :: Json.Object
    -> Json.Key
    -> Text
    -> Json.Parser ()
parseKey =
    parseKeyWith (==)

parseKeyWith
    :: (Text -> Text -> Bool)
    -> Json.Object
    -> Json.Key
    -> Text
    -> Json.Parser ()
parseKeyWith checkKey obj key expected =
    case Json.parseMaybe (.:? key) obj of
        Just Nothing ->
            fail
                $ prettyErrValidateKey
                $ ErrValidateKeyMissing
                $ ErrMissingKey key
        Nothing ->
            fail
                $ prettyErrValidateKey
                $ ErrValidateKeyInvalid
                $ ErrInvalidKey key (toJSON expected)
        Just (Just v) | not (checkKey expected v) ->
            fail
                $ prettyErrValidateKey
                $ ErrValidateKeyInvalid
                $ ErrInvalidKey key (toJSON expected)
        Just (Just{}) ->
            pure ()

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
