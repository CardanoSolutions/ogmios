--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2020 KtorZ <matthias.benkort@gmail.com>
-- License: MPL-2.0
-- Stability: Stable
-- Portability: Unix
module Codec.Json.Wsp.Handler
    ( -- * Types
      Request (..)
    , Response (..)
    , serverFault
    , clientFault

      -- * Routing
    , Handler (..)
    , handle
    ) where

import Prelude

import Control.Applicative
    ( Alternative (..), (<|>) )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericToJSON )
import Data.ByteString
    ( ByteString )
import Data.Char
    ( toLower )
import Data.List
    ( foldl' )
import GHC.Generics

import qualified Data.Aeson as Json

--
-- Types
--

-- | Represent a JSON-WSP request (from a client to the server)
--
-- @since 1.0.0
data Request a = Request Mirror a
    deriving (Generic, Show)

-- | Represent a JSON-WSP response (from the server to a client)
--
-- @since 1.0.0
data Response a = Response Mirror a
    deriving (Generic, Show)

-- | Type alias for the optional mirror(ed) value in Request/Response
--
-- @since 1.0.0
type Mirror = Maybe Json.Value

-- | Types of fault as specified by the standard.
--
-- @since 1.0.0
data FaultCode
    = FaultIncompatible
    | FaultClient
    | FaultServer
    deriving (Generic, Show)

instance ToJSON FaultCode where
    toJSON = genericToJSON $ Json.defaultOptions
        { Json.constructorTagModifier = fmap toLower . drop 5 }

-- | Wrapper for a 'FaultCode'
--
-- @since 1.0.0
data Fault = Fault
    { faultCode :: FaultCode
    , faultString :: String
    } deriving (Generic, Show)

instance ToJSON Fault where
    toJSON = genericToJSON Json.defaultOptions

-- | Smart constructor for a client 'Fault'
--
-- @since 1.0.0
clientFault :: String -> Fault
clientFault = Fault FaultClient

-- | Smart constructor for a server 'Fault'
--
-- @since 1.0.0
serverFault :: String -> Fault
serverFault = Fault FaultServer

-- | A data-type to capture the logic to 'handle' any request.
--
-- @since 1.0.0
data Handler (m :: * -> *) a where
    Handler
        :: (FromJSON (Request req))
        => (req -> (res -> Response res) -> m a)
        -> Handler m a

-- | Try each handler in sequence. Returns 'Nothing' the request failed to match
-- any of the handler.
--
-- If matches, runs the corresponding handler and returns either a fault, or a
-- serialized WSP response.
--
-- @since 1.0.0
handle
    :: forall m a. Monad m
    => (ByteString -> m a)
        -- ^ Default action to perform when no handler is matching
    -> [Handler m a]
        -- ^ Known handlers / routes
    -> ByteString
        -- ^ Raw request bytes
    -> m a
handle whenMissing handlers bytes = do
    routes <- mapM (\(Handler action) -> tryHandler action) handlers
    maybe (whenMissing bytes) pure $ foldl' (<|>) Nothing routes
  where
    tryHandler
        :: (FromJSON (Request req))
        => (req -> (res -> Response res) -> m a)
        -> m (Maybe a)
    tryHandler action = case Json.decodeStrict bytes of
        Nothing -> pure Nothing
        Just (Request refl req) -> Just
            <$> action req (Response refl)
