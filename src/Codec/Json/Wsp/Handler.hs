--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

data Request a = Request Mirror a
    deriving (Generic, Show)

data Response a = Response Mirror a
    deriving (Generic, Show)

type Mirror = Maybe Json.Value

data FaultCode
    = FaultIncompatible
    | FaultClient
    | FaultServer
    deriving (Generic, Show)

instance ToJSON FaultCode where
    toJSON = genericToJSON $ Json.defaultOptions
        { Json.constructorTagModifier = fmap toLower . drop 5 }

data Fault = Fault
    { faultCode :: FaultCode
    , faultString :: String
    } deriving (Generic, Show)

instance ToJSON Fault where
    toJSON = genericToJSON Json.defaultOptions

clientFault :: String -> Fault
clientFault = Fault FaultClient

serverFault :: String -> Fault
serverFault = Fault FaultServer

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
