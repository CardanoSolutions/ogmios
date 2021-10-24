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
    , Mirror
    , Fault (..)
    , FaultCode (..)
    , serverFault
    , clientFault

      -- * Routing
    , Handler (..)
    , match
    ) where

import Prelude

import Data.Aeson
    ( ToJSON (..), genericToJSON )
import Data.ByteString
    ( ByteString )
import Data.Char
    ( toLower )
import Data.Kind
    ( Type )
import GHC.Generics
    ( Generic )

import qualified Data.Aeson as Json

--
-- Types
--

-- | Represent a JSON-WSP request (from a client to the server)
--
-- @since 1.0.0
data Request a = Request Mirror a
    deriving (Generic, Show, Eq)

instance Functor Request where
    fmap fn (Request mirror a) = Request mirror (fn a)

-- | Represent a JSON-WSP response (from the server to a client)
--
-- @since 1.0.0
data Response a = Response Mirror a
    deriving (Generic, Show, Eq)

instance Functor Response where
    fmap fn (Response mirror a) = Response mirror (fn a)

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
    { faultMirror :: Mirror
    , faultCode :: FaultCode
    , faultString :: String
    } deriving (Generic, Show)

instance ToJSON Fault where
    toJSON = genericToJSON Json.defaultOptions
        { Json.fieldLabelModifier = fmap toLower . drop 5 }

-- | Smart constructor for a client 'Fault'
--
-- @since 2.0.0
clientFault :: Mirror -> String -> Fault
clientFault mirror = Fault mirror FaultClient

-- | Smart constructor for a server 'Fault'
--
-- @since 2.0.0
serverFault :: Mirror -> String -> Fault
serverFault mirror = Fault mirror FaultServer

-- | A data-type to capture the logic to 'handle' any request.
--
-- @since 1.0.0
data Handler (m :: Type -> Type) a where
    Handler
        :: (ByteString -> Maybe (Request req))
        -> (req -> (res -> Response res) -> (FaultCode -> String -> Fault) -> m a)
        -> Handler m a

type Matched m = (ByteString, m ())

-- | Try parsing a given 'ByteString' into a request Handler. Handlers are
-- tried alternatively in order; The most frequent handlers must therefore be
-- placed first in the list.
--
-- It also returns the handler that was matched if any, which allows for
-- re-running it if the same input is presented (without having to decode and
-- look again for a route). This is useful when the same handler gets repeatedly
-- triggered by identical messages.
--
-- @since 2.0.0
match
    :: Monad m
    => ByteString
    -> m ()
    -> [Handler m ()]
    -> m (Maybe (Matched m))
match bytes defaultHandler = \case
    [] -> Nothing <$ defaultHandler
    (Handler decode next):q ->
        case decode bytes of
            Just (Request refl req) -> do
                let matched = next req (Response refl) (Fault refl)
                Just (bytes, matched) <$ matched

            Nothing ->
                match bytes defaultHandler q
