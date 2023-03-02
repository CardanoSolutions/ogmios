--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2020 KtorZ <matthias.benkort@gmail.com>
-- License: MPL-2.0
-- Stability: Stable
-- Portability: Unix
module Codec.Json.Rpc.Handler
    ( -- * Types
      Request (..)
    , Response (..)
    , Mirror
    , Fault (..)
    , FaultCode (..)
    , parseError
    , invalidRequest
    , methodNotFound
    , invalidParams
    , internalError
    , serverError

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

-- | Represent a JSON-RPC request (from a client to the server)
--
-- @since 1.0.0
data Request a = Request Mirror a
    deriving (Generic, Show, Eq)

instance Functor Request where
    fmap fn (Request mirror a) = Request mirror (fn a)

-- | Represent a JSON-RPC response (from the server to a client)
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
    = FaultParseError
    | FaultInvalidRequest
    | FaultMethodNotFound
    | FaultInvalidParams
    | FaultInternalError
    | FaultServerError Int
    deriving (Generic, Show)

instance ToJSON FaultCode where
    toJSON = \case
        FaultParseError -> toJSON (negate @Int 32700)
        FaultInvalidRequest -> toJSON (negate @Int 32600)
        FaultMethodNotFound -> toJSON (negate @Int 32601)
        FaultInvalidParams -> toJSON (negate @Int 32602)
        FaultInternalError -> toJSON (negate @Int 32603)
        FaultServerError i -> toJSON i

-- | Wrapper for a 'FaultCode'
--
-- @since 1.0.0
data Fault = Fault
    { faultMirror :: Mirror
    , faultCode :: FaultCode
    , faultMessage :: String
    } deriving (Generic, Show)

instance ToJSON Fault where
    toJSON = genericToJSON Json.defaultOptions
        { Json.fieldLabelModifier = fmap toLower . drop 5 }

-- | Smart constructor for a 'Fault'
--
-- @since 1.0.0
parseError :: Mirror -> String -> Fault
parseError mirror = Fault mirror FaultParseError

-- | Smart constructor for a 'Fault'
--
-- @since 1.0.0
invalidRequest :: Mirror -> String -> Fault
invalidRequest mirror = Fault mirror FaultInvalidRequest

-- | Smart constructor for a 'Fault'
--
-- @since 1.0.0
methodNotFound :: Mirror -> String -> Fault
methodNotFound mirror = Fault mirror FaultMethodNotFound

-- | Smart constructor for a 'Fault'
--
-- @since 1.0.0
invalidParams :: Mirror -> String -> Fault
invalidParams mirror = Fault mirror FaultInvalidParams

-- | Smart constructor for a 'Fault'
--
-- @since 1.0.0
internalError :: Mirror -> String -> Fault
internalError mirror = Fault mirror FaultInternalError

-- | Smart constructor for a 'Fault'
--
-- @since 1.0.0
serverError :: Mirror -> Int -> String -> Fault
serverError mirror code = Fault mirror (FaultServerError code)

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
-- @since 1.0.0
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
