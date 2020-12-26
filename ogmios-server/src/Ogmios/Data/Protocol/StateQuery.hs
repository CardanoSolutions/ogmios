--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- NOTE:
-- This module uses partial record field accessor to automatically derive
-- JSON instances from the generic data-type structure. The partial fields are
-- otherwise unused.
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.Data.Protocol.StateQuery
    ( Acquire (..)
    , AcquireResponse (..)
    , Release (..)
    , Query (..)
    , QueryResponse (..)
    , parserVoid
    ) where

import Prelude

import Ogmios.Data.Json
    ( SomeQuery (..) )
import Ogmios.Data.Protocol
    ( MethodName )

import Control.Monad
    ( void )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Void
    ( Void )
import GHC.Generics
    ( Generic, Rep )
import Ouroboros.Network.Block
    ( Point (..) )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure )

import qualified Codec.Json.Wsp as Wsp
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json

--
-- Acquire
--

data Acquire point
    = Acquire { point :: point }
    deriving (Generic, Show)

instance
    ( FromJSON (Point block)
    ) => FromJSON (Wsp.Request (Acquire (Point block)))
  where
    parseJSON = Wsp.genericFromJSON Wsp.defaultOptions

--
-- AcquireResponse
--

data AcquireResponse point
    = AcquireSuccess { acquired :: point }
    | AcquireFailed { failure :: AcquireFailure }
    deriving (Generic, Show)

instance
    ( ToJSON AcquireFailure
    , ToJSON (Point block)
    ) => ToJSON (Wsp.Response (AcquireResponse (Point block)))
  where
    toJSON = Wsp.genericToJSON Wsp.defaultOptions proxy
      where proxy = Proxy @(Wsp.Request (Acquire _))

--
-- Release
--

data Release
    = Release
    deriving (Generic, Show)

instance FromJSON (Wsp.Request Release)
  where
    parseJSON = Wsp.genericFromJSON Wsp.defaultOptions

--
-- Query
--

data Query block = Query { query :: SomeQuery Maybe block }
    deriving (Generic)

instance
    ( FromJSON (SomeQuery Maybe block)
    ) => FromJSON (Wsp.Request (Query block))
  where
    parseJSON = Wsp.genericFromJSON Wsp.defaultOptions

--
-- QueryResponse
--

newtype QueryResponse =
    QueryResponse { unQueryResponse :: Json.Value }
    deriving (Generic, Show)

instance ToJSON (Wsp.Response QueryResponse)
  where
    toJSON = Wsp.mkResponse Wsp.defaultOptions unQueryResponse proxy
      where proxy = Proxy @(Wsp.Request (Query _))

--
-- Error Handling
--

-- | Default handler attempting to provide user-friendly error message. See also
-- 'Ogmios.Data.Protocol#onUnmatchedMessage'.
--
-- NOTE: This function is ambiguous in 'block' and requires a type application.
parserVoid
    :: forall block.
        ( FromJSON (SomeQuery Maybe block)
        , FromJSON (Point block)
        )
    => MethodName
    -> Json.Value
    -> Json.Parser Void
parserVoid methodName json = do
    if | methodName == Wsp.gWSPMethodName (Proxy @(Rep (Acquire (Point block)) _)) ->
            void $ parseJSON @(Wsp.Request (Acquire (Point block))) json
       | methodName == Wsp.gWSPMethodName (Proxy @(Rep Release _)) ->
            void $ parseJSON @(Wsp.Request Release) json
       | methodName == Wsp.gWSPMethodName (Proxy @(Rep (Query block) _)) ->
            void $ parseJSON @(Wsp.Request (Query block)) json
       | otherwise ->
          pure ()

    fail "unknown method in 'methodname' (beware names are case-sensitive). \
         \Expected one of: 'Acquire', 'Release' or 'Query'."
