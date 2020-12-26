--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

-- NOTE:
-- This module uses partial record field accessor to automatically derive
-- JSON instances from the generic data-type structure. The partial fields are
-- otherwise unused.
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.Data.Protocol.ChainSync
    ( FindIntersect (..)
    , FindIntersectResponse (..)
    , RequestNext (..)
    , RequestNextResponse (..)
    , parserVoid
    ) where

import Prelude

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
    ( Point (..), Tip (..) )

import qualified Codec.Json.Wsp as Wsp
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json

--
-- FindIntersect
--

data FindIntersect block
    = FindIntersect { points :: [Point block] }
    deriving (Generic, Show)

instance
    ( FromJSON (Point block)
    ) => FromJSON (Wsp.Request (FindIntersect block))
  where
    parseJSON = Wsp.genericFromJSON Wsp.defaultOptions

--
-- FindIntersectResponse
--

data FindIntersectResponse block
    = IntersectionFound { point :: Point block, tip :: Tip block }
    | IntersectionNotFound { tip :: Tip block }
    deriving (Generic, Show)

instance
    ( ToJSON (Point block)
    , ToJSON (Tip block)
    ) => ToJSON (Wsp.Response (FindIntersectResponse block))
  where
    toJSON = Wsp.genericToJSON Wsp.defaultOptions proxy
      where proxy = Proxy @(Wsp.Request (FindIntersect block))

--
-- Requestnext
--

data RequestNext
    = RequestNext
    deriving (Generic, Show)

instance FromJSON (Wsp.Request RequestNext)
  where
    parseJSON = Wsp.genericFromJSON Wsp.defaultOptions

--
-- RequestNextResponse
--

data RequestNextResponse block
    = RollForward { block :: block, tip :: Tip block }
    | RollBackward { point :: Point block, tip :: Tip block }
    deriving (Generic, Show)

instance
    ( ToJSON block
    , ToJSON (Tip block)
    , ToJSON (Point block)
    ) => ToJSON (Wsp.Response (RequestNextResponse block))
  where
    toJSON = Wsp.genericToJSON Wsp.defaultOptions proxy
      where proxy = Proxy @(Wsp.Request RequestNext)

--
-- Error Handling
--

-- | Default handler attempting to provide user-friendly error message. See also
-- 'Ogmios.Data.Protocol#onUnmatchedMessage'.
--
-- NOTE: This function is ambiguous in 'block' and requires a type application.
parserVoid
    :: forall block. (FromJSON (Point block))
    => MethodName
    -> Json.Value
    -> Json.Parser Void
parserVoid methodName json = do
    if | methodName == Wsp.gWSPMethodName (Proxy @(Rep (FindIntersect block) _)) ->
            void $ parseJSON @(Wsp.Request (FindIntersect block)) json
       | methodName == Wsp.gWSPMethodName (Proxy @(Rep RequestNext _)) ->
            void $ parseJSON @(Wsp.Request RequestNext) json
       | otherwise ->
            pure ()

    fail "unknown method in 'methodname' (beware names are case-sensitive). \
         \Expected either: 'FindIntersect' or 'RequestNext'."
