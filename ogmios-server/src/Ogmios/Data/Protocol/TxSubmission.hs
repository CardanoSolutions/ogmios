--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

-- NOTE:
-- This module uses partial record field accessor to automatically derive
-- JSON instances from the generic data-type structure. The partial fields are
-- otherwise unused.
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

-- NOTE:
-- Needed to derive 'ToJSON' and 'Show' instances for 'SubmitResult'.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.Data.Protocol.TxSubmission
    ( SubmitTx (..)
    , SubmitTxResponse (..)
    , parserVoid
    ) where

import Prelude

import Ogmios.Data.Protocol
    ( MethodName )

import Control.Monad
    ( void )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), (.=) )
import Data.Proxy
    ( Proxy (..) )
import Data.Void
    ( Void )
import GHC.Generics
    ( Generic, Rep )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..) )

import qualified Codec.Json.Wsp as Wsp
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json

--
-- SubmitTx
--

data SubmitTx tx
    = SubmitTx { bytes :: tx }
    deriving (Generic, Show)

instance
    ( FromJSON tx
    ) => FromJSON (Wsp.Request (SubmitTx tx))
  where
    parseJSON = Wsp.genericFromJSON Wsp.defaultOptions

--
-- SubmitTxResponse
--

data SubmitTxResponse e
    = SubmitTxResponse { error :: SubmitResult e }
    deriving (Generic, Show)

instance
    ( Show e
    ) => Show (SubmitResult e)
  where
    show = \case
        SubmitSuccess ->"SubmitSuccess"
        SubmitFail e -> "SubmitFail " <> show e

instance
    ( ToJSON e
    ) => ToJSON (Wsp.Response (SubmitTxResponse e))
  where
    toJSON = Wsp.genericToJSON Wsp.defaultOptions proxy
      where proxy = Proxy @(Wsp.Request (SubmitTx _))

instance
    ( ToJSON e
    ) => ToJSON (SubmitResult e)
  where
    toJSON = \case
        SubmitSuccess -> Json.String "SubmitSuccess"
        SubmitFail e  -> Json.object [ "SubmitFail" .= e ]

--
-- Error Handling
--

-- | Default handler attempting to provide user-friendly error message. See also
-- 'Ogmios.Data.Protocol#onUnmatchedMessage'.
--
-- NOTE: This function is ambiguous in 'tx' and requires a type application.
parserVoid
    :: forall tx. (FromJSON tx)
    => MethodName
    -> Json.Value
    -> Json.Parser Void
parserVoid methodName json = do
    if methodName == Wsp.gWSPMethodName (Proxy @(Rep (SubmitTx tx) _)) then
        void $ parseJSON @(Wsp.Request (SubmitTx tx)) json
    else
        pure ()

    fail "unknown method in 'methodname' (beware names are case-sensitive). \
         \Expected: 'SubmitTx'."
