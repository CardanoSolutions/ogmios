--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE LambdaCase #-}

module Cardano.Byron.Constants.Trace
    ( TraceLookup (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )

data TraceLookup
    = LookupDefaultNetwork { usingDefault :: (String, NetworkMagic) }
    | LookupUserDefinedNetwork { using :: (String, NetworkMagic) }
    | LookupInvalidNetwork { expectedOneOf :: [String] }
    deriving (Show)

instance HasPrivacyAnnotation TraceLookup
instance HasSeverityAnnotation TraceLookup where
    getSeverityAnnotation = \case
        LookupDefaultNetwork{} -> Info
        LookupUserDefinedNetwork{} -> Info
        LookupInvalidNetwork{} -> Critical
