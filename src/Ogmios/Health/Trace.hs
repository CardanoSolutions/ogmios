--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE LambdaCase #-}

module Ogmios.Health.Trace
    ( TraceHealth (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )

newtype TraceHealth s
    = HealthTick { status :: s }
    deriving Show

instance HasPrivacyAnnotation (TraceHealth s)
instance HasSeverityAnnotation (TraceHealth s) where
    getSeverityAnnotation = \case
        HealthTick{} -> Info
