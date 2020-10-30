--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.Health.Trace
    ( TraceHealth (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Control.Exception
    ( SomeException )
import Data.Time.Clock
    ( NominalDiffTime )
import Ogmios.Metrics.Trace
    ( TraceMetrics )

data TraceHealth s where
    HealthTick
        :: { status :: s }
        -> TraceHealth s

    HealthFailedToConnect
        :: { socket :: FilePath, retryingIn :: NominalDiffTime }
        -> TraceHealth s

    HealthMetrics
        :: { metrics :: TraceMetrics }
        -> TraceHealth s

    HealthUnknownException
        :: { exception :: SomeException }
        -> TraceHealth s

deriving instance Show s => Show (TraceHealth s)

instance HasPrivacyAnnotation (TraceHealth s)
instance HasSeverityAnnotation (TraceHealth s) where
    getSeverityAnnotation = \case
        HealthMetrics e -> getSeverityAnnotation e
        HealthTick{} -> Info
        HealthFailedToConnect{} -> Warning
        HealthUnknownException{} -> Error
