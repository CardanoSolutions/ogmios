--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.Metrics.Trace
    ( TraceMetrics (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Data.Text
    ( Text )

data TraceMetrics where
    MetricsRuntimeStatsDisabled
        :: { recommendation :: Text }
        -> TraceMetrics

deriving instance Show TraceMetrics

instance HasPrivacyAnnotation TraceMetrics
instance HasSeverityAnnotation TraceMetrics where
    getSeverityAnnotation = \case
        MetricsRuntimeStatsDisabled{} -> Warning
