--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Severity
    ( Severity (..)
    , HasSeverityAnnotation (..)
    ) where

import Prelude

import Data.Aeson
    ( FromJSON, ToJSON )
import GHC.Generics
    ( Generic )

-- | Define a log-level severity.
--
-- +----------+----------------------------------------------------------------------------------+
-- | Severity | Semantic                                                                         |
-- +==========+==================================================================================+
-- | Debug	  | Messages that contain information normally of use only when debugging a program. |
-- | Info	  | Informational messages.                                                          |
-- | Notice	  | Normal but significant conditions.                                               |
-- | Warning  | A situation that may become an error.                                            |
-- | Error	  | Unexpected problem.                                                              |
-- +----------+----------------------------------------------------------------------------------+
--
data Severity
    = Debug
    | Info
    | Notice
    | Warning
    | Error
    deriving stock (Generic, Eq, Ord, Enum, Bounded, Show, Read)
    deriving anyclass (ToJSON, FromJSON)

-- | Associate a 'Severity' to a typed log message.
--
--     data MyLog = MyLog
--     instance HasSeverityAnnotation MyLog where
--         getSeverityAnnotation = \case
--             MyLog -> Info
--
class HasSeverityAnnotation a where
    getSeverityAnnotation :: a -> Severity
