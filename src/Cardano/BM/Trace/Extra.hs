--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Cardano.BM.Trace.Extra
    ( withStdoutTracer
    ) where

import Prelude

import Cardano.BM.Backend.Switchboard
    ( Switchboard )
import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.LogItem
    ( LOContent (..), LogObject (..), LoggerName, mkLOMeta )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.BM.Setup
    ( setupTrace_, shutdown )
import Control.Exception
    ( bracket )
import Control.Monad.IO.Class
    ( MonadIO )
import Control.Tracer
    ( Tracer (..), traceWith )
import Data.Text
    ( Text )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.BackendKind as CM

-- | Acquire a tracer that automatically shutdown once the action is done via
-- bracket-style allocation.
withStdoutTracer
    :: forall msg a. (HasPrivacyAnnotation msg, HasSeverityAnnotation msg)
    => LoggerName
    -> Severity
    -> (msg -> Text)
    -> (Tracer IO msg -> IO a)
    -> IO a
withStdoutTracer name minSeverity transform between = do
    bracket before (shutdown . snd) (between . fst)
  where
    before :: IO (Tracer IO msg, Switchboard Text)
    before = do
        config <- defaultConfigStdout
        CM.setMinSeverity config minSeverity
        CM.setSetupBackends config [CM.KatipBK]
        (tr, sb) <- setupTrace_ config name
        pure (transformLogObject transform tr, sb)

-- | Tracer transformer which converts 'Trace m a' to 'Tracer m a' by wrapping
-- typed log messages into a 'LogObject'.
transformLogObject
    :: (MonadIO m, HasPrivacyAnnotation msg, HasSeverityAnnotation msg)
    => (msg -> Text)
    -> Tracer m (LoggerName, LogObject Text)
    -> Tracer m msg
transformLogObject transform tr = Tracer $ \a -> do
    traceWith tr . (mempty,) =<< LogObject
        <$> pure mempty
        <*> (mkLOMeta (getSeverityAnnotation a) (getPrivacyAnnotation a))
        <*> pure (LogMessage (transform a))
