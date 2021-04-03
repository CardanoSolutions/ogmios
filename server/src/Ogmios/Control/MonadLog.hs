--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE UndecidableInstances #-}

module Ogmios.Control.MonadLog
    ( -- * Class
      MonadLog (..)
    , Logger

      -- * Tracer
    , Tracer
    , natTracer
    , nullTracer
    , contramap
    , traceWith

      -- * Severity
    , Severity (..)
    , HasSeverityAnnotation (..)

      -- * Instantiation
    , LoggerName
    , withStdoutTracer
    ) where

import Relude

import Ogmios.Control.Exception
    ( MonadThrow (..) )

import Cardano.BM.Backend.Switchboard
    ( Switchboard )
import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.LogItem
    ( LOContent (..)
    , LogObject (..)
    , LoggerName
    , PrivacyAnnotation (..)
    , mkLOMeta
    )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasSeverityAnnotation (..) )
import Cardano.BM.Setup
    ( setupTrace_, shutdown )
import Control.Tracer
    ( Tracer (..), natTracer, nullTracer, traceWith )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.BackendKind as CM

class Monad m => MonadLog (m :: Type -> Type) where
    logWith :: Logger msg -> msg -> m ()

type Logger = Tracer IO

instance MonadLog IO where
    logWith = traceWith

instance MonadLog m => MonadLog (ReaderT env m) where
    logWith tr = lift . logWith tr

-- | Acquire a tracer that automatically shutdown once the action is done via
-- bracket-style allocation.
withStdoutTracer
    :: forall m msg a. (MonadIO m, HasSeverityAnnotation msg)
    => LoggerName
    -> Severity
    -> (msg -> Text)
    -> (Tracer m msg -> IO a)
    -> IO a
withStdoutTracer name minSeverity transform between = do
    bracket before after (between . natTracer liftIO . fst)
  where
    before :: IO (Tracer IO msg, Switchboard Text)
    before = do
        config <- defaultConfigStdout
        CM.setMinSeverity config minSeverity
        CM.setSetupBackends config [CM.KatipBK]
        (tr, sb) <- setupTrace_ config name
        pure (transformLogObject transform tr, sb)

    after :: (Tracer IO msg, Switchboard Text) -> IO ()
    after = shutdown . snd

-- | Tracer transformer which converts 'Trace m a' to 'Tracer m a' by wrapping
-- typed log messages into a 'LogObject'.
transformLogObject
    :: (MonadIO m, HasSeverityAnnotation msg)
    => (msg -> Text)
    -> Tracer m (LoggerName, LogObject Text)
    -> Tracer m msg
transformLogObject transform tr = Tracer $ \a -> do
    traceWith tr . (mempty,) =<< LogObject mempty
        <$> (mkLOMeta (getSeverityAnnotation a) Public)
        <*> pure (LogMessage (transform a))
