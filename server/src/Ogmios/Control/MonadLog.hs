--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE UndecidableInstances #-}

module Ogmios.Control.MonadLog
    ( -- * Class
      MonadLog (..)
    , Logger
    , ToObject

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

import Ogmios.Prelude

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
    ( HasSeverityAnnotation (..), ToObject (..), TracingVerbosity (..) )
import Cardano.BM.Setup
    ( setupTrace_, shutdown )
import Control.Tracer
    ( Tracer (..), natTracer, nullTracer, traceWith )
import Data.Aeson
    ( FromJSON, ToJSON (..) )

import qualified Cardano.BM.Configuration.Model as CM

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
    :: forall m msg a. (MonadIO m, ToJSON msg, FromJSON msg, ToObject msg, HasSeverityAnnotation msg)
    => LoggerName
    -> (Text, Text)
    -> Severity
    -> (Tracer m msg -> IO a)
    -> IO a
withStdoutTracer name (version, revision) minSeverity between = do
    bracket before after (between . natTracer liftIO . fst)
  where
    before :: IO (Tracer IO msg, Switchboard msg)
    before = do
        config <- defaultConfigStdout
        CM.setMinSeverity config minSeverity
        CM.setDefaultScribes config ["StdoutSK::json"]
        CM.setTextOption config "appversion" version
        CM.setTextOption config "appcommit" revision
        (tr, sb) <- setupTrace_ config name
        pure (transformLogObject tr, sb)

    after :: (tracer, Switchboard msg) -> IO ()
    after = shutdown . snd

-- | Tracer transformer which converts 'Trace m a' to 'Tracer m a' by wrapping
-- typed log messages into a 'LogObject'.
transformLogObject
    :: (MonadIO m, HasSeverityAnnotation msg, ToObject msg)
    => Tracer m (LoggerName, LogObject msg)
    -> Tracer m msg
transformLogObject tr = Tracer $ \msg -> do
    traceWith tr . (mempty,) =<< LogObject mempty
        <$> (mkLOMeta (getSeverityAnnotation msg) Public)
        <*> pure (LogStructured (toObject MaximalVerbosity msg))
