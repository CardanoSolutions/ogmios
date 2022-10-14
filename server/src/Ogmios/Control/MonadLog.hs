--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Control.MonadLog
    ( -- * Class
      MonadLog (..)
    , Logger

      -- * Severity
    , Severity (..)
    , HasSeverityAnnotation (..)
    , getSeverityAnnotation'

      -- * Tracer
    , Tracer
    , natTracer
    , nullTracer
    , contramap
    , traceWith

      -- * Tracers
    , TracerDefinition(..)
    , type TracerHKD
    , defaultTracers
    , withStdoutTracers
    ) where

import Ogmios.Prelude

import Control.Concurrent
    ( myThreadId
    )
import Control.Monad.IOSim
    ( IOSim
    )
import Control.Tracer
    ( Tracer (..)
    , natTracer
    , nullTracer
    , traceWith
    )
import Data.Aeson
    ( ToJSON (..)
    , toEncoding
    )
import Data.Aeson.Encoding
    ( Encoding
    , encodingToLazyByteString
    , pair
    , pairs
    )
import Data.Char
    ( isLower
    )
import Data.Generics.Tracers
    ( IsRecordOfTracers
    , SomeMsg (..)
    , TracerDefinition (..)
    , TracerHKD
    , configureTracers
    , defaultTracers
    )
import Data.Severity
    ( HasSeverityAnnotation (..)
    , Severity (..)
    )
import Ogmios.Control.MonadClock
    ( getCurrentTime
    )
import Ogmios.Control.MonadSTM
    ( newTMVarIO
    , withTMVar
    )
import System.IO
    ( BufferMode (..)
    , hSetBuffering
    , hSetEncoding
    , utf8
    )

import qualified Cardano.BM.Data.Severity as BM
import qualified Cardano.BM.Data.Tracer as BM
import qualified Data.ByteString.Lazy.Char8 as BL8

class Monad m => MonadLog (m :: Type -> Type) where
    logWith :: Logger msg -> msg -> m ()

type Logger = Tracer IO

instance MonadLog IO where
    logWith = traceWith

-- TODO: This instance is bonkers, but this is because of the over-specialized
-- Logger = Tracer IO. Ideally, MonadLog should work with _any_
instance MonadLog (IOSim s) where
    logWith _ _ = pure ()

instance MonadLog m => MonadLog (ReaderT env m) where
    logWith tr = lift . logWith tr

type AppVersion = Text

-- | Acquire and configure multiple tracers which outputs structured JSON on the
-- standard output. The tracer is concurrent-safe but none buffered, while it is
-- okay for a vast majority of applications, it also relies on a simple
-- implementation and does not perform any caching or hardcore optimizations;
-- For example timestamps are computed on-the-fly for every log messages.
--
withStdoutTracers
    :: forall tracers. (IsRecordOfTracers tracers IO)
    => AppVersion
        -- ^ Extra information to embed in the logging envelope.
    -> tracers IO 'MinSeverities
        -- ^ A configuration of tracers.
    -> (tracers IO 'Concrete -> IO ())
        -- ^ Callback with acquired and configured tracers.
    -> IO ()
withStdoutTracers version tracers action = do
    hSetBuffering stdout LineBuffering
    hSetEncoding stdout utf8
    lock <- newTMVarIO ()
    action (configureTracers tracers (tracer lock))
  where
    tracer lock = Tracer $ \(SomeMsg minSeverity tracerName msg) -> do
        let severity = getSeverityAnnotation msg
        when (severity >= minSeverity) $ liftIO $ withTMVar lock $ \() -> do
            mkEnvelop msg severity tracerName >>= liftIO . BL8.putStrLn . encodingToLazyByteString

    mkEnvelop :: forall m msg. (ToJSON msg, MonadIO m) => msg -> Severity -> String -> m Encoding
    mkEnvelop msg severity tracerName = do
        let context = fromString (dropWhile isLower tracerName)
        timestamp <- liftIO getCurrentTime
        threadId <- drop 9 . show <$> liftIO myThreadId
        pure $ pairs $ mempty
            <> pair "severity"  (toEncoding severity)
            <> pair "timestamp" (toEncoding timestamp)
            <> pair "thread"    (toEncoding threadId)
            <> pair "message"   (pairs $ pair context (toEncoding msg))
            <> pair "version"   (toEncoding version)

-- | Working around iohk-monitoring. Ogmios doesn't use 'Severity' from
-- iohk-monitoring because the JSON instances are bonkers and, it defines way
-- too many severity levels. Still, there are places where we need to convert
-- from existing severity types (when wrapping traces from ouroboros-network in
-- particular).
getSeverityAnnotation' :: BM.HasSeverityAnnotation msg => msg -> Severity
getSeverityAnnotation' =
    toSeverity . BM.getSeverityAnnotation
  where
    toSeverity :: BM.Severity -> Severity
    toSeverity = \case
        BM.Debug -> Debug
        BM.Info -> Info
        BM.Notice -> Notice
        BM.Warning -> Warning
        BM.Error -> Error
        BM.Critical -> Error
        BM.Alert -> Error
        BM.Emergency -> Error
