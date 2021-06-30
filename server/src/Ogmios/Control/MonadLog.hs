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
    , withStdoutTracer
    ) where

import Ogmios.Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasSeverityAnnotation (..) )
import Control.Tracer
    ( Tracer (..), natTracer, nullTracer, traceWith )
import Data.Aeson
    ( ToJSON (..) )
import Data.Aeson.Encoding
    ( pair, pairs )
import GHC.Conc
    ( myThreadId )
import Ogmios.Control.MonadClock
    ( getCurrentTime )
import Ogmios.Control.MonadSTM
    ( newTMVarIO, withTMVar )
import System.IO
    ( BufferMode (..), hSetBuffering, hSetEncoding, utf8 )

import qualified Data.Aeson.Encoding as Json
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

class Monad m => MonadLog (m :: Type -> Type) where
    logWith :: Logger msg -> msg -> m ()

type Logger = Tracer IO
type AppVersion = Text

instance MonadLog IO where
    logWith = traceWith

instance MonadLog m => MonadLog (ReaderT env m) where
    logWith tr = lift . logWith tr

-- | Acquire a tracer to use across the app lifecycle.
withStdoutTracer
    :: forall m msg a. (MonadIO m, ToJSON msg, HasSeverityAnnotation msg)
    => AppVersion
    -> Severity
    -> (Tracer m msg -> IO a)
    -> IO a
withStdoutTracer version minSeverity action = do
    hSetBuffering stdout LineBuffering
    hSetEncoding stdout utf8
    lock <- newTMVarIO ()
    action (tracer lock)
  where
    tracer lock = Tracer $ \msg -> do
        let severity = getSeverityAnnotation msg
        when (severity >= minSeverity) $ liftIO $ withTMVar lock $ \() -> do
            mkEnvelop msg severity >>= liftIO . TIO.putStrLn . decodeUtf8 . Json.encodingToLazyByteString

    mkEnvelop msg severity = do
        timestamp <- liftIO getCurrentTime
        threadId <- T.drop 9 . show <$> liftIO myThreadId
        pure $ pairs $ mempty
            <> pair "severity"  (toEncoding severity)
            <> pair "timestamp" (toEncoding timestamp)
            <> pair "thread"    (toEncoding threadId)
            <> pair "message"   (toEncoding msg)
            <> pair "version"   (toEncoding version)
