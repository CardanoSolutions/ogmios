--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios
    ( -- * App
      App (..)
    , application
    , runWith

    -- * Environment
    , Env (..)
    , newEnvironment
    -- ** Command & Options
    , Command (..)
    , Options (..)
    , NetworkParameters (..)
    , parseOptions
    -- ** Version
    , version

    -- * Logging
    , TraceOgmios (..)
    , withStdoutTracer
    , structuredJson
    ) where

import Ogmios.Prelude

import Ogmios.App.Health
    ( Health
    , TraceHealth
    , connectHealthCheckClient
    , emptyHealth
    , newHealthCheckClient
    )
import Ogmios.App.Metrics
    ( RuntimeStats, Sampler, Sensors, TraceMetrics, newSampler, newSensors )
import Ogmios.App.Options
    ( Command (..), NetworkParameters (..), Options (..), parseOptions )
import Ogmios.App.Server
    ( TraceServer, connectHybridServer )
import Ogmios.App.Server.Http
    ( mkHttpApp )
import Ogmios.App.Server.WebSocket
    ( TraceWebSocket, newWebSocketApp )
import Ogmios.App.Version
    ( version )
import Ogmios.Control.Exception
    ( MonadCatch, MonadMask, MonadThrow )
import Ogmios.Control.MonadAsync
    ( MonadAsync (..), MonadFork, MonadThread )
import Ogmios.Control.MonadClock
    ( MonadClock, getCurrentTime, withDebouncer, _10s )
import Ogmios.Control.MonadLog
    ( HasSeverityAnnotation (..)
    , Logger
    , MonadLog (..)
    , Severity (..)
    , structuredJson
    , withStdoutTracer
    )
import Ogmios.Control.MonadMetrics
    ( MonadMetrics )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..), TVar, newTVar )
import Ogmios.Control.MonadWebSocket
    ( MonadWebSocket )
import Ogmios.Data.Json
    ( ToJSON )

import Cardano.Network.Protocol.NodeToClient
    ( Block )
import Control.Monad.Class.MonadST
    ( MonadST )
import Data.Aeson.Via.Show
    ( GenericToJsonViaShow (..), ViaJson (..) )

--
-- App
--

-- | Main application monad.
newtype App a = App
    { unApp :: ReaderT (Env App) IO a
    } deriving
        ( Functor, Applicative, Monad
        , MonadReader (Env App)
        , MonadIO
        , MonadLog, MonadMetrics
        , MonadWebSocket
        , MonadClock
        , MonadSTM, MonadST
        , MonadAsync, MonadThread, MonadFork
        , MonadThrow, MonadCatch, MonadMask
        )

-- | Application runner with an instantiated environment. See 'newEnvironment'.
runWith :: forall a. App a -> Env App -> IO a
runWith app = runReaderT (unApp app)

-- | Ogmios, where everything gets stitched together.
application :: Logger TraceOgmios -> App ()
application tr = withDebouncer _10s $ \debouncer -> do
    env@Env{network} <- ask
    logWith tr (OgmiosNetwork $ ViaJson network)

    healthCheckClient <- newHealthCheckClient (contramap (OgmiosHealth . ViaJson) tr) debouncer

    webSocketApp <- newWebSocketApp (contramap (OgmiosWebSocket . ViaJson) tr) (`runWith` env)
    httpApp      <- mkHttpApp @_ @_ @Block (`runWith` env)

    concurrently_
        (connectHealthCheckClient
            (contramap (OgmiosHealth . ViaJson) tr) (`runWith` env) healthCheckClient)
        (connectHybridServer
            (contramap (OgmiosServer . ViaJson) tr) webSocketApp httpApp)

--
-- Environment
--

-- | Environment of the application, carrying around what's needed for the
-- application to run.
data Env (m :: Type -> Type) = Env
    { health  :: !(TVar m (Health Block))
    , sensors :: !(Sensors m)
    , sampler :: !(Sampler RuntimeStats m)
    , network :: !NetworkParameters
    , options :: !Options
    } deriving (Generic)

newEnvironment
    :: Logger TraceOgmios
    -> NetworkParameters
    -> Options
    -> IO (Env App)
newEnvironment tr network options = do
    health  <- getCurrentTime >>= atomically . newTVar . emptyHealth
    sensors <- newSensors
    sampler <- newSampler (contramap (OgmiosMetrics . ViaJson) tr)
    pure $ Env{health,sensors,sampler,network,options}

--
-- Logging
--

data TraceOgmios where
    OgmiosHealth
        :: { healthCheck :: ViaJson (TraceHealth (Health Block)) }
        -> TraceOgmios

    OgmiosMetrics
        :: { metrics :: ViaJson TraceMetrics }
        -> TraceOgmios

    OgmiosWebSocket
        :: { webSocket :: ViaJson TraceWebSocket }
        -> TraceOgmios

    OgmiosServer
        :: { server :: ViaJson TraceServer }
        -> TraceOgmios

    OgmiosNetwork
        :: { networkParameters :: ViaJson NetworkParameters }
        -> TraceOgmios
    deriving stock (Generic, Show)
    deriving ToJSON via GenericToJsonViaShow TraceOgmios

instance HasSeverityAnnotation TraceOgmios where
    getSeverityAnnotation = \case
        OgmiosHealth (ViaJson msg)  -> getSeverityAnnotation msg
        OgmiosMetrics (ViaJson msg) -> getSeverityAnnotation msg
        OgmiosWebSocket (ViaJson msg)  -> getSeverityAnnotation msg
        OgmiosServer (ViaJson msg)     -> getSeverityAnnotation msg
        OgmiosNetwork{}      -> Info
