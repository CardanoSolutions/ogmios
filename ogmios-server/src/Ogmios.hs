--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios
    ( App (..)
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
    ) where

import Prelude hiding
    ( max, min )

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
    ( MonadClock )
import Ogmios.Control.MonadLog
    ( HasSeverityAnnotation (..)
    , Logger
    , MonadLog (..)
    , Severity (..)
    , contramap
    , withStdoutTracer
    )
import Ogmios.Control.MonadMetrics
    ( MonadMetrics )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..), newTVar )
import Ogmios.Control.MonadWebSocket
    ( MonadWebSocket )

import Cardano.Network.Protocol.NodeToClient
    ( Block )
import Control.Concurrent.STM.TVar
    ( TVar )
import Control.Monad.Class.MonadST
    ( MonadST )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Reader
    ( MonadReader, ReaderT (..), ask, asks )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Product.Typed
    ( typed )
import GHC.Generics
    ( Generic )

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
application tr = do
    env@Env{network} <- ask
    logWith tr (OgmiosNetwork network)

    healthCheckClient <- newHealthCheckClient (contramap OgmiosHealth tr)

    webSocketApp <- newWebSocketApp (contramap OgmiosWebSocket tr) (`runWith` env)
    httpApp      <- asks (mkHttpApp @(Health Block) <$> view typed)

    concurrently_
        (connectHealthCheckClient
            (contramap OgmiosHealth tr) (`runWith` env) healthCheckClient)
        (connectHybridServer
            (contramap OgmiosServer tr) webSocketApp httpApp)

--
-- Environment
--

-- | Environment of the application, carrying around what's needed for the
-- application to run.
data Env (m :: * -> *) = Env
    { health :: !(TVar (Health Block))
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
    health <- atomically $ newTVar emptyHealth
    sensors <- newSensors
    sampler <- newSampler (contramap OgmiosMetrics tr)
    pure $ Env{health,sensors,sampler,network,options}

--
-- Logging
--

data TraceOgmios where
    OgmiosHealth
        :: { healthCheck :: TraceHealth (Health Block) }
        -> TraceOgmios

    OgmiosMetrics
        :: { metrics :: TraceMetrics }
        -> TraceOgmios

    OgmiosWebSocket
        :: { webSocket :: TraceWebSocket }
        -> TraceOgmios

    OgmiosServer
        :: { server :: TraceServer }
        -> TraceOgmios

    OgmiosNetwork
        :: NetworkParameters
        -> TraceOgmios

deriving instance Show TraceOgmios

instance HasSeverityAnnotation TraceOgmios where
    getSeverityAnnotation = \case
        OgmiosHealth msg     -> getSeverityAnnotation msg
        OgmiosMetrics msg    -> getSeverityAnnotation msg
        OgmiosWebSocket msg  -> getSeverityAnnotation msg
        OgmiosServer msg     -> getSeverityAnnotation msg
        OgmiosNetwork{}      -> Info
