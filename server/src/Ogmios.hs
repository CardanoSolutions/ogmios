--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DerivingVia #-}

-- NOTE: necessary for the 'Generic' instance on the Env data-family.
-- This data-family is exported in the same module, and that's the top
-- of the stack so that is deemed *acceptable*.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios
    ( -- * App
      App (..)
    , application
    , runWith
    , version
    , healthCheck
    , module Ogmios.App.Inspect

    -- * Environment
    , Env (..)
    , newEnvironment

    -- * Command & Options
    , Command (..)
    , InspectCommand (..)
    , parseOptions

    -- * Logging
    , Tracers (..)
    , withStdoutTracers
    ) where

import Ogmios.Prelude

import Ogmios.App.Inspect

import Cardano.Network.Protocol.NodeToClient
    ( Block
    )
import Ogmios.App.Configuration
    ( Configuration (..)
    , NetworkParameters (..)
    , TraceConfiguration (..)
    )
import Ogmios.App.Health
    ( Health
    , connectHealthCheckClient
    , emptyHealth
    , newHealthCheckClient
    )
import Ogmios.App.Metrics
    ( RuntimeStats
    , Sampler
    , Sensors
    , newSampler
    , newSensors
    )
import Ogmios.App.Server
    ( connectHybridServer
    )
import Ogmios.App.Server.Http
    ( healthCheck
    , mkHttpApp
    )
import Ogmios.App.Server.WebSocket
    ( newWebSocketApp
    )
import Ogmios.Control
    ( App (..)
    , Env
    )
import Ogmios.Control.MonadAsync
    ( MonadAsync (..)
    )
import Ogmios.Control.MonadClock
    ( _10s
    , getCurrentTime
    , withDebouncer
    )
import Ogmios.Control.MonadLog
    ( MonadLog (..)
    , TracerDefinition (..)
    , withStdoutTracers
    )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..)
    )
import Ogmios.Options
    ( Command (..)
    , InspectCommand (..)
    , Tracers (..)
    , parseOptions
    )
import Ogmios.Version
    ( version
    )
import System.Posix.Signals
    ( Handler (..)
    , installHandler
    , keyboardSignal
    , raiseSignal
    , softwareTermination
    )

--
-- App
--

-- | Application runner with an instantiated environment. See 'newEnvironment'.
runWith :: forall a. App a -> Env App -> IO a
runWith app = runReaderT (unApp app)

-- | Ogmios, where everything gets stitched together.
application :: Tracers IO 'Concrete -> App ()
application tracers = hijackSigTerm >> withDebouncer _10s (\debouncer -> do
    env@Env{network} <- ask
    logWith tracerConfiguration (ConfigurationNetwork network)

    healthCheckClient <- newHealthCheckClient tracerHealth debouncer

    webSocketApp <- newWebSocketApp tracerWebSocket (`runWith` env)
    httpApp      <- mkHttpApp @_ @_ @Block (`runWith` env)

    concurrently_
        (connectHealthCheckClient tracerHealth (`runWith` env) healthCheckClient)
        (connectHybridServer tracerServer webSocketApp httpApp)
    )
  where
    Tracers { tracerHealth, tracerWebSocket, tracerServer, tracerConfiguration } = tracers

-- | The runtime does not let the application terminate gracefully when a
-- SIGTERM is received. It does however for SIGINT which allows the application
-- to cleanup sub-processes.
--
-- This function install handlers for SIGTERM and turn them into SIGINT.
hijackSigTerm :: App ()
hijackSigTerm =
    liftIO $ void (installHandler softwareTermination handler empty)
  where
    handler = CatchOnce (raiseSignal keyboardSignal)

--
-- Environment
--

-- | Environment of the application, carrying around what's needed for the
-- application to run.
data instance Env App = Env
    { health :: !(TVar App (Health Block))
    , sensors :: !(Sensors App)
    , sampler :: !(Sampler RuntimeStats App)
    , network :: !NetworkParameters
    , configuration :: !Configuration
    } deriving stock (Generic)

newEnvironment
    :: Tracers IO 'Concrete
    -> NetworkParameters
    -> Configuration
    -> IO (Env App)
newEnvironment Tracers{tracerMetrics} network configuration = do
    health  <- getCurrentTime >>= newTVarIO . emptyHealth
    sensors <- newSensors
    sampler <- newSampler tracerMetrics
    pure $ Env{health,sensors,sampler,network,configuration}
