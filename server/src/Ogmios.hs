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
    , version

    -- * Environment
    , Env (..)
    , newEnvironment

    -- * Command & Options
    , Command (..)
    , parseOptions

    -- * Logging
    , Tracers (..)
    , mkTracers
    , withStdoutTracer
    ) where

import Ogmios.Prelude

import Cardano.Network.Protocol.NodeToClient
    ( Block )
import Control.Monad.Class.MonadST
    ( MonadST )
import Ogmios.App.Configuration
    ( Configuration (..), NetworkParameters (..), TraceConfiguration (..) )
import Ogmios.App.Health
    ( Health, connectHealthCheckClient, emptyHealth, newHealthCheckClient )
import Ogmios.App.Metrics
    ( RuntimeStats, Sampler, Sensors, newSampler, newSensors )
import Ogmios.App.Server
    ( connectHybridServer )
import Ogmios.App.Server.Http
    ( mkHttpApp )
import Ogmios.App.Server.WebSocket
    ( newWebSocketApp )
import Ogmios.Control.Exception
    ( MonadCatch, MonadMask, MonadThrow )
import Ogmios.Control.MonadAsync
    ( MonadAsync (..), MonadFork, MonadThread )
import Ogmios.Control.MonadClock
    ( MonadClock, getCurrentTime, withDebouncer, _10s )
import Ogmios.Control.MonadLog
    ( MonadLog (..), mkTracers, withStdoutTracer )
import Ogmios.Control.MonadMetrics
    ( MonadMetrics )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..), TVar, newTVar )
import Ogmios.Control.MonadWebSocket
    ( MonadWebSocket )
import Ogmios.Options
    ( Command (..), Tracers (..), parseOptions )
import Ogmios.Version
    ( version )
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

-- | Main application monad.
newtype App a = App
    { unApp :: ReaderT (Env App) IO a
    } deriving newtype
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
application :: Tracers IO Identity -> App ()
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
data Env (m :: Type -> Type) = Env
    { health :: !(TVar m (Health Block))
    , sensors :: !(Sensors m)
    , sampler :: !(Sampler RuntimeStats m)
    , network :: !NetworkParameters
    , configuration :: !Configuration
    } deriving stock (Generic)

newEnvironment
    :: Tracers IO Identity
    -> NetworkParameters
    -> Configuration
    -> IO (Env App)
newEnvironment Tracers{tracerMetrics} network configuration = do
    health  <- getCurrentTime >>= atomically . newTVar . emptyHealth
    sensors <- newSensors
    sampler <- newSampler tracerMetrics
    pure $ Env{health,sensors,sampler,network,configuration}
