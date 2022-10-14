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
    , healthCheck

    -- * Environment
    , Env (..)
    , newEnvironment

    -- * Command & Options
    , Command (..)
    , parseOptions

    -- * Logging
    , Tracers (..)
    , withStdoutTracers
    ) where

import Ogmios.Prelude

import Cardano.Network.Protocol.NodeToClient
    ( Block
    )
import Control.Monad.Class.MonadST
    ( MonadST
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
import Ogmios.Control.Exception
    ( MonadCatch
    , MonadMask
    , MonadThrow
    )
import Ogmios.Control.MonadAsync
    ( MonadAsync (..)
    , MonadFork
    , MonadThread
    )
import Ogmios.Control.MonadClock
    ( MonadClock
    , _10s
    , getCurrentTime
    , withDebouncer
    )
import Ogmios.Control.MonadLog
    ( MonadLog (..)
    , TracerDefinition (..)
    , withStdoutTracers
    )
import Ogmios.Control.MonadMetrics
    ( MonadMetrics
    )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..)
    )
import Ogmios.Control.MonadWebSocket
    ( MonadWebSocket
    )
import Ogmios.Options
    ( Command (..)
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

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM.TBQueue as STM
import qualified Control.Concurrent.STM.TMVar as STM
import qualified Control.Concurrent.STM.TQueue as STM
import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM

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
        , MonadST
        , MonadThread, MonadFork
        , MonadThrow, MonadCatch, MonadMask
        )

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
data Env (m :: Type -> Type) = Env
    { health :: !(TVar m (Health Block))
    , sensors :: !(Sensors m)
    , sampler :: !(Sampler RuntimeStats m)
    , network :: !NetworkParameters
    , configuration :: !Configuration
    } deriving stock (Generic)

newEnvironment
    :: Tracers IO 'Concrete
    -> NetworkParameters
    -> Configuration
    -> IO (Env App)
newEnvironment Tracers{tracerMetrics} network configuration = do
    health  <- getCurrentTime >>= atomically . newTVar . emptyHealth
    sensors <- newSensors
    sampler <- newSampler tracerMetrics
    pure $ Env{health,sensors,sampler,network,configuration}

--
-- Instances
--

-- NOTE:
--
-- Since `6b83e2b385b3533ccfc336c1099d27f1c6a79a2c` in ouroboros-network,
-- functional dependencies have been introduced between the class type family
-- and the monad argument making that monad injective with the type-family.
--
-- This means it's no longer possible to define instances of MonadSTM which use
-- `STM` as a target monad directly, without conflicting with the existing
-- instance definition for `IO`... this disqualifies `ReaderT` and all kind of
-- transformers, and also, our `App` application monad!

newtype WrappedSTM a = WrappedSTM { unwrapSTM :: STM.STM a }
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus, MonadThrow)

instance MonadSTM App where
  type STM     App = WrappedSTM
  type TVar    App = STM.TVar
  type TMVar   App = STM.TMVar
  type TQueue  App = STM.TQueue
  type TBQueue App = STM.TBQueue

  atomically      = App . lift . STM.atomically . unwrapSTM
  retry           = WrappedSTM STM.retry
  orElse          = \a0 a1 -> WrappedSTM (STM.orElse (unwrapSTM a0) (unwrapSTM a1))
  check           = WrappedSTM . STM.check

  newTVar         = WrappedSTM . STM.newTVar
  newTVarIO       = App . lift . STM.newTVarIO
  readTVar        = WrappedSTM . STM.readTVar
  readTVarIO      = App . lift . STM.readTVarIO
  writeTVar       = \a0 -> WrappedSTM . STM.writeTVar a0
  modifyTVar      = \a0 -> WrappedSTM . STM.modifyTVar a0
  modifyTVar'     = \a0 -> WrappedSTM . STM.modifyTVar' a0
  stateTVar       = \a0 -> WrappedSTM . STM.stateTVar a0
  swapTVar        = \a0 -> WrappedSTM . STM.swapTVar a0

  newTMVar        = WrappedSTM . STM.newTMVar
  newTMVarIO      = App . lift . STM.newTMVarIO
  newEmptyTMVar   = WrappedSTM STM.newEmptyTMVar
  newEmptyTMVarIO = App (lift STM.newEmptyTMVarIO)
  takeTMVar       = WrappedSTM . STM.takeTMVar
  tryTakeTMVar    = WrappedSTM . STM.tryTakeTMVar
  putTMVar        = \a0 -> WrappedSTM . STM.putTMVar a0
  tryPutTMVar     = \a0 -> WrappedSTM . STM.tryPutTMVar a0
  readTMVar       = WrappedSTM . STM.readTMVar
  tryReadTMVar    = WrappedSTM . STM.tryReadTMVar
  swapTMVar       = \a0 -> WrappedSTM . STM.swapTMVar a0
  isEmptyTMVar    = WrappedSTM . STM.isEmptyTMVar

  newTQueue       = WrappedSTM STM.newTQueue
  newTQueueIO     = App (lift STM.newTQueueIO)
  readTQueue      = WrappedSTM . STM.readTQueue
  tryReadTQueue   = WrappedSTM . STM.tryReadTQueue
  peekTQueue      = WrappedSTM . STM.peekTQueue
  tryPeekTQueue   = WrappedSTM . STM.tryPeekTQueue
  flushTBQueue    = WrappedSTM . STM.flushTBQueue
  writeTQueue     = \a0 -> WrappedSTM . STM.writeTQueue a0
  isEmptyTQueue   = WrappedSTM . STM.isEmptyTQueue

  newTBQueue      = WrappedSTM . STM.newTBQueue
  newTBQueueIO    = App . lift . STM.newTBQueueIO
  readTBQueue     = WrappedSTM . STM.readTBQueue
  tryReadTBQueue  = WrappedSTM . STM.tryReadTBQueue
  peekTBQueue     = WrappedSTM . STM.peekTBQueue
  tryPeekTBQueue  = WrappedSTM . STM.tryPeekTBQueue
  writeTBQueue    = \a0 -> WrappedSTM . STM.writeTBQueue a0
  lengthTBQueue   = WrappedSTM . STM.lengthTBQueue
  isEmptyTBQueue  = WrappedSTM . STM.isEmptyTBQueue
  isFullTBQueue   = WrappedSTM . STM.isFullTBQueue

newtype WrappedAsync a = WrappedAsync { unwrapAsync :: Async.Async a }
    deriving newtype (Functor)

instance MonadAsync App where
  type Async App  = WrappedAsync
  async           = \(App (ReaderT m)) -> App (ReaderT $ \r -> WrappedAsync <$> async (m r))
  asyncThreadId   = Async.asyncThreadId . unwrapAsync
  pollSTM         = WrappedSTM . Async.pollSTM . unwrapAsync
  waitCatchSTM    = WrappedSTM . Async.waitCatchSTM . unwrapAsync
  cancel          = App . lift . Async.cancel . unwrapAsync
  cancelWith      = \a0 -> App . lift . Async.cancelWith (unwrapAsync a0)
  asyncWithUnmask = \restore -> App $ ReaderT $ \r ->
      fmap WrappedAsync $ Async.asyncWithUnmask $ \unmask ->
        runReaderT (unApp (restore (liftF unmask))) r
    where
      liftF :: (IO a -> IO a) -> App a -> App a
      liftF g (App (ReaderT f)) = App (ReaderT (g . f))
