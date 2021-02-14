--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.App.Health
    ( -- * Health
      Health (..)
    , emptyHealth
    , healthCheck

      -- * HealthCheckClient
    , HealthCheckClient
    , newHealthCheckClient
    , connectHealthCheckClient

    -- * Logging
    , TraceHealth (..)
    ) where

import Relude hiding
    ( STM, TVar, atomically, writeTVar )

import Ogmios.App.Metrics
    ( RuntimeStats, Sampler, Sensors )
import Ogmios.App.Options
    ( NetworkParameters (..), Options (..) )
import Ogmios.App.Protocol.ChainSync
    ( mkHealthCheckClient )
import Ogmios.Control.Exception
    ( Exception (..)
    , IOException
    , MonadCatch (..)
    , MonadThrow (..)
    , SomeException
    , isAsyncException
    , isDoesNotExistError
    , isResourceVanishedError
    , isTryAgainError
    )
import Ogmios.Control.MonadAsync
    ( MonadAsync )
import Ogmios.Control.MonadClock
    ( Debouncer (..), MonadClock (..), idle, _5s )
import Ogmios.Control.MonadLog
    ( HasSeverityAnnotation (..)
    , Logger
    , MonadLog (..)
    , Severity (..)
    , contramap
    , natTracer
    , nullTracer
    )
import Ogmios.Control.MonadMetrics
    ( MonadMetrics )
import Ogmios.Control.MonadOuroboros
    ( MonadOuroboros )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..), TVar, writeTVar )
import Ogmios.Data.Health
    ( Health (..), emptyHealth )

import qualified Ogmios.App.Metrics as Metrics

import Cardano.Network.Protocol.NodeToClient
    ( Block
    , Clients (..)
    , SubmitTxError
    , SubmitTxPayload
    , connectClient
    , mkClient
    )
import Cardano.Network.Protocol.NodeToClient.Trace
    ( TraceClient )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.Time.Clock
    ( DiffTime )
import Ouroboros.Network.Block
    ( Tip (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (NodeToClientVersionData) )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( LocalStateQueryClient (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxSubmissionClient (..) )

-- | A simple wrapper around Ouroboros 'Clients'. A health check client only
-- carries a chain-sync client.
newtype HealthCheckClient m
    = HealthCheckClient (Clients m Block)

-- | Instantiate a new set of Ouroboros mini-protocols clients. Note that only
-- the chain-sync client does something here. Others are just idling.
newHealthCheckClient
    :: forall m env.
        ( MonadAsync m
        , MonadClock m
        , MonadLog m
        , MonadMetrics m
        , MonadReader env m
        , HasType (TVar m (Health Block)) env
        , HasType (Sensors m) env
        , HasType (Sampler RuntimeStats m) env
        )
    => Logger (TraceHealth (Health Block))
    -> Debouncer m
    -> m (HealthCheckClient m)
newHealthCheckClient tr Debouncer{debounce} = do
    pure $ HealthCheckClient $ Clients
        { chainSyncClient = mkHealthCheckClient $ \lastKnownTip -> debounce $ do
            tvar <- asks (view typed)
            sensors <- asks (view typed)
            sampler <- asks (view typed)
            health <- healthCheck (pure lastKnownTip) tvar sensors sampler
            logWith tr (HealthTick health)

        , txSubmissionClient =
            LocalTxSubmissionClient idle

        , stateQueryClient =
            LocalStateQueryClient idle
        }

-- | Construct a health check by sampling all application sensors.
--
-- See also 'Ogmios.App.Protocol.ChainSync#mkHealthCheckClient'
healthCheck
    :: forall m block.
        ( MonadClock m
        , MonadMetrics m
        , MonadSTM m
        )
    => STM m (Tip block)
    -> TVar m (Health block)
    -> Sensors m
    -> Sampler RuntimeStats m
    -> m (Health block)
healthCheck readTip tvar sensors sampler = do
    lastTipUpdate <- Just <$> getCurrentTime
    metrics <- Metrics.sample sampler sensors
    atomically $ do
        lastKnownTip <- readTip
        let health = Health{ lastKnownTip, lastTipUpdate, metrics }
        health <$ writeTVar tvar health

connectHealthCheckClient
    :: forall m env.
        ( MonadIO m -- Needed by 'connectClient'
        , MonadCatch m
        , MonadLog m
        , MonadOuroboros m
        , MonadReader env m
        , HasType NetworkParameters env
        , HasType Options env
        )
    => Logger (TraceHealth (Health Block))
    -> (forall a. m a -> IO a)
    -> HealthCheckClient m
    -> m ()
connectHealthCheckClient tr embed (HealthCheckClient clients) = do
    NetworkParameters{slotsPerEpoch,networkMagic} <- asks (view typed)
    Options{nodeSocket} <- asks (view typed)
    let trClient = natTracer liftIO $ contramap HealthClient tr
    let client = mkClient embed trClient slotsPerEpoch clients
    connectClient nullTracer client (NodeToClientVersionData networkMagic) nodeSocket
        & onExceptions nodeSocket
        & forever
  where
    onExceptions nodeSocket
        = handle onUnknownException
        . handle (onIOException nodeSocket)

    onUnknownException :: SomeException -> m ()
    onUnknownException e
        | isAsyncException e = do
            logWith tr $ HealthShutdown e
            throwIO e

        | otherwise = do
            logWith tr $ HealthUnknownException e
            throwIO e

    onIOException :: FilePath -> IOException -> m ()
    onIOException nodeSocket e
        | isRetryable = do
            logWith tr $ HealthFailedToConnect nodeSocket _5s
            liftIO $ threadDelay _5s
        | otherwise = do
            logWith tr $ HealthUnknownException (toException e)
            throwIO e
      where
        isRetryable :: Bool
        isRetryable = isResourceVanishedError e || isDoesNotExistError e || isTryAgainError e

--
-- Logging
--

data TraceHealth s where
    HealthClient
        :: TraceClient (SubmitTxPayload Block) (SubmitTxError Block)
        -> TraceHealth s

    HealthTick
        :: { status :: s }
        -> TraceHealth s

    HealthFailedToConnect
        :: { socket :: FilePath, retryingIn :: DiffTime }
        -> TraceHealth s

    HealthShutdown
        :: { action :: SomeException }
        -> TraceHealth s

    HealthUnknownException
        :: { exception :: SomeException }
        -> TraceHealth s

deriving instance Show s => Show (TraceHealth s)

instance HasSeverityAnnotation (TraceHealth s) where
    getSeverityAnnotation = \case
        HealthClient msg -> getSeverityAnnotation msg
        HealthTick{} -> Info
        HealthFailedToConnect{} -> Warning
        HealthShutdown{} -> Notice
        HealthUnknownException{} -> Error
