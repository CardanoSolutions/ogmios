--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Ogmios.Health
    (
    -- * Heath Check
      Health (..)
    , RuntimeStats (..)
    , ApplicationMetrics
    , recordSession
    , mkHealthCheckClient

    -- * Wai Application
    , application
    ) where

import Prelude hiding
    ( max, min )

import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Cardano.Network.Protocol.NodeToClient
    ( Block
    , Client
    , NodeVersionData
    , codecs
    , connectClient
    , localChainSync
    , nullProtocol
    )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, link )
import Control.Concurrent.MVar
    ( MVar, modifyMVar_, newMVar, putMVar, readMVar, tryTakeMVar )
import Control.Exception
    ( IOException, SomeException, handle, throwIO )
import Control.Monad
    ( forever )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Tracer
    ( Tracer, nullTracer, traceWith )
import Data.Aeson
    ( ToJSON (..), genericToJSON )
import Data.FileEmbed
    ( embedFile )
import Data.Function
    ( (&) )
import Data.HashMap.Strict
    ( HashMap, (!) )
import Data.Int
    ( Int64 )
import Data.List
    ( isInfixOf )
import Data.Ratio
    ( (%) )
import Data.Scientific
    ( Scientific, fromRationalRepetendLimited )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( UTCTime, getCurrentTime )
import GHC.Generics
    ( Generic )
import GHC.Stats
    ( RTSStats (..), getRTSStats, getRTSStatsEnabled )
import Network.TypedProtocol.Pipelined
    ( N (..) )
import Ogmios.Health.Trace
    ( TraceHealth (..) )
import Ouroboros.Consensus.Config.SecurityParam
    ( SecurityParam (..) )
import Ouroboros.Consensus.Network.NodeToClient
    ( Codecs' (..) )
import Ouroboros.Network.Block
    ( Tip (..), genesisPoint, getTipPoint )
import Ouroboros.Network.Mux
    ( MuxPeer (..), RunMiniProtocol (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientProtocols (..)
    , NodeToClientVersion (..)
    , nodeToClientProtocols
    )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..)
    , ClientPipelinedStIdle (..)
    , ClientPipelinedStIntersect (..)
    , ClientStNext (..)
    )
import System.IO.Error
    ( isDoesNotExistError )
import System.Metrics.Counter
    ( Counter )
import System.Metrics.Distribution
    ( Distribution )
import System.Metrics.Gauge
    ( Gauge )
import System.Time.Clock
    ( NominalDiffTime
    , nominalDiffTimeToMicroseconds
    , nominalDiffTimeToMilliseconds
    , timed
    )
import Wai.Routes
    ( Handler
    , RenderRoute (..)
    , Routable (..)
    , asContent
    , html
    , json
    , mkRoute
    , parseRoutes
    , route
    , runHandlerM
    , sub
    , waiApp
    )

import qualified Data.Aeson as Json
import qualified Data.HashMap.Strict as Map
import qualified Data.Text.Encoding as T
import qualified Network.Wai as Wai
import qualified Prelude
import qualified System.Metrics as Ekg
import qualified System.Metrics.Counter as Ekg.Counter
import qualified System.Metrics.Distribution as Ekg.Distribution
import qualified System.Metrics.Gauge as Ekg.Gauge

import Cardano.Types.Json.Orphans
    ()

data Health block = Health
    { lastKnownTip :: Tip block
        -- ^ Last known tip of the core node.
    , lastTipUpdate :: Maybe UTCTime
        -- ^ Date at which the last update was received.
    , runtimeStats :: Maybe RuntimeStats
        -- ^ Runtime statistics of the program.
    , activeConnections :: Integer
        -- ^ Number of currently active connections
    , totalConnections :: Integer
        -- ^ Total connections since the last restart
    , sessionsDuration :: DistributionStats
        -- ^ Statistics about the duration of each session, in ms
    } deriving (Generic, Eq, Show)

initHealth :: Health block
initHealth = Health
    { lastKnownTip = TipGenesis
    , lastTipUpdate = Nothing
    , runtimeStats = Nothing
    , activeConnections = 0
    , totalConnections = 0
    , sessionsDuration = mempty
    }

instance ToJSON (Tip block) => ToJSON (Health block) where
    toJSON = genericToJSON Json.defaultOptions

-- | Some Statistics collected from the runtime execution
data RuntimeStats = RuntimeStats
    { productivity :: Scientific
        -- ^ Proportion of the time spent doing actual work (vs garbage collecting)
    , maxHeapSize :: Int64
        -- ^ Maximum live data in the heap, in KB
    } deriving (Generic, Eq, Show)

instance ToJSON RuntimeStats where
    toJSON = genericToJSON Json.defaultOptions

data DistributionStats = DistributionStats
    { mean :: Double
    , min :: Double
    , max :: Double
    } deriving (Generic, Eq, Show)

instance Semigroup DistributionStats where
    s1 <> s2 = DistributionStats
        { mean = (mean s1 + mean s2) / 2 -- FIXME: Not quite accurate
        , min = Prelude.min (min s1) (min s2)
        , max = Prelude.max (max s1) (max s2)
        }

instance Monoid DistributionStats where
    mempty = DistributionStats 0 0 0

instance ToJSON DistributionStats where
    toJSON = genericToJSON Json.defaultOptions

-- | An interface for capturing application metrics
data ApplicationMetrics = ApplicationMetrics
    { activeConnectionsGauge :: Gauge
    , totalConnectionsCounter :: Counter
    , sessionsDurationDistribution :: Distribution
    }

-- | Record some metrics about a given session.
recordSession :: ApplicationMetrics -> IO a -> IO a
recordSession metrics session = do
    Ekg.Counter.inc (totalConnectionsCounter metrics)
    Ekg.Gauge.inc (activeConnectionsGauge metrics)
    (a, duration) <- timed session
    Ekg.Distribution.add (sessionsDurationDistribution metrics) (ms duration)
    Ekg.Gauge.dec (activeConnectionsGauge metrics)
    return a
  where
    ms = fromIntegral . nominalDiffTimeToMilliseconds

--
-- Ouroboros Client
--

-- | Simple client that follows the chain by jumping directly to the tip and
-- notify a consumer for every tip change.
mkHealthCheckClient
    :: forall m block. (Monad m)
    => (Tip block -> m ())
    -> ChainSyncClientPipelined block (Tip block) m ()
mkHealthCheckClient notify =
    ChainSyncClientPipelined stInit
  where
    stInit
        :: m (ClientPipelinedStIdle Z block (Tip block) m ())
    stInit = pure $
        SendMsgFindIntersect [genesisPoint] $ stIntersect $ \tip -> pure $
            SendMsgFindIntersect [getTipPoint tip] $ stIntersect (const stIdle)

    stIntersect
        :: (Tip block -> m (ClientPipelinedStIdle Z block (Tip block) m ()))
        -> ClientPipelinedStIntersect block (Tip block) m ()
    stIntersect stFound = ClientPipelinedStIntersect
        { recvMsgIntersectNotFound = const stInit
        , recvMsgIntersectFound = const stFound
        }

    stIdle
        :: m (ClientPipelinedStIdle Z block (Tip block) m ())
    stIdle = pure $
        SendMsgRequestNext stNext (pure stNext)

    stNext
        :: ClientStNext Z block (Tip block) m ()
    stNext = ClientStNext
        { recvMsgRollForward  = const check
        , recvMsgRollBackward = const check
        }
      where
        check tip = notify tip *> stIdle

--
-- HTTP Server
--

newtype Server = Server (MVar (Health Block), Ekg.Store)

mkRoute "Server" [parseRoutes|
/                HomeR          GET
/health          HealthR        GET
/benchmark.html  BenchmarkR     GET
/ogmios.wsp.json SpecificationR GET
|]

application
    :: Tracer IO (TraceHealth (Health Block))
    -> (NodeVersionData, EpochSlots, SecurityParam)
    -> FilePath
    -> IO (Wai.Application, ApplicationMetrics)
application tr (vData, epochSlots, _securityParam) socket = do
    store <- Ekg.newStore
    appMetrics <- newApplicationMetrics store
    getRTSStatsEnabled >>= \case
        True  -> Ekg.registerGroup runtimeMetrics getRTSStats store
        False -> traceWith tr (HealthRuntimeStatsDisabled "run with '+RTS -T -RTS'")
    mvar <- newMVar initHealth
    debouncer <- newDebouncer 20
    link =<< async (monitor $ mkClient debouncer mvar store)
    pure (waiApp $ route $ Server (mvar, store), appMetrics)
  where
    mkClient
        :: Debouncer
        -> MVar (Health Block)
        -> Ekg.Store
        -> Client IO
    mkClient Debouncer{debounce} mvar store = do
        let codec = cChainSyncCodec $ codecs epochSlots
        nodeToClientProtocols (const $ pure $ NodeToClientProtocols
            { localChainSyncProtocol = InitiatorProtocolOnly
                $ MuxPeerRaw
                $ localChainSync nullTracer codec
                $ mkHealthCheckClient
                $ \lastKnownTip -> debounce $ modifyMVar_ mvar $ \_ -> do
                    lastTipUpdate <- Just <$> getCurrentTime

                    (runtimeStats, activeConnections, totalConnections, sessionsDuration)
                        <- sample store

                    let health = Health
                            { lastKnownTip
                            , lastTipUpdate
                            , runtimeStats
                            , activeConnections
                            , totalConnections
                            , sessionsDuration
                            }

                    health <$ traceWith tr (HealthTick health)

            , localTxSubmissionProtocol = nullProtocol
            , localStateQueryProtocol = nullProtocol
            })
            NodeToClientV_2

    monitor :: Client IO -> IO ()
    monitor client = forever $ handlers $ do
        connectClient nullTracer client vData socket
      where
        handlers
            = handle onUnknownException
            . handle onIOException

        _5s :: NominalDiffTime
        _5s = 5

        onUnknownException :: SomeException -> IO ()
        onUnknownException e = do
            traceWith tr $ HealthUnknownException e
            threadDelay (fromIntegral $ nominalDiffTimeToMicroseconds _5s)

        onIOException :: IOException -> IO ()
        onIOException e
            | isDoesNotExistError e || isTryAgainError e = do
                traceWith tr $ HealthFailedToConnect socket _5s
                threadDelay (fromIntegral $ nominalDiffTimeToMicroseconds _5s)

            | otherwise =
                throwIO e
          where
            isTryAgainError = isInfixOf "resource exhausted" . show

getHomeR :: Handler Server
getHomeR = runHandlerM $ do
    html $ T.decodeUtf8 $(embedFile "static/index.html")

getHealthR :: Handler Server
getHealthR = runHandlerM $ do
    Server (mvar, store) <- sub
    health <- liftIO (readMVar mvar)
    (runtimeStats, activeConnections, totalConnections, sessionsDuration)
        <- liftIO (sample store)
    json $ health
        { runtimeStats
        , activeConnections
        , totalConnections
        , sessionsDuration
        }

getBenchmarkR :: Handler Server
getBenchmarkR = runHandlerM $ do
    html $ T.decodeUtf8 $(embedFile "static/benchmark.html")

getSpecificationR :: Handler Server
getSpecificationR = runHandlerM $ do
    asContent "application/json" $ T.decodeUtf8 $(embedFile "../ogmios.wsp.json")

sample :: Ekg.Store -> IO (Maybe RuntimeStats, Integer, Integer, DistributionStats)
sample store = do
    values <- Ekg.sampleAll store
    return $ if Map.null values then (Nothing, 0, 0, mempty) else
        let
            Ekg.Gauge maxHeapSize = values ! "maxHeapSize"
            Ekg.Counter cpuTime = values ! "cpuTime"
            Ekg.Counter gcCpuTime = values ! "gcCpuTime"
            Ekg.Gauge _activeConnections = values ! "activeConnections"
            Ekg.Counter _totalConnections = values ! "totalConnections"
            Ekg.Distribution _sessionsDuration = values ! "sessionsDuration"

            productivity
                | denominator == 0 = 1
                | otherwise
                    = either fst fst
                    $ fromRationalRepetendLimited 3
                    $ toInteger cpuTime % denominator
              where
                denominator = toInteger cpuTime + toInteger gcCpuTime
        in
            ( Just $ RuntimeStats { productivity, maxHeapSize }
            , toInteger _activeConnections
            , toInteger _totalConnections
            , DistributionStats
                { mean = Ekg.Distribution.mean _sessionsDuration
                , min  = Ekg.Distribution.min _sessionsDuration
                , max  = Ekg.Distribution.max _sessionsDuration
                }
            )

runtimeMetrics :: HashMap Text (RTSStats -> Ekg.Value)
runtimeMetrics = Map.fromList
    [ ("maxHeapSize", Ekg.Gauge . fromIntegral . toKB . max_live_bytes)
    , ("cpuTime", Ekg.Counter . fromIntegral . cpu_ns)
    , ("gcCpuTime", Ekg.Counter . fromIntegral . gc_cpu_ns)
    ]
  where
    toKB = (`div` 1024)

newApplicationMetrics :: Ekg.Store -> IO ApplicationMetrics
newApplicationMetrics store = do
    activeConnectionsGauge <- Ekg.Gauge.new
    store & Ekg.registerGauge "activeConnections"
        (Ekg.Gauge.read activeConnectionsGauge)

    totalConnectionsCounter <- Ekg.Counter.new
    store & Ekg.registerCounter "totalConnections"
        (Ekg.Counter.read totalConnectionsCounter)

    sessionsDurationDistribution <- Ekg.Distribution.new
    store & Ekg.registerDistribution "sessionsDuration"
        (Ekg.Distribution.read sessionsDurationDistribution)

    pure ApplicationMetrics
        { activeConnectionsGauge
        , totalConnectionsCounter
        , sessionsDurationDistribution
        }

newtype Debouncer = Debouncer { debounce :: IO () -> IO () }

-- | Run an action, but no more than once every chosen interval of time.
newDebouncer :: NominalDiffTime -> IO Debouncer
newDebouncer delay = do
    lock <- newMVar ()
    link =<< async (forever $ threadDelay (micro delay) *> putMVar lock ())
    return $ Debouncer $ \action -> tryTakeMVar lock >>= \case
        Nothing -> return ()
        Just () -> action
  where
    micro = fromIntegral . nominalDiffTimeToMicroseconds
