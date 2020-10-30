--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Ogmios.Health
    (
    -- * Heath Check
      Health (..)
    , mkHealthCheckClient

    -- * Wai Application
    , application
    ) where

import Prelude

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
    ( MVar, modifyMVar_, newMVar, readMVar )
import Control.Exception
    ( IOException, SomeException, handle, throwIO )
import Control.Monad
    ( forever )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Tracer
    ( Tracer, contramap, nullTracer, traceWith )
import Data.Aeson
    ( ToJSON (..), genericToJSON )
import Data.FileEmbed
    ( embedFile )
import Data.List
    ( isInfixOf )
import Data.Time.Clock
    ( UTCTime, getCurrentTime )
import GHC.Generics
    ( Generic )
import Network.TypedProtocol.Pipelined
    ( N (..) )
import Ogmios.Health.Trace
    ( TraceHealth (..) )
import Ogmios.Metrics
    ( Metrics )
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
import System.Time.Clock
    ( Debouncer (..)
    , NominalDiffTime
    , newDebouncer
    , nominalDiffTimeToMicroseconds
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
import qualified Data.Text.Encoding as T
import qualified Network.Wai as Wai
import qualified Ogmios.Metrics as Metrics

import Cardano.Types.Json.Orphans
    ()

-- | Capture some health heartbeat of the application. This is populated by two
-- things:
--
-- - A metric store which measure runtime statistics.
-- - An Ourobors local chain-sync client which follows the chain's tip.
data Health block = Health
    { lastKnownTip :: Tip block
    -- ^ Last known tip of the core node.
    , lastTipUpdate :: Maybe UTCTime
    -- ^ Date at which the last update was received.
    , metrics :: Metrics
    -- ^ Application metrics measured at regular interval
    } deriving (Generic, Eq, Show)

initHealth :: Health block
initHealth = Health
    { lastKnownTip = TipGenesis
    , lastTipUpdate = Nothing
    , metrics = Metrics.empty
    }

instance ToJSON (Tip block) => ToJSON (Health block) where
    toJSON = genericToJSON Json.defaultOptions

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

newtype Server = Server (MVar (Health Block))

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
    -> IO (Wai.Application, Metrics.Sensors)
application tr (vData, epochSlots, _securityParam) socket = do
    (sensors, sampler)  <- Metrics.init (contramap HealthMetrics tr)
    mvar <- newMVar initHealth
    debouncer <- newDebouncer 20
    link =<< async (monitor $ mkClient debouncer mvar sampler)
    pure (waiApp $ route $ Server mvar, sensors)
  where
    mkClient
        :: Debouncer
        -> MVar (Health Block)
        -> Metrics.Sampler IO
        -> Client IO
    mkClient Debouncer{debounce} mvar sample = do
        let codec = cChainSyncCodec $ codecs epochSlots
        nodeToClientProtocols (const $ pure $ NodeToClientProtocols
            { localChainSyncProtocol = InitiatorProtocolOnly
                $ MuxPeerRaw
                $ localChainSync nullTracer codec
                $ mkHealthCheckClient
                $ \lastKnownTip -> debounce $ modifyMVar_ mvar $ \_ -> do
                    health <- Health lastKnownTip
                        <$> (Just <$> getCurrentTime)
                        <*> sample
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
    Server mvar <- sub
    health  <- liftIO (readMVar mvar)
    json health

getBenchmarkR :: Handler Server
getBenchmarkR = runHandlerM $ do
    html $ T.decodeUtf8 $(embedFile "static/benchmark.html")

getSpecificationR :: Handler Server
getSpecificationR = runHandlerM $ do
    asContent "application/json" $ T.decodeUtf8 $(embedFile "../ogmios.wsp.json")
