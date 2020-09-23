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
    , mkHealthCheckClient

    -- * Wai Application
    , application
    ) where

import Prelude

import Cardano.Byron.Constants
    ( NodeVersionData )
import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Cardano.Network.Protocol.NodeToClient
    ( Block, Client, codecs, connectClient, localChainSync, nullProtocol )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, link )
import Control.Concurrent.MVar
    ( MVar, modifyMVar_, newMVar, readMVar )
import Control.Exception
    ( SomeException, handle )
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
import Data.Time.Clock
    ( UTCTime, getCurrentTime )
import GHC.Generics
    ( Generic )
import Network.TypedProtocol.Pipelined
    ( N (..) )
import Ogmios.Health.Trace
    ( TraceHealth (..) )
import Ogmios.Trace
    ( TraceOgmios (..) )
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

import Cardano.Types.Json.Orphans
    ()

data Health block = Health
    { nodeTip :: Tip block
        -- ^ Current tip of the core node.
    , lastUpdate :: Maybe UTCTime
        -- ^ Date at which the last update was received.
    } deriving (Generic, Eq, Show)

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
            SendMsgFindIntersect [getTipPoint tip] $ stIntersect $ \_tip ->
                stIdle

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
    :: Tracer IO TraceOgmios
    -> (NodeVersionData, EpochSlots, SecurityParam)
    -> FilePath
    -> IO Wai.Application
application tr (vData, epochSlots, _securityParam) socket = do
    mvar <- newMVar $ Health TipGenesis Nothing
    link =<< async (monitor $ mkClient mvar)
    pure $ waiApp $ route $ Server mvar
  where
    mkClient
        :: MVar (Health Block)
        -> Client IO
    mkClient mvar = do
        let codec = cChainSyncCodec $ codecs epochSlots
        nodeToClientProtocols (const $ pure $ NodeToClientProtocols
            { localChainSyncProtocol = InitiatorProtocolOnly
                $ MuxPeerRaw
                $ localChainSync nullTracer codec
                $ mkHealthCheckClient
                $ \tip -> modifyMVar_ mvar $ \_ -> do
                    s <- Health tip . Just <$> getCurrentTime
                    s <$ traceWith tr (OgmiosHealth $ HealthTick s)

            , localTxSubmissionProtocol = nullProtocol
            , localStateQueryProtocol = nullProtocol
            })
            NodeToClientV_2

    monitor :: Client IO -> IO ()
    monitor client = forever $ handle onUnknownException $
        connectClient nullTracer client vData socket

    onUnknownException :: SomeException -> IO ()
    onUnknownException e = do
        traceWith tr $ OgmiosUnknownException e
        let fiveSeconds = 5_000_000
        threadDelay fiveSeconds

getHomeR :: Handler Server
getHomeR = runHandlerM $ do
    html $ T.decodeUtf8 $(embedFile "static/index.html")

getHealthR :: Handler Server
getHealthR = runHandlerM $ do
    Server mvar <- sub
    liftIO (readMVar mvar) >>= json

getBenchmarkR :: Handler Server
getBenchmarkR = runHandlerM $ do
    html $ T.decodeUtf8 $(embedFile "static/benchmark.html")

getSpecificationR :: Handler Server
getSpecificationR = runHandlerM $ do
    asContent "application/json" $ T.decodeUtf8 $(embedFile "ogmios.wsp.json")
