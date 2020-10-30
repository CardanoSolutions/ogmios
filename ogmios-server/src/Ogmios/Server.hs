--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Ogmios.Server
    ( runServer
    , printVersion
    ) where

import Prelude

import Ogmios.Json
    ()

import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Cardano.Network.Protocol.NodeToClient
    ( Block, Client, NodeVersionData, connectClient, mkClient )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( ExceptionInLinkedThread (..), async, link )
import Control.Concurrent.MVar
    ( MVar, newMVar, readMVar )
import Control.Exception
    ( IOException, SomeException, catch, fromException, handle, throwIO )
import Control.Monad
    ( forever, unless )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Tracer
    ( Tracer, contramap, nullTracer, traceWith )
import Data.FileEmbed
    ( embedFile )
import Data.Function
    ( (&) )
import Data.Git.Revision.TH
    ( gitRevParseHEAD, gitTags )
import Data.List
    ( isInfixOf )
import Data.String
    ( IsString (..) )
import Network.HTTP.Types.Header
    ( hUserAgent )
import Network.WebSockets
    ( ConnectionException (..) )
import Ogmios.Health
    ( Health )
import Ogmios.Health.Trace
    ( TraceHealth (..) )
import Ogmios.Metrics
    ( Sensors, recordSession )
import Ogmios.Options
    ( Options (..) )
import Ogmios.Trace
    ( TraceOgmios (..) )
import System.Directory
    ( doesPathExist )
import System.IO.Error
    ( isDoesNotExistError )
import System.Time.Clock
    ( NominalDiffTime, nominalDiffTimeToMicroseconds )
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

import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as Wai
import qualified Network.WebSockets as WS
import qualified Ogmios.Bridge as Bridge
import qualified Ogmios.Health as Health
import qualified Ogmios.Metrics as Metrics
import qualified Ouroboros.Network.NodeToClient.Version as O


--  _    _      _     _____            _        _
-- | |  | |    | |   /  ___|          | |      | |
-- | |  | | ___| |__ \ `--.  ___   ___| | _____| |_
-- | |/\| |/ _ \ '_ \ `--. \/ _ \ / __| |/ / _ \ __|
-- \  /\  /  __/ |_) /\__/ / (_) | (__|   <  __/ |_
--  \/  \/ \___|_.__/\____/ \___/ \___|_|\_\___|\__|

-- | The actual WebSocket server, creating a new connection to cardano-node
-- for each WebSocket client connected.
webSocketApp
    :: Tracer IO TraceOgmios
    -> Sensors
    -> (NodeVersionData, EpochSlots)
    -> Options
    -> WS.ServerApp
webSocketApp tr sensors (versionData, epochSlots) Options{nodeSocket} pending = do
    traceWith tr (OgmiosConnectionAccepted userAgent)
    conn <- WS.acceptRequest pending
    recordSession sensors $ WS.withPingThread conn 30 (pure ()) $ handlers $ do
        let trClient = contramap OgmiosClient tr
        client <- mkClient trClient epochSlots <$> Bridge.newClients conn
        connectClient trClient client versionData nodeSocket
            `catch` Bridge.handleIOException tr conn
        traceWith tr $ OgmiosConnectionEnded userAgent
  where
    userAgent = maybe "User-Agent unknown" snd
        $ L.find ((== hUserAgent) . fst)
        $ WS.requestHeaders
        $ WS.pendingRequest pending

    handlers
        = handle onUnknownException
        . handle onConnectionClosed
        . handle onLinkedException

    onConnectionClosed = \case
        CloseRequest{} -> traceWith tr $ OgmiosConnectionEnded userAgent
        ConnectionClosed{} -> traceWith tr $ OgmiosConnectionEnded userAgent
        e -> throwIO e

    onLinkedException = \case
        ExceptionInLinkedThread _ e -> case fromException e of
            Just e' -> onConnectionClosed e'
            Nothing -> throwIO e

    onUnknownException = traceWith tr . OgmiosUnknownException

--  _   _ _____ ___________
-- | | | |_   _|_   _| ___ \
-- | |_| | | |   | | | |_/ /
-- |  _  | | |   | | |  __/
-- | | | | | |   | | | |
-- \_| |_/ \_/   \_/ \_|

newtype Server = Server (MVar (Health Block))

mkRoute "Server" [parseRoutes|
/                HomeR          GET
/benchmark.html  BenchmarkR     GET
/ogmios.wsp.json SpecificationR GET
/health          HealthR        GET
|]

getHomeR :: Handler Server
getHomeR = runHandlerM $ do
    html $ T.decodeUtf8 $(embedFile "static/index.html")

getBenchmarkR :: Handler Server
getBenchmarkR = runHandlerM $ do
    html $ T.decodeUtf8 $(embedFile "static/benchmark.html")

getSpecificationR :: Handler Server
getSpecificationR = runHandlerM $ do
    asContent "application/json" $ T.decodeUtf8 $(embedFile "../ogmios.wsp.json")

getHealthR :: Handler Server
getHealthR = runHandlerM $ do
    Server mvar <- sub
    health  <- liftIO (readMVar mvar)
    json health

-- | Create a new Wai application which serves some static files and, run a
-- health-check client.
newHttpApp
    :: Tracer IO TraceOgmios
    -> (NodeVersionData, EpochSlots)
    -> FilePath
    -> IO (Wai.Application, Metrics.Sensors)
newHttpApp tr (vData, epochSlots) socket = do
    (sensors, sampler) <- Metrics.init (contramap OgmiosMetrics tr)
    mvar <- newMVar Health.init
    client <- mkClient (contramap OgmiosClient tr) epochSlots
        <$> Health.newClients (contramap OgmiosHealth tr) mvar sampler
    link =<< async (connectHealth (contramap OgmiosHealth tr) vData socket client)
    pure (waiApp $ route $ Server mvar, sensors)

connectHealth
    :: Tracer IO (TraceHealth (Health Block))
    -> NodeVersionData
    -> FilePath
    -> Client IO
    -> IO ()
connectHealth tr vData socket client = forever $ handlers $ do
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

--  _____                 _
-- |  _  |               (_)
-- | | | | __ _ _ __ ___  _  ___  ___
-- | | | |/ _` | '_ ` _ \| |/ _ \/ __|
-- \ \_/ / (_| | | | | | | | (_) \__ \
--  \___/ \__, |_| |_| |_|_|\___/|___/
--         __/ |
--        |___/

-- | Ogmios is made of two part (although the first part really is the core of
-- it):
--
-- (1) A WebSocket server which dedicated communication with an underlying
--     Cardano node and runs all three Ouroboros mini-protocols.
--
-- (2) An HTTP server which mostly handle monitoring operations.
--
-- This functions is blocking and starts a Warp server which will route requests
-- to either of the two Wai applications described above.
runServer
    :: (NodeVersionData, EpochSlots)
    -> Options
    -> Tracer IO TraceOgmios
    -> IO ()
runServer env@(vData, epochSlots) opts@Options{host,port,nodeSocket} tr = do
    traceWith tr $ OgmiosNetwork (O.networkMagic $ fst vData) epochSlots
    (httpApp, sensors) <- newHttpApp tr env nodeSocket
    Warp.runSettings settings $ Wai.websocketsOr WS.defaultConnectionOptions
        (webSocketApp tr sensors env opts)
        httpApp
  where
    settings = Warp.defaultSettings
        & Warp.setHost (fromString host)
        & Warp.setPort port
        & Warp.setBeforeMainLoop beforeMainLoop

    beforeMainLoop :: IO ()
    beforeMainLoop = do
        socketExist <- doesPathExist nodeSocket
        unless socketExist $ traceWith tr $ OgmiosSocketNotFound nodeSocket
        traceWith tr OgmiosStarted{host,port}

-- | Simply print the current version and revision
printVersion
    :: IO ()
printVersion = do
    B8.putStrLn $ T.encodeUtf8 $ T.pack revision
  where
    tags = $(gitTags)
    revHEAD = $(gitRevParseHEAD)
    lastKnownTag = fst $ head tags
    revision = case L.find ((== revHEAD) . snd) tags of
        Just (tag, _) ->
            tag
        Nothing -> unwords
            [ "unreleased (> " <> lastKnownTag <> ")"
            , "-"
            , "git revision " <> take 8 revHEAD
            ]
