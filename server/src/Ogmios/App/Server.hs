--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.App.Server
    ( connectHybridServer

      -- * Logging
    , TraceServer (..)
    ) where

import Ogmios.Prelude

import Ogmios.App.Configuration
    ( Configuration (..)
    )
import Ogmios.Control.MonadLog
    ( HasSeverityAnnotation (..)
    , Logger
    , MonadLog (..)
    , Severity (..)
    )
import Ogmios.Control.MonadWebSocket
    ( WebSocketApp
    )
import Ogmios.Data.Json
    ( ToJSON
    )

import System.Directory
    ( doesPathExist
    )

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as Wai
import qualified Network.WebSockets as WS

-- | Ogmios is made of two parts:
--
-- (1) A WebSocket server with a dedicated communication channel to an underlying
--     Cardano node. It runs all three Ouroboros mini-protocols and translate
--     them to JSON.
--
-- (2) An HTTP server which mostly handle monitoring operations and serve some
--     static files.
--
-- This function is blocking and starts a Warp server which will route requests
-- to either of the two Wai applications described above.
connectHybridServer
    :: forall m env.
        ( MonadIO m
        , MonadReader env m
        , HasType Configuration env
        )
    => Logger TraceServer
    -> WebSocketApp
    -> Wai.Application
    -> m ()
connectHybridServer tr webSocketApp httpApp = do
    opts <- asks (view typed)
    liftIO
        $ Warp.runSettings (serverSettings opts)
        $ Wai.websocketsOr WS.defaultConnectionOptions webSocketApp httpApp
  where
    serverSettings opts@Configuration{serverHost, serverPort, connectionTimeout} =
        Warp.defaultSettings
        & Warp.setHost (fromString serverHost)
        & Warp.setPort serverPort
        & Warp.setBeforeMainLoop (beforeMainLoop opts)
        & Warp.setTimeout connectionTimeout

    beforeMainLoop :: Configuration -> IO ()
    beforeMainLoop Configuration{nodeSocket,serverHost,serverPort} = do
        socketExist <- doesPathExist nodeSocket
        unless socketExist $ logWith tr $ ServerNodeSocketNotFound nodeSocket
        logWith tr $ ServerStarted{dashboardUrl, nodeSocket}
      where
        dashboardUrl = "http://" ++ serverHost ++ ":" ++ show serverPort ++ "/"

--
-- Logging
--

data TraceServer where
    ServerStarted
        :: { dashboardUrl :: String, nodeSocket :: FilePath }
        -> TraceServer

    ServerNodeSocketNotFound
        :: { path :: FilePath }
        -> TraceServer
    deriving stock (Generic, Show)
    deriving anyclass ToJSON

instance HasSeverityAnnotation TraceServer where
    getSeverityAnnotation = \case
        ServerNodeSocketNotFound{} -> Warning
        ServerStarted{}        -> Info
