--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- NOTE: Needed because of auto-generated template-haskell code for wai-routes.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Ogmios.App.Server.Http
    ( mkHttpApp
    , healthCheck
    ) where

import Ogmios.Prelude

import Ogmios.App.Metrics
    ( RuntimeStats (..)
    , Sampler
    , Sensors
    )
import Ogmios.Control.MonadClock
    ( MonadClock
    )
import Ogmios.Control.MonadMetrics
    ( MonadMetrics
    )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..)
    , TVar
    )
import Ogmios.Data.Health
    ( ConnectionStatus (..)
    , Health (..)
    , Tip (..)
    , modifyHealth
    )
import Ogmios.Data.Metrics.Prometheus
    ( mkPrometheusMetrics
    )

import qualified Ogmios.App.Metrics as Metrics

import Data.Aeson
    ( ToJSON (..)
    )
import Data.FileEmbed
    ( embedFile
    )
import Network.HTTP.Client
    ( defaultManagerSettings
    , httpLbs
    , newManager
    , parseRequest
    , responseBody
    )
import Network.HTTP.Types
    ( status200
    , status400
    , status500
    )
import Ogmios.App.Configuration
    ( Configuration (..)
    )
import Relude.Extra
    ( lookup
    )
import System.Exit
    ( ExitCode (..)
    )
import Wai.Routes
    ( Handler
    , RenderRoute (..)
    , Routable (..)
    , header
    , html
    , javascript
    , mkRoute
    , parseRoutes
    , raw
    , rawBuilder
    , request
    , route
    , runHandlerM
    , showRoute
    , status
    , sub
    , waiApp
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.KeyMap as Json.KeyMap
import qualified Data.Binary.Builder as Builder
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WS

data EnvServer block m = EnvServer
    { health        :: TVar m (Health block)
    , sensors       :: Sensors m
    , sampler       :: Sampler RuntimeStats m
    , configuration :: !Configuration
    }

data Server where
    Server
        :: (MonadClock m, MonadMetrics m, MonadSTM m, ToJSON (Tip block))
        => (forall a. m a -> IO a)
        -> EnvServer block m
        -> Server

-- The HTTP 'Server' serves multiple purposes:
--
-- - A landing page
-- - A health check to hook into monitoring systems / operations
-- - A quick'n'dirty "benchmark" script used for rapid smoke testing.
--
mkRoute "Server" [parseRoutes|
/                           RootR         GET POST
/dashboard.js               DashboardJsR  GET
/health                     HealthR       GET
/metrics                    MetricsR      GET
/assets/ogmios__light.png   LogoR         GET
/favicon.ico                FaviconR      GET
|]

-- Dashboard
getRootR :: Handler Server
getRootR = runHandlerM $ do
    html $ decodeUtf8 $(embedFile "static/dashboard.html")

-- Proxy simple request/response events from HTTP into the WebSocket.
postRootR :: Handler Server
postRootR = runHandlerM $ do
    header "Access-Control-Allow-Origin" "*"
    Server _unliftIO EnvServer{configuration} <- sub
    req <- Wai.lazyRequestBody <$> request
    let Configuration{serverHost, serverPort} = configuration
    res <- liftIO $ WS.runClient serverHost serverPort "/" $ \ws -> do
        WS.sendTextData ws =<< req
        WS.receiveData ws
    case Json.decode res of
        Just (Json.Object obj) -> do
            header "Content-Type" "application/json; charset=utf-8"
            case Json.KeyMap.lookup "error" obj of
                Just{} -> do
                    status status400
                _ ->
                    status status200
        _otherwise -> do
            status status500
    rawBuilder $ Builder.fromLazyByteString res

getDashboardJsR :: Handler Server
getDashboardJsR = runHandlerM $ do
    javascript $ decodeUtf8 $(embedFile "static/dashboard.js")


getHealthR :: Handler Server
getHealthR = runHandlerM $ do
    header "Access-Control-Allow-Origin" "*"
    Server unliftIO EnvServer{health,sensors,sampler} <- sub
    json =<< liftIO (unliftIO (do
        metrics <- Metrics.sample sampler sensors
        modifyHealth health (\h -> h { metrics })))
  where
    -- NOTE: Not using Wai.Routes.json because it forces 'toJSON' instead of
    -- defaulting to 'toEncoding'. So, quickly redefining it here, relying on
    -- 'toEncoding'.
    json a = do
      header "Content-Type" "application/json; charset=utf-8"
      rawBuilder $ Json.fromEncoding $ toEncoding a

getMetricsR :: Handler Server
getMetricsR = runHandlerM $ do
    header "Access-Control-Allow-Origin" "*"
    header "Content-Type" "text/plain; charset=utf-8"
    Server unliftIO EnvServer{health,sensors,sampler} <- sub
    (rawBuilder . mkPrometheusMetrics) =<< liftIO (unliftIO (do
        metrics <- Metrics.sample sampler sensors
        modifyHealth health (\h -> h { metrics })))

getLogoR :: Handler Server
getLogoR = runHandlerM $ do
    header "Content-Type" "image/png"
    raw $(embedFile "static/assets/ogmios__light.png")

getFaviconR :: Handler Server
getFaviconR = runHandlerM $ do
    header "Content-Type" "image/x-icon"
    raw $(embedFile "static/assets/favicon.ico")

--
-- HealthCheck
--

-- | Performs a health check against a running server, this is a standalone
-- program which exits immediately, either with a success or an error code.
healthCheck :: Int -> IO ()
healthCheck port = do
    response <- join $ httpLbs
        <$> parseRequest (toString $ "http://localhost:" <> show port <> showRoute HealthR)
        <*> newManager defaultManagerSettings
    case Json.decode (responseBody response) >>= getConnectionStatus of
        Just st | st == toJSON Connected ->
            return ()
        _ ->
            exitWith (ExitFailure 1)
  where
    getConnectionStatus = lookup @(Map String Json.Value) "connectionStatus"

-- | Wai 'Application' representing the HTTP server.
mkHttpApp
    :: forall m env block.
        ( MonadClock m
        , MonadMetrics m
        , MonadSTM m
        , MonadReader env m
        , HasType (TVar m (Health block)) env
        , HasType (Sensors m) env
        , HasType (Sampler RuntimeStats m) env
        , HasType Configuration env
        , ToJSON (Tip block)
        )
    => (forall a. m a -> IO a)
    -> m Wai.Application
mkHttpApp unliftIO = do
    env <- EnvServer @block @m
        <$> asks (view typed)
        <*> asks (view typed)
        <*> asks (view typed)
        <*> asks (view typed)
    pure $ waiApp $ route (Server unliftIO env)
