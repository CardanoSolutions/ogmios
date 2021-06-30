--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- NOTE: Needed because of auto-generated template-haskell code for wai-routes.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Ogmios.App.Server.Http
    ( mkHttpApp
    ) where

import Ogmios.Prelude

import Ogmios.App.Health
    ( Health (..), modifyHealth )
import Ogmios.App.Metrics
    ( RuntimeStats, Sampler, Sensors )
import Ogmios.Control.MonadClock
    ( MonadClock )
import Ogmios.Control.MonadMetrics
    ( MonadMetrics )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..), TVar )

import qualified Ogmios.App.Metrics as Metrics

import Data.Aeson
    ( ToJSON (..) )
import Data.FileEmbed
    ( embedFile )
import Ouroboros.Network.Block
    ( Tip (..) )
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
    , route
    , runHandlerM
    , sub
    , waiApp
    )

import qualified Data.Aeson.Encoding as Json
import qualified Network.Wai as Wai

data EnvServer block m = EnvServer
    { health   :: TVar m (Health block)
    , sensors  :: Sensors m
    , sampler  :: Sampler RuntimeStats m
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
/                      DashboardR           GET
/dashboard.js          DashboardJsR         GET
/health                HealthR              GET
/tests.html            TestsR               GET
/tests/chain-sync.js   TestsChainSyncR      GET
/tests/state-query.js  TestsStateQueryR     GET
/assets/logo.png       LogoR                GET
/favicon.ico           FaviconR             GET
|]

getDashboardR :: Handler Server
getDashboardR = runHandlerM $ do
    html $ decodeUtf8 $(embedFile "static/dashboard.html")

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

getTestsR :: Handler Server
getTestsR = runHandlerM $ do
    html $ decodeUtf8 $(embedFile "static/tests.html")

getTestsChainSyncR :: Handler Server
getTestsChainSyncR = runHandlerM $ do
    javascript $ decodeUtf8 $(embedFile "static/tests/chain-sync.js")

getTestsStateQueryR :: Handler Server
getTestsStateQueryR = runHandlerM $ do
    javascript $ decodeUtf8 $(embedFile "static/tests/state-query.js")

getLogoR :: Handler Server
getLogoR = runHandlerM $ do
    header "Content-Type" "image/png"
    raw $(embedFile "static/assets/logo.png")

getFaviconR :: Handler Server
getFaviconR = runHandlerM $ do
    header "Content-Type" "image/x-icon"
    raw $(embedFile "static/assets/favicon.ico")

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
        , ToJSON (Tip block)
        )
    => (forall a. m a -> IO a)
    -> m Wai.Application
mkHttpApp unliftIO = do
    env <- EnvServer @block @m
        <$> asks (view typed)
        <*> asks (view typed)
        <*> asks (view typed)
    pure $ waiApp $ route (Server unliftIO env)
