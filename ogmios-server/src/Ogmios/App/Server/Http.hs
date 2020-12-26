--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- NOTE: Needed because of auto-generated template-haskell code for wai-routes.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Ogmios.App.Server.Http
    ( mkHttpApp
    ) where

import Prelude

import Control.Concurrent.STM
    ( TVar, readTVarIO )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Aeson
    ( ToJSON (..) )
import Data.FileEmbed
    ( embedFile )
import Wai.Routes
    ( Handler
    , RenderRoute (..)
    , Routable (..)
    , html
    , json
    , mkRoute
    , parseRoutes
    , route
    , runHandlerM
    , sub
    , waiApp
    )

import qualified Data.Text.Encoding as T
import qualified Network.Wai as Wai

data Server where
    Server :: ToJSON health => TVar health -> Server

-- The HTTP 'Server' serves multiple purposes:
--
-- - A landing page
-- - A health check to hook into monitoring systems / operations
-- - A quick'n'dirty "benchmark" script used for rapid smoke testing.
--
mkRoute "Server" [parseRoutes|
/                HomeR          GET
/health          HealthR        GET
/benchmark.html  BenchmarkR     GET
|]

getHomeR :: Handler Server
getHomeR = runHandlerM $ do
    -- TODO: Show live 'health' data instead of a static landing page.
    html $ T.decodeUtf8 $(embedFile "static/index.html")

getHealthR :: Handler Server
getHealthR = runHandlerM $ do
    Server tvar <- sub
    health <- liftIO $ readTVarIO tvar
    json health

getBenchmarkR :: Handler Server
getBenchmarkR = runHandlerM $ do
    html $ T.decodeUtf8 $(embedFile "static/benchmark.html")

-- | Wai 'Application' representing the HTTP server.
mkHttpApp
    :: ToJSON health
    => TVar health
    -> Wai.Application
mkHttpApp =
    waiApp . route . Server
