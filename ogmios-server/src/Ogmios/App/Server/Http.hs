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

import Relude

import Data.Aeson
    ( ToJSON (..) )
import Data.FileEmbed
    ( embedFile )
import Wai.Routes
    ( Handler
    , RenderRoute (..)
    , Routable (..)
    , header
    , html
    , javascript
    , json
    , mkRoute
    , parseRoutes
    , route
    , runHandlerM
    , sub
    , waiApp
    )

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
/                      HomeR                GET
/health                HealthR              GET
/tests.html            TestsR               GET
/tests/chain-sync.js   TestsChainSyncR      GET
/tests/state-query.js  TestsStateQueryR     GET
|]

getHomeR :: Handler Server
getHomeR = runHandlerM $ do
    -- TODO: Show live 'health' data instead of a static landing page.
    html $ decodeUtf8 $(embedFile "static/index.html")

getHealthR :: Handler Server
getHealthR = runHandlerM $ do
    header "Access-Control-Allow-Origin" "*"
    Server tvar <- sub
    health <- liftIO $ readTVarIO tvar
    json health

getTestsR :: Handler Server
getTestsR = runHandlerM $ do
    html $ decodeUtf8 $(embedFile "static/tests.html")

getTestsChainSyncR :: Handler Server
getTestsChainSyncR = runHandlerM $ do
    javascript $ decodeUtf8 $(embedFile "static/tests/chain-sync.js")

getTestsStateQueryR :: Handler Server
getTestsStateQueryR = runHandlerM $ do
    javascript $ decodeUtf8 $(embedFile "static/tests/state-query.js")

-- | Wai 'Application' representing the HTTP server.
mkHttpApp
    :: ToJSON health
    => TVar health
    -> Wai.Application
mkHttpApp =
    waiApp . route . Server
