--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- NOTE: Needed because of auto-generated template-haskell code for wai-routes.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Ogmios.App.Server.Http
    ( mkHttpApp
    , healthCheck
    ) where

import Ogmios.Prelude

import Network.HTTP.Types.Header
    ( Header
    , hContentType
    )
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
    , NetworkSynchronization (..)
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
import Data.Binary.Builder
    ( Builder
    , toLazyByteString
    )
import Data.FileEmbed
    ( embedFile
    )
import Network.HTTP.Client
    ( defaultManagerSettings
    , httpLbs
    , newManager
    , parseRequest
    )
import Network.HTTP.Types
    ( HeaderName
    , status200
    , status202
    , status400
    , status404
    , status406
    , status500
    )
import Network.HTTP.Types.Status
    ( Status
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

import qualified Data.Aeson as Json
import qualified Data.Aeson.KeyMap as Json.KeyMap
import qualified Data.Binary.Builder as Builder
import qualified Network.HTTP.Client as Http
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WS

data EnvServer block m = EnvServer
    { health        :: TVar m (Health block)
    , sensors       :: Sensors m
    , sampler       :: Sampler RuntimeStats m
    , configuration :: !Configuration
    }

data Handler where
    Handler
        :: (forall m block. (MonadIO m, MonadClock m, MonadMetrics m, MonadSTM m, ToJSON (Tip block)) => EnvServer block m -> StateT HandlerState m ())
        -> Handler

-- Dashboard
getRootR :: Handler
getRootR = Handler $ \_env -> do
    header "Access-Control-Allow-Origin" "*"
    status status200
    html $ decodeUtf8 $(embedFile "static/dashboard.html")

-- Proxy simple request/response events from HTTP into the WebSocket.
postRootR :: Handler
postRootR = Handler $ \EnvServer{configuration} -> do
    header "Access-Control-Allow-Origin" "*"
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
    raw $ Builder.fromLazyByteString res

getHealthR :: Handler
getHealthR = Handler $ \EnvServer{health, sensors, sampler} -> do
    header "Access-Control-Allow-Origin" "*"
    response@Health{connectionStatus, networkSynchronization} <- do
        metrics <- lift $ Metrics.sample sampler sensors
        modifyHealth health (\h -> h { metrics })
    status $ case (connectionStatus, networkSynchronization) of
        (Disconnected, _) -> status500
        (Connected, Just (NetworkSynchronization 1)) -> status200
        (Connected, _) -> status202
    json response

getMetricsR :: Handler
getMetricsR = Handler $ \EnvServer{health, sensors, sampler} -> do
    header "Access-Control-Allow-Origin" "*"
    header "Content-Type" "text/plain; charset=utf-8"
    status status200
    (raw . mkPrometheusMetrics) =<< do
        metrics <- lift $ Metrics.sample sampler sensors
        modifyHealth health (\h -> h { metrics })

getFaviconR :: Handler
getFaviconR = Handler $ \_env -> do
    header "Access-Control-Allow-Origin" "*"
    header "Content-Type" "image/png"
    status status200
    raw $ Builder.fromByteString $(embedFile "static/favicon.png")

--
-- HealthCheck
--

-- | Performs a health check against a running server, this is a standalone
-- program which exits immediately, either with a success or an error code.
healthCheck :: Int -> IO ()
healthCheck port = do
    response <- join $ httpLbs
        <$> parseRequest (toString $ "http://localhost:" <> show @Text port <> "/health")
        <*> newManager defaultManagerSettings
    case Json.decode (Http.responseBody response) >>= getConnectionStatus of
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
        , MonadIO m
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
    return $ \req send -> do
        let (Handler mkRes) = route (Wai.requestMethod req, Wai.pathInfo req)
        st <- unliftIO $ execStateT (mkRes env) (emptyHandlerState req)
        send $ Wai.responseLBS (_status st) (_headers st) (_responseBody st)
  where
    route = \case
        ("OPTIONS", _) ->
            Handler $ \_env -> do
                header "Access-Control-Allow-Origin" "*"
                status status200

        -- /
        ("GET", []) ->
            getRootR
        ("POST", []) ->
            postRootR
        (_, []) ->
            notAllowed

        -- /health
        ("GET", ["health"]) ->
            getHealthR
        (_, ["health"]) ->
            notAllowed

        -- /metrics
        ("GET", ["metrics"]) ->
            getMetricsR
        (_, ["metrics"]) ->
            notAllowed

        -- /favicon.png
        ("GET", ["favicon.png"]) ->
            getFaviconR
        (_, ["favicon.png"]) ->
            notAllowed

        (_, _) ->
            notFound

--
-- Responses
--

data HandlerState = HandlerState
    { _status :: Status
    , _headers :: [Header]
    , _responseBody :: LByteString
    , _request :: Wai.Request
    }

emptyHandlerState :: Wai.Request -> HandlerState
emptyHandlerState _request = HandlerState
    { _status = status500
    , _headers = []
    , _responseBody = mempty
    , _request
    }

status :: Monad m => Status -> StateT HandlerState m ()
status s =
    modify $ \st -> st
        { _status = s
        }

header :: Monad m => HeaderName -> ByteString -> StateT HandlerState m ()
header headerName headerValue =
    modify $ \st -> st
        { _headers = (headerName, headerValue) : _headers st
        }

request :: Monad m => StateT HandlerState m Wai.Request
request =
    gets _request

html :: Monad m => Text -> StateT HandlerState m ()
html doc =
    modify $ \st -> st
        { _headers = textHtml : _headers st
        , _responseBody = encodeUtf8 doc
        }
  where
    textHtml = (hContentType, "text/html; charset=utf-8")

json :: (Monad m, ToJSON a) => a -> StateT HandlerState m ()
json a =
    modify $ \st -> st
        { _headers = applicationJson : _headers st
        , _responseBody = toLazyByteString $ Json.fromEncoding $ toEncoding a
        }
  where
    applicationJson = (hContentType, "application/json; charset=utf-8")

raw :: (Monad m) => Builder -> StateT HandlerState m ()
raw builder =
    modify $ \st -> st
        { _responseBody = toLazyByteString builder
        }

notFound :: Handler
notFound = Handler $ \_env -> do
    status status404

notAllowed :: Handler
notAllowed = Handler $ \_env -> do
    status status406
