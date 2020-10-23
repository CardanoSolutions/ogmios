--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Prelude

import Cardano.BM.Trace.Extra
    ( withStdoutTracer )
import Cardano.Byron.Constants
    ( EpochSlots, SecurityParam, lookupVersionData )
import Cardano.Network.Protocol.NodeToClient
    ( NodeVersionData, connectClient, mkClient )
import Control.Concurrent.Async
    ( ExceptionInLinkedThread (..) )
import Control.Exception
    ( catch, fromException, handle, throwIO )
import Control.Monad
    ( unless )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.Function
    ( (&) )
import Data.String
    ( IsString (..) )
import Network.HTTP.Types.Header
    ( hUserAgent )
import Network.WebSockets
    ( ConnectionException (..) )
import Ogmios.Bridge
    ( handleIOException, newClients )
import Ogmios.Health
    ( ApplicationMetrics, recordSession )
import Ogmios.Options.Applicative
    ( Options (..), run )
import Ogmios.Trace
    ( TraceOgmios (..) )
import System.Directory
    ( doesPathExist )

import Cardano.Types.Json.Orphans
    ()

import qualified Data.List as L
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as Wai
import qualified Network.WebSockets as WS
import qualified Ogmios.Health as Health

main :: IO ()
main = do
    run $ \opts@Options{logLevel} -> do
        withStdoutTracer "ogmios" logLevel (T.pack . show) $ runServer opts
  where
    runServer opts@Options{host,port,nodeSocket} tr = do
        env <- lookupVersionData (contramap OgmiosLookupEnv tr) "OGMIOS_NETWORK"
        (healthCheck, metrics) <- Health.application (contramap OgmiosHealth tr) env nodeSocket
        Warp.runSettings settings $ Wai.websocketsOr WS.defaultConnectionOptions
            (websocketApp tr metrics env opts)
            healthCheck
      where
        settings = Warp.defaultSettings
            & Warp.setHost (fromString host)
            & Warp.setPort port
            & Warp.setBeforeMainLoop (do
                socketExist <- doesPathExist nodeSocket
                unless socketExist $ traceWith tr $ OgmiosSocketNotFound nodeSocket
                traceWith tr OgmiosStarted{host,port}
            )

-- | The actual WebSocket server, creating a new connection to cardano-node
-- for each WebSocket client connected.
websocketApp
    :: Tracer IO TraceOgmios
    -> ApplicationMetrics
    -> (NodeVersionData, EpochSlots, SecurityParam)
    -> Options
    -> WS.ServerApp
websocketApp tr metrics (versionData, epochSlots, _) Options{nodeSocket} pending = do
    traceWith tr (OgmiosConnectionAccepted userAgent)
    conn <- WS.acceptRequest pending
    recordSession metrics $ WS.withPingThread conn 30 (pure ()) $ handlers $ do
        let trClient = contramap OgmiosClient tr
        client <- mkClient trClient epochSlots <$> newClients conn
        connectClient trClient client versionData nodeSocket
            `catch` handleIOException tr conn
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
