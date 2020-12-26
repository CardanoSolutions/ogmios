--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.App.Server
    ( connectHybridServer

      -- * Logging
    , TraceServer (..)
    ) where

import Prelude

import Ogmios.App.Options
    ( Options (..) )
import Ogmios.Control.MonadLog
    ( HasSeverityAnnotation (..), Logger, MonadLog (..), Severity (..) )
import Ogmios.Control.MonadWebSocket
    ( WebSocketApp )

import Control.Monad
    ( unless )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Reader
    ( MonadReader, asks )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.String
    ( IsString (..) )
import System.Directory
    ( doesPathExist )

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as Wai
import qualified Network.WebSockets as WS

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
connectHybridServer
    :: forall m env.
        ( MonadIO m
        , MonadReader env m
        , HasType Options env
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
    serverSettings opts@Options{serverHost, serverPort} = Warp.defaultSettings
        & Warp.setHost (fromString serverHost)
        & Warp.setPort serverPort
        & Warp.setBeforeMainLoop (beforeMainLoop opts)

    beforeMainLoop :: Options -> IO ()
    beforeMainLoop opts@Options{nodeSocket} = do
        socketExist <- doesPathExist nodeSocket
        unless socketExist $ logWith tr $ ServerNodeSocketNotFound nodeSocket
        logWith tr $ ServerStarted opts

--
-- Logging
--

data TraceServer where
    ServerStarted
        :: Options
        -> TraceServer

    ServerNodeSocketNotFound
        :: { path :: FilePath }
        -> TraceServer

deriving instance Show TraceServer

instance HasSeverityAnnotation TraceServer where
    getSeverityAnnotation = \case
        ServerNodeSocketNotFound{} -> Warning
        ServerStarted{}        -> Info
