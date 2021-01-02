--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Control.MonadWebSocket
  ( -- * Class
    MonadWebSocket (..)

    -- * Helpers
  , WebSocketApp
  , Connection
  , PendingConnection
  , ConnectionException (..)
  , Headers
  , headers
  ) where

import Relude

import Network.WebSockets
    ( Connection, ConnectionException (..), Headers, PendingConnection )

import qualified Network.WebSockets as WS

type WebSocketApp = PendingConnection -> IO ()

class Monad m => MonadWebSocket (m :: * -> *) where
    receive
        :: Connection -> m ByteString
    send
        :: Connection -> ByteString -> m ()
    close
        :: Connection -> ByteString -> m ()
    acceptRequest
        :: PendingConnection
        -> (Connection -> m ())
        -> m ()

instance MonadWebSocket m => MonadWebSocket (ReaderT env m) where
    receive =
        lift . receive
    send conn =
        lift . send conn
    close conn =
        lift . close conn
    acceptRequest pending action = do
        env <- ask
        lift $ acceptRequest pending (\conn -> runReaderT (action conn) env)

instance MonadWebSocket IO where
    receive =
        WS.receiveData
    send =
        WS.sendTextData
    close =
        WS.sendClose
    acceptRequest pending action = do
        conn <- WS.acceptRequest pending
        WS.withPingThread conn 30 afterEachPing (action conn)
      where
        afterEachPing :: IO ()
        afterEachPing = return ()

headers :: PendingConnection -> Headers
headers = WS.requestHeaders . WS.pendingRequest
