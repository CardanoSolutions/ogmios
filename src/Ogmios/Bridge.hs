--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Ogmios.Bridge
    (
    -- * Clients
      pipeClients

    -- * JSON-WSP Description
    , serviceDescription

    -- * Internals
    , FindIntersect (..)
    , FindIntersectResponse (..)
    , RequestNext (..)
    , RequestNextResponse (..)
    , SubmitTx (..)
    , SubmitTxResponse (..)

    -- * Re-Exports
    , ByronBlock
    , GenTx
    , Point
    , Tip
    ) where

import Prelude

import Control.Concurrent.Async
    ( async, link )
import Control.Concurrent.STM
    ( atomically )
import Control.Concurrent.STM.TQueue
    ( newTQueueIO, readTQueue, tryReadTQueue, writeTQueue )
import Control.Monad
    ( forever, guard )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.ByteString
    ( ByteString )
import Data.FileEmbed
    ( embedFile )
import Data.Functor
    ( ($>) )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import GHC.Generics
    ( Generic )
import Network.TypedProtocol.Pipelined
    ( Nat (..), natToInt )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock, GenTx )
import Ouroboros.Network.Block
    ( Point (..), Tip (..) )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..)
    , ClientPipelinedStIdle (..)
    , ClientPipelinedStIntersect (..)
    , ClientStNext (..)
    )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxClientStIdle (..), LocalTxSubmissionClient (..) )

import qualified Codec.Json.Wsp as Wsp
import qualified Codec.Json.Wsp.Handler as Wsp
import qualified Data.Aeson as Json
import qualified Data.Binary.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as Wai
import qualified Network.WebSockets as WS
import qualified WaiAppStatic.Types as Wai

data SimplePipe input output (m :: * -> *) = SimplePipe
    { await :: m input
        -- ^ Await for the next input. Block until an input is available.

    , tryAwait :: m (Maybe input)
        -- ^ Return 'Just input' if there's an input available, nothing
        -- otherwise. Non blocking.

    , yield :: output -> m ()
        -- ^ Yield a result.

    , pass  :: input -> m ()
        -- ^ Pass the input onto another component
    }

data Queue a (m :: * -> *) = Queue
    { pop :: m a
        -- ^ Unstash a value. Block until one is available
        --
    , tryPop :: m (Maybe a)
        -- ^ Unstash a value, if any.

    , push :: a -> m ()
        -- ^ Stash a value for later.
    }

-- | Create a 'ChainSyncClient' and a 'LocalTxSubmissionClient' from a single
-- WebSocket 'Connection'.
pipeClients
    :: forall block tx err.
        ( FromJSON (Point block)
        , ToJSON block
        , ToJSON (Point block)
        , ToJSON (Tip block)
        , FromJSON tx
        , ToJSON err
        )
    => WS.Connection
    -> IO ( ChainSyncClientPipelined block (Tip block) IO ()
          , LocalTxSubmissionClient tx err IO ()
          )
pipeClients conn = do
    rcvd <- newTQueueIO
    pipe <- newTQueueIO

    let receive = WS.receiveData conn >>= atomically . writeTQueue rcvd
    link =<< async (forever receive)

    resp <- newTQueueIO
    let queue = Queue
            { pop = atomically $ readTQueue resp
            , tryPop = atomically $ tryReadTQueue resp
            , push  = atomically . writeTQueue resp
            }

    let chainSyncClient = mkChainSyncClient queue $ SimplePipe
            { await = atomically $ readTQueue rcvd
            , tryAwait = atomically $ tryReadTQueue rcvd
            , yield = WS.sendTextData conn
            , pass  = atomically . writeTQueue pipe
            }

    let localTxSubmissionClient = mkLocalTxSubmissionClient $ SimplePipe
            { await = atomically $ readTQueue pipe
            , tryAwait = atomically $ tryReadTQueue pipe
            , yield = WS.sendTextData conn
            , pass  = const (defaultHandler conn)
            }

    pure (chainSyncClient, localTxSubmissionClient)

--  _____ _           _         _____
-- /  __ \ |         (_)       /  ___|
-- | /  \/ |__   __ _ _ _ __   \ `--. _   _ _ __   ___
-- | |   | '_ \ / _` | | '_ \   `--. \ | | | '_ \ / __|
-- | \__/\ | | | (_| | | | | | /\__/ / |_| | | | | (__
--  \____/_| |_|\__,_|_|_| |_| \____/ \__, |_| |_|\___|
--                                     __/ |
--                                    |___/

newtype FindIntersect block
    = FindIntersect { points :: [Point block] }
    deriving (Generic, Show)

data FindIntersectResponse block
    = IntersectionFound { point :: Point block, tip :: Tip block }
    | IntersectionNotFound { tip :: Tip block }
    deriving (Generic, Show)

data RequestNext
    = RequestNext
    deriving (Generic, Show)

data RequestNextResponse block
    = RollForward { block :: block, tip :: Tip block }
    | RollBackward { point :: Point block, tip :: Tip block }
    deriving (Generic, Show)

mkChainSyncClient
    :: forall m block.
        ( FromJSON (Point block)
        , ToJSON block
        , ToJSON (Point block)
        , ToJSON (Tip block)
        , Monad m
        )
    => Queue (RequestNextResponse block -> Wsp.Response (RequestNextResponse block)) m
    -> SimplePipe ByteString ByteString m
    -> ChainSyncClientPipelined block (Tip block) m ()
mkChainSyncClient Queue{push,pop,tryPop} SimplePipe{await,tryAwait,yield,pass} =
    ChainSyncClientPipelined (clientStIdle Zero)
  where
    clientStIdle
        :: forall n. Nat n
        -> m (ClientPipelinedStIdle n block (Tip block) m ())
    clientStIdle Zero = await >>= Wsp.handle
        (\bytes -> pass bytes *> clientStIdle Zero)
        [ Wsp.Handler $ \FindIntersect{points} ->
            pure . SendMsgFindIntersect points . clientStIntersect

        , Wsp.Handler $ \RequestNext toResponse -> do
            let collect = CollectResponse
                    (Just $ clientStIdle (Succ Zero))
                    (clientStNext Zero pop)
            push toResponse $> SendMsgRequestNextPipelined collect
        ]
    clientStIdle n@(Succ prev) = tryAwait >>= \case
        -- If there's no immediate incoming message, we take this opportunity to
        -- wait and collect one response.
        Nothing -> tryPop >>= \case
            Nothing ->
                clientStIdle n
            Just toResponse ->
                pure $ CollectResponse Nothing (clientStNext prev $ pure toResponse)

        -- Yet, if we have already received a new message from the client, we
        -- prioritize it and pipeline it right away unless there are already too
        -- many requests in flights.
        Just msg -> Wsp.handle
            (\bytes -> pass bytes *> clientStIdle n)
            [ Wsp.Handler $ \RequestNext toResponse -> do
                let collect = CollectResponse
                        (guard (natToInt n < 1000) $> clientStIdle (Succ n))
                        (clientStNext n pop)
                push toResponse $> SendMsgRequestNextPipelined collect
            ] msg

    clientStIntersect
        :: (FindIntersectResponse block -> Wsp.Response (FindIntersectResponse block))
        -> ClientPipelinedStIntersect block (Tip block) m ()
    clientStIntersect toResponse = ClientPipelinedStIntersect
        { recvMsgIntersectFound = \point tip -> do
            yield $ json $ toResponse $ IntersectionFound point tip
            clientStIdle Zero
        , recvMsgIntersectNotFound = \tip -> do
            yield $ json $ toResponse $ IntersectionNotFound tip
            clientStIdle Zero
        }

    clientStNext
        :: Nat n
        -> m (RequestNextResponse block -> Wsp.Response (RequestNextResponse block))
        -> ClientStNext n block (Tip block) m ()
    clientStNext n pop' = ClientStNext
        { recvMsgRollForward = \block tip -> do
            toResponse <- pop'
            yield $ json $ toResponse $ RollForward block tip
            clientStIdle n
        , recvMsgRollBackward = \point tip -> do
            toResponse <- pop'
            yield $ json $ toResponse $ RollBackward point tip
            clientStIdle n
        }

--  _____      _____       _               _         _
-- |_   _|    /  ___|     | |             (_)       (_)
--   | |_  __ \ `--. _   _| |__  _ __ ___  _ ___ ___ _  ___  _ __
--   | \ \/ /  `--. \ | | | '_ \| '_ ` _ \| / __/ __| |/ _ \| '_ \
--   | |>  <  /\__/ / |_| | |_) | | | | | | \__ \__ \ | (_) | | | |
--   \_/_/\_\ \____/ \__,_|_.__/|_| |_| |_|_|___/___/_|\___/|_| |_|

newtype SubmitTx tx
    = SubmitTx { bytes :: tx }
    deriving (Generic, Show)

newtype SubmitTxResponse err
    = SubmitTxResponse { error :: Maybe err }
    deriving (Generic, Show)

mkLocalTxSubmissionClient
    :: forall m tx err.
        ( FromJSON tx
        , ToJSON err
        , Monad m
        )
    => SimplePipe ByteString ByteString m
    -> LocalTxSubmissionClient tx err m ()
mkLocalTxSubmissionClient SimplePipe{await,yield,pass} =
     LocalTxSubmissionClient clientStIdle
  where
    clientStIdle
        :: m (LocalTxClientStIdle tx err m ())
    clientStIdle = await >>= Wsp.handle
        (\bytes -> pass bytes *> clientStIdle)
        [ Wsp.Handler $ \SubmitTx{bytes} toResponse ->
            pure $ SendMsgSubmitTx bytes $ \e -> do
                yield $ json $ toResponse $ SubmitTxResponse e
                clientStIdle
        ]

-- ___  ____              _ _
-- |  \/  (_)            | | |
-- | .  . |_ ___  ___ ___| | | __ _ _ __   ___  ___  _   _ ___
-- | |\/| | / __|/ __/ _ \ | |/ _` | '_ \ / _ \/ _ \| | | / __|
-- | |  | | \__ \ (_|  __/ | | (_| | | | |  __/ (_) | |_| \__ \
-- \_|  |_/_|___/\___\___|_|_|\__,_|_| |_|\___|\___/ \__,_|___/

-- | A simple static web-server for serving the WSP description.
serviceDescription
    :: Maybe String
    -> Wai.Application
serviceDescription publicUrl =
    Wai.staticApp $ (Wai.embeddedSettings
        [ ("/ogmios.wsp.json", replaceUrl (fromMaybe "N/A" publicUrl) spec)
        , ("/benchmark", bench)
        ])
        { Wai.ssGetMimeType = \file -> pure $
            case Wai.fromPiece (Wai.fileName file) of
                x | x == "ogmios.wsp.json" ->
                    "application/json"
                x | x == "benchmark" ->
                    "text/html"
                x | x == "" ->
                    "text/html"
                _anythingElse ->
                    "application/octet-stream"

        , Wai.ssListing = Just $ \_pieces _folder -> do
            pure (Builder.fromByteString index)
        }
  where
    index :: ByteString
    index = $(embedFile "static/index.html")

    bench :: ByteString
    bench = $(embedFile "static/benchmark.html")

    spec :: ByteString
    spec = $(embedFile "ogmios.wsp.json")

    replaceUrl url file =
        T.encodeUtf8 $ T.replace "{{url}}" (T.pack url) $ T.decodeUtf8 file

-- | Helper function to yield a value that is serialisable to JSON.
json :: ToJSON a => a -> ByteString
json = BL.toStrict . Json.encode

-- | A default handler for unmatched requests.
defaultHandler
    :: WS.Connection
    -> IO ()
defaultHandler conn = WS.sendTextData conn $ json $ Wsp.clientFault
    "Invalid request: no route found for the given request. Verify the request's \
    \name and/or parameters."

--
-- ToJSON / FromJSON instances. Nothing to see here.
--

type instance Wsp.ServiceName (Wsp.Response _) = "ogmios"

instance
    ( FromJSON (Point block)
    ) => FromJSON (Wsp.Request (FindIntersect block))
  where
    parseJSON = Wsp.genericFromJSON Wsp.defaultOptions

instance FromJSON (Wsp.Request RequestNext)
  where
    parseJSON = Wsp.genericFromJSON Wsp.defaultOptions

instance
    ( ToJSON (Point block)
    , ToJSON (Tip block)
    ) => ToJSON (Wsp.Response (FindIntersectResponse block))
  where
    toJSON = Wsp.genericToJSON Wsp.defaultOptions proxy
      where proxy = Proxy @(Wsp.Request (FindIntersect block))

instance
    ( ToJSON block
    , ToJSON (Tip block)
    , ToJSON (Point block)
    ) => ToJSON (Wsp.Response (RequestNextResponse block))
  where
    toJSON = Wsp.genericToJSON Wsp.defaultOptions proxy
      where proxy = Proxy @(Wsp.Request RequestNext)

instance
    ( FromJSON tx
    ) => FromJSON (Wsp.Request (SubmitTx tx))
  where
    parseJSON = Wsp.genericFromJSON Wsp.defaultOptions

instance
    ( ToJSON err
    ) => ToJSON (Wsp.Response (SubmitTxResponse err))
  where
    toJSON = Wsp.genericToJSON Wsp.defaultOptions proxy
      where proxy = Proxy @(Wsp.Request (SubmitTx _))
